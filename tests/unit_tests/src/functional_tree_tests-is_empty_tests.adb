--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Functional_Tree_Tests.Is_Empty_Tests is
   use Integer_3Way_Trees;

   -------------------------
   -- Test_Is_Empty_Empty --
   -------------------------

   --  This test checks that Is_Empty returns True for an empty tree

   procedure Test_Is_Empty_Empty (T : in out Test_Fixture) is
   begin
      Assert (Is_Empty (Empty_Tree),
              "Is_Empty returned False for an empty tree");
   end Test_Is_Empty_Empty;

   -----------------------------
   -- Test_Is_Empty_Not_Empty --
   -----------------------------

   --  This test checks that Is_Empty returns True for a tree that has one node

   procedure Test_Is_Empty_Not_Empty (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;
      Path      : Private_Path;

   begin
      Container := Add (Container, 1, Root);

      Path := Iter_First (Container);
      Assert (Iter_Has_Element (Container, Path), "doesnt have root");

      Assert (not Is_Empty (Container),
              "Is_Empty returned True for a non-empty tree");
   end Test_Is_Empty_Not_Empty;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Is_Empty (empty tree)",
                     Test_Is_Empty_Empty'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Is_Empty (not empty tree)",
                     Test_Is_Empty_Not_Empty'Access));
   end Add_To_Suite;

end Functional_Tree_Tests.Is_Empty_Tests;