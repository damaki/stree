--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Functional_Tree_Tests.Contains_Tests is
   use Integer_3Way_Trees;

   -------------------------
   -- Test_Contains_Empty --
   -------------------------

   --  This test checks that Contains returns False for the root of an empty
   --  tree.

   procedure Test_Contains_Empty (T : in out Test_Fixture) is
   begin
      Assert (not Contains (Empty_Tree, Root),
              "Contains returned True for an empty tree");
   end Test_Contains_Empty;

   -------------------------
   -- Test_Contains_Valid --
   -------------------------

   --  This test checks that Contains returns True for a valid node in a
   --  non-empty tree.

   procedure Test_Contains_Valid (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);
      Container := Add (Container, 2, Child (Root, Left));

      Assert (Contains (Container, Root),
              "Contains returned False for the root");
      Assert (Contains (Container, Child (Root, Left)),
              "Contains returned False for the Left node");
   end Test_Contains_Valid;

   -----------------------------
   -- Test_Contains_Not_Valid --
   -----------------------------

   --  This test checks that Contains returns False for the root of an empty
   --  tree.

   procedure Test_Contains_Not_Valid (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);

      Assert (Contains (Container, Root),
              "Contains returned False for the root");
      Assert (not Contains (Container, Child (Root, Left)),
              "Contains returned True for an invalid node");
   end Test_Contains_Not_Valid;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Contains (empty tree)",
                     Test_Contains_Empty'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Contains (valid node)",
                     Test_Contains_Valid'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Contains (invalid node)",
                     Test_Contains_Not_Valid'Access));
   end Add_To_Suite;

end Functional_Tree_Tests.Contains_Tests;