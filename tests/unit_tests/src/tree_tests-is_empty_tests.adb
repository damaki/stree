--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Is_Empty_Tests is
   use Integer_3Way_Trees;

   -------------------------
   -- Test_Is_Empty_Empty --
   -------------------------

   --  Test that Is_Empty returns True when presented with an empty tree

   procedure Test_Is_Empty_Empty (T : in out Test_Fixture) is
   begin
      Assert (Is_Empty (Empty_Tree), "Is_Empty returned False");
   end Test_Is_Empty_Empty;

   -----------------------------
   -- Test_Is_Empty_Not_Empty --
   -----------------------------

   --  Test that Is_Empty returns True when presented with tree that is not
   --  empty.

   procedure Test_Is_Empty_Not_Empty (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;
   begin
      Insert_Root (Container, 5);
      Assert (not Is_Empty (Container), "Is_Empty returned True");
   end Test_Is_Empty_Not_Empty;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Is_Empty (empty tree)",
                                 Test_Is_Empty_Empty'Access));
      S.Add_Test (Caller.Create ("Is_Empty (not empty tree)",
                                 Test_Is_Empty_Not_Empty'Access));
   end Add_To_Suite;

end Tree_Tests.Is_Empty_Tests;