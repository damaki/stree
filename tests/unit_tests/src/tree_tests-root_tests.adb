--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Root_Tests is
   use Integer_3Way_Trees;

   ---------------------
   -- Test_Root_Empty --
   ---------------------

   --  Test that Root returns No_Element for an empty tree

   procedure Test_Root_Empty (T : in out Test_Fixture) is
   begin
      Assert (Root (Empty_Tree) = No_Element,
              "Root is not No_Element for an empty tree");
   end Test_Root_Empty;

   -------------------------
   -- Test_Root_Non_Empty --
   -------------------------

   --  Test that Root returns a valid cursor

   procedure Test_Root_Non_Empty (T : in out Test_Fixture) is
      Container : Tree;
      R         : Cursor;
   begin
      Insert_Root (Container, 1);
      R := Root (Container);

      Assert (R /= No_Element, "Root is No_Element for an empty tree");
      Assert (Has_Element (Container, R), "Root is not a valid cursor");
      Assert (Element (Container, R) = 1, "Root element has wrong value");
   end Test_Root_Non_Empty;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Unbounded_Multiway_Trees.Root (Empty)",
                                 Test_Root_Empty'Access));
      S.Add_Test (Caller.Create ("Unbounded_Multiway_Trees.Root (Non-empty)",
                                 Test_Root_Non_Empty'Access));
   end Add_To_Suite;

end Tree_Tests.Root_Tests;