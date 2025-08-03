--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Has_Element_Tests is
   use Integer_3Way_Trees;

   ---------------------------------
   -- Test_Has_Element_No_Element --
   ---------------------------------

   --  Test that Has_Element returns False for No_Element, even when there
   --  are nodes in the tree.

   procedure Test_Has_Element_No_Element (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      Assert (not Has_Element (Container, No_Element),
              "Has_Element returned True for No_Element");
   end Test_Has_Element_No_Element;

   ----------------------------
   -- Test_Has_Element_Empty --
   ----------------------------

   --  Test that Has_Element returns False for an empty tree and an invalid
   --  cursor.

   procedure Test_Has_Element_Empty (T : in out Test_Fixture) is
   begin
      Assert (not Has_Element (Empty_Tree, Cursor'(Node => 1)),
              "Has_Element returned True for Cursor 1");
   end Test_Has_Element_Empty;

   -------------------------------------
   -- Test_Has_Element_Invalid_Cursor --
   -------------------------------------

   --  Test that Has_Element returns False for a non-empty tree, but with an
   --  invalid cursor.

   procedure Test_Has_Element_Invalid_Cursor (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);

      Assert (not Has_Element (Empty_Tree, Cursor'(Node => 2)),
              "Has_Element returned True for an invalid cursor");
   end Test_Has_Element_Invalid_Cursor;

   -----------------------------------
   -- Test_Has_Element_Valid_Cursor --
   -----------------------------------

   --  Test that Has_Element returns False for a non-empty tree and with a
   --  valid cursor.

   procedure Test_Has_Element_Valid_Cursor (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);

      Assert (Has_Element (Container, Root (Container)),
              "Has_Element returned False for a valid cursor");
   end Test_Has_Element_Valid_Cursor;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Has_Element (No_Element)",
                     Test_Has_Element_No_Element'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Has_Element (Empty Tree)",
                     Test_Has_Element_Empty'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Has_Element (Invalid cursor)",
                     Test_Has_Element_Invalid_Cursor'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Has_Element (Valid cursor)",
                     Test_Has_Element_Valid_Cursor'Access));
   end Add_To_Suite;

end Tree_Tests.Has_Element_Tests;