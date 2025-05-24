--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Parent_Tests is
   use Integer_3Way_Trees;

   ----------------------------
   -- Test_Parent_No_Element --
   ----------------------------

   --  Test that Parent returns No_Element when given No_Element.

   procedure Test_Parent_No_Element (T : in out Test_Fixture) is
   begin
      Assert (Parent (Empty_Tree, No_Element) = No_Element,
              "Parent is not No_Element");
   end Test_Parent_No_Element;

   ----------------------
   -- Test_Parent_Root --
   ----------------------

   --  Test that Parent returns No_Element for the root node.

   procedure Test_Parent_Root (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Assert (Parent (Container, Root (Container)) = No_Element,
              "Parent of Root is not No_Element");
   end Test_Parent_Root;

   -----------------------
   -- Test_Parent_Child --
   -----------------------

   --  Test that Parent returns the correct cursor for each child of a node.

   procedure Test_Parent_Child (T : in out Test_Fixture) is
      Container : Tree;
      N         : Cursor;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      --  Check that each child has the root as the parent

      for D in Direction loop
         N := Child (Container, Root (Container), D);
         Assert (Parent (Container, N) = Root (Container),
                 "Incorrect parent for direction " & D'Image);
      end loop;
   end Test_Parent_Child;

   ----------------------------
   -- Test_Parent_Grandchild --
   ----------------------------

   --  Test that Parent returns the correct cursor for a 2-level deep tree.

   procedure Test_Parent_Grandchild (T : in out Test_Fixture) is
      Container : Tree;
      P         : Cursor;
      C         : Cursor;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      P := Child (Container, Root (Container), Middle);
      Insert_Child (Container, 5, P, Left);
      C := Child (Container, P, Left);

      Assert (Parent (Container, C) = P, "Incorrect parent");
   end Test_Parent_Grandchild;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Parent (No_Element)",
                                 Test_Parent_No_Element'Access));
      S.Add_Test (Caller.Create ("Parent (Root)",
                                 Test_Parent_Root'Access));
      S.Add_Test (Caller.Create ("Parent (Child)",
                                 Test_Parent_Child'Access));
      S.Add_Test (Caller.Create ("Parent (Grandchild)",
                                 Test_Parent_Grandchild'Access));
   end Add_To_Suite;

end Tree_Tests.Parent_Tests;