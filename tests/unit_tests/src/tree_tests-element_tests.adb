--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Element_Tests is
   use Integer_3Way_Trees;

   --------------------
   -- Test_Element_1 --
   --------------------

   --  Test that Element returns the correct value for a tree with 1 node.

   procedure Test_Element_1 (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 123);

      Assert (Element (Container, Root (Container)) = 123,
              "Invalid element value returned");
   end Test_Element_1;

   --------------------
   -- Test_Element_5 --
   --------------------

   --  Test that Element returns the correct value for each element in a tree
   --  of 5 nodes with the following element structure:
   --
   --       123
   --      / | \
   --   200 300 400
   --      /
   --   500

   procedure Test_Element_5 (T : in out Test_Fixture) is
      Container : Tree;
      R         : Cursor;
      M         : Cursor;
   begin
      Insert_Root (Container, 123);             -- Node T
      R := Root (Container);

      Insert_Child (Container, 200, R, Left);   -- Node L
      Insert_Child (Container, 300, R, Middle); -- Node M
      Insert_Child (Container, 400, R, Right);  -- Node R

      M := Child (Container, R, Middle);
      Insert_Child (Container, 500, M, Left);   -- Node ML

      Assert (Element (Container, R) = 123,
              "Invalid element value returned for node T");

      Assert (Element (Container, Child (Container, R, Left)) = 200,
              "Invalid element value returned for node L");

      Assert (Element (Container, Child (Container, R, Middle)) = 300,
              "Invalid element value returned for node M");

      Assert (Element (Container, Child (Container, R, Right)) = 400,
              "Invalid element value returned for node R");

      Assert (Element (Container, Child (Container, M, Left)) = 500,
              "Invalid element value returned for node ML");
   end Test_Element_5;

   ---------------------------------
   -- Test_Element_Invalid_Cursor --
   ---------------------------------

   --  Test that calling Element and violating its precondition
   --  (with a cursor that is not No_Element, but is still invalid) raises
   --  a Constraint_Error.

   procedure Test_Element_Invalid_Cursor (T : in out Test_Fixture) is
      Container : Tree;
      Unused    : Integer;
   begin
      Insert_Root (Container, 1);

      declare
      begin
         Unused := Element (Container, Cursor'(Node => 123));
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Element_Invalid_Cursor;

   -----------------------------
   -- Test_Element_No_Element --
   -----------------------------

   --  Test that calling Element and violating its precondition
   --  (with a No_Element cursor) raises a Constraint_Error.

   procedure Test_Element_No_Element (T : in out Test_Fixture) is
      Container : Tree;
      Unused    : Integer;
   begin
      Insert_Root (Container, 1);

      declare
      begin
         Unused := Element (Container, No_Element);
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Element_No_Element;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Element (1 node tree)",
                     Test_Element_1'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Element (5 node tree)",
                     Test_Element_5'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Element (Invalid cursor)",
                     Test_Element_Invalid_Cursor'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Element (No_Element cursor)",
                     Test_Element_No_Element'Access));
   end Add_To_Suite;

end Tree_Tests.Element_Tests;