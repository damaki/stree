--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Root_Element_Tests is
   use Integer_3Way_Trees;

   ---------------------------------
   -- Test_Root_Element_Non_Empty --
   ---------------------------------

   --  Test that Root_Element returns the correct value in a tree with several
   --  nodes.

   procedure Test_Root_Element_Non_Empty (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      Assert (Root_Element (Container) = 1, "Root_Element has wrong value");
   end Test_Root_Element_Non_Empty;

   -----------------------------
   -- Test_Root_Element_Empty --
   -----------------------------

   --  Test that Root_Element raises a Constraint_Error when the precondition
   --  (Is_Empty must be False) is violated.

   procedure Test_Root_Element_Empty (T : in out Test_Fixture) is
      Unused : Integer;
   begin
      Unused := Root_Element (Empty_Tree);
      Assert (False, "Constraint_Error not raised");
   exception
      when Constraint_Error =>
         null;
   end Test_Root_Element_Empty;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Root_Element (Non Empty)",
                                 Test_Root_Element_Non_Empty'Access));
      S.Add_Test (Caller.Create ("Root_Element (Empty)",
                                 Test_Root_Element_Empty'Access));
   end Add_To_Suite;

end Tree_Tests.Root_Element_Tests;