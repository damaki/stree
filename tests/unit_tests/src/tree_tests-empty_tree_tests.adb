--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Containers;
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Empty_Tree_Tests is
   use Integer_3Way_Trees;

   use type Ada.Containers.Count_Type;

   ---------------------
   -- Test_Empty_Tree --
   ---------------------

   --  This test checks that Empty_Tree returns a tree that is empty, i.e.:
   --   * Is_Empty returns True.
   --   * Length is zero.
   --   * Root returns No_Element.

   procedure Test_Empty_Tree (T : in out Test_Fixture) is
      Container : constant Tree := Empty_Tree;

   begin
      Assert (Is_Empty (Container), "Is_Empty returned False");
      Assert (Length (Container) = 0, "Length is non-zero");
      Assert (Root (Container) = No_Element, "Root is not No_Element");
   end Test_Empty_Tree;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Unbounded_Multiway_Trees.Empty_Tree",
                                 Test_Empty_Tree'Access));
   end Add_To_Suite;

end Tree_Tests.Empty_Tree_Tests;