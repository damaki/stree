--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Root_Element_Tests is
   use Integer_3Way_Trees;

   -----------------------
   -- Test_Root_Element --
   -----------------------

   --  Test that Root_Element returns the correct value in a tree with several
   --  nodes.

   procedure Test_Root_Element (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      Assert (Root_Element (Container) = 1, "Root_Element has wrong value");
   end Test_Root_Element;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Root_Element",
                                 Test_Root_Element'Access));
   end Add_To_Suite;

end Tree_Tests.Root_Element_Tests;