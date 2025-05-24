--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.First_Tests is
   use Integer_3Way_Trees;

   ----------------------
   -- Test_First_Empty --
   ----------------------

   --  Test that First returns No_Element for an empty tree.

   procedure Test_First_Empty (T : in out Test_Fixture) is
   begin
      Assert (First (Empty_Tree) = No_Element, "First was not No_Element");
   end Test_First_Empty;

   --------------------------
   -- Test_First_Not_Empty --
   --------------------------

   --  Test that First returns the Root node for a tree with multiple nodes.

   procedure Test_First_Not_Empty (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      Assert (First (Container) = Root (Container), "First was not the root");
   end Test_First_Not_Empty;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("First (Empty)",
                                 Test_First_Empty'Access));
      S.Add_Test (Caller.Create ("First (Not empty)",
                                 Test_First_Not_Empty'Access));
   end Add_To_Suite;

end Tree_Tests.First_Tests;