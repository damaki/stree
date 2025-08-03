--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Containers;
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Length_Tests is
   use Integer_3Way_Trees;

   use type Ada.Containers.Count_Type;

   -------------------
   -- Test_Length_0 --
   -------------------

   --  Test that an empty tree has length zero.

   procedure Test_Length_0 (T : in out Test_Fixture) is
   begin
      Assert (Length (Empty_Tree) = 0, "Length was not zero");
   end Test_Length_0;

   -------------------
   -- Test_Length_1 --
   -------------------

   --  Test that a tree with one root node has length 1.

   procedure Test_Length_1 (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 5);
      Assert (Length (Container) = 1, "Length was not 1");
   end Test_Length_1;

   -------------------
   -- Test_Length_2 --
   -------------------

   --  Test that a tree with one root and one child has length 2.

   procedure Test_Length_2 (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 5);
      Insert_Child (Container, 5, Root (Container), Left);
      Assert (Length (Container) = 2, "Length was not 2");
   end Test_Length_2;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Unbounded_Multiway_Trees.Length (0 nodes)",
                                 Test_Length_0'Access));
      S.Add_Test (Caller.Create ("Unbounded_Multiway_Trees.Length (1 node)",
                                 Test_Length_1'Access));
      S.Add_Test (Caller.Create ("Unbounded_Multiway_Trees.Length (2 nodes)",
                                 Test_Length_2'Access));
   end Add_To_Suite;

end Tree_Tests.Length_Tests;