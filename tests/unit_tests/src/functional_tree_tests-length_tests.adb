--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;
with SPARK.Big_Integers;

package body Functional_Tree_Tests.Length_Tests is
   use Integer_3Way_Trees;

   use type SPARK.Big_Integers.Big_Integer;

   -------------------
   -- Test_Length_0 --
   -------------------

   --  This test checks that Length returns 0 for an empty tree

   procedure Test_Length_0 (T : in out Test_Fixture) is
   begin
      Assert (Length (Empty_Tree) = 0,
              "Length was not zero");
   end Test_Length_0;

   -------------------
   -- Test_Length_1 --
   -------------------

   --  This test checks that Length returns 1 for a tree with 1 node

   procedure Test_Length_1 (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);

      Assert (Length (Container) = 1, "Length was not 1");
   end Test_Length_1;

   -------------------
   -- Test_Length_2 --
   -------------------

   --  This test checks that Length returns 2 for a tree with 2 nodes

   procedure Test_Length_2 (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);
      Container := Add (Container, 2, Child (Root, Left));

      Assert (Length (Container) = 2, "Length was not 2");
   end Test_Length_2;

   -------------------
   -- Test_Length_3 --
   -------------------

   --  This test checks that Length returns 3 for a tree with 3 nodes

   procedure Test_Length_3 (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);
      Container := Add (Container, 2, Child (Root, Left));
      Container := Add (Container, 3, Child (Root, Right));

      Assert (Length (Container) = 3, "Length was not 3");
   end Test_Length_3;

   -------------------
   -- Test_Length_4 --
   -------------------

   --  This test checks that Length returns 4 for a tree with 4 nodes

   procedure Test_Length_4 (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);
      Container := Add (Container, 2, Child (Root, Left));
      Container := Add (Container, 3, Child (Root, Right));
      Container := Add (Container, 3, Child (Child (Root, Left), Right));

      Assert (Length (Container) = 4, "Length was not 4");
   end Test_Length_4;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Length (0 nodes)",
                     Test_Length_0'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Length (1 nodes)",
                     Test_Length_1'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Length (2 nodes)",
                     Test_Length_2'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Length (3 nodes)",
                     Test_Length_3'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Length (4 nodes)",
                     Test_Length_4'Access));
   end Add_To_Suite;

end Functional_Tree_Tests.Length_Tests;