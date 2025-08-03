--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Functional_Tree_Tests.Add_Parent_Tests is
   use Integer_3Way_Trees;

   --------------------------
   -- Test_Add_Parent_Root --
   --------------------------

   --  This test checks that adding a parent to the root node correctly
   --  updates the tree structure.

   procedure Test_Add_Parent_Root (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, []);
      Container := Add_Parent (Container, 2, [], Middle);

      Assert (Get (Container, []) = 2, "root node is not 2");
      Assert (Get (Container, [Middle]) = 1, "middle node is not 1");
   end Test_Add_Parent_Root;

   ------------------------------
   -- Test_Add_Parent_Non_Root --
   ------------------------------

   --  This test checks that adding a parent to a non-root node correctly
   --  updates the tree structure.
   --
   --  The tree in this example adds node '9' as the parent of node '2'.
   --  Node '2' becomes the Middle child of node '9'.
   --
   --            1                    1
   --          / | \                / | \
   --         2  5  6              9  5  6
   --       / |     |     ==>      |     |
   --      3  4     7              2     7
   --                \           / |      \
   --                 8         3  4       8
   --

   procedure Test_Add_Parent_Non_Root (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, []);
      Container := Add (Container, 2, [Left]);
      Container := Add (Container, 3, [Left, Left]);
      Container := Add (Container, 4, [Left, Middle]);
      Container := Add (Container, 5, [Middle]);
      Container := Add (Container, 6, [Right]);
      Container := Add (Container, 7, [Right, Middle]);
      Container := Add (Container, 8, [Right, Middle, Right]);

      Container := Add_Parent (Container, 9, [Left], Middle);

      Assert (Get (Container, [])                     = 1, "bad value for 1");
      Assert (Get (Container, [Left])                 = 9, "bad value for 9");
      Assert (Get (Container, [Left, Middle])         = 2, "bad value for 2");
      Assert (Get (Container, [Left, Middle, Left])   = 3, "bad value for 3");
      Assert (Get (Container, [Left, Middle, Middle]) = 4, "bad value for 4");
      Assert (Get (Container, [Middle])               = 5, "bad value for 5");
      Assert (Get (Container, [Right])                = 6, "bad value for 6");
      Assert (Get (Container, [Right, Middle])        = 7, "bad value for 7");
      Assert (Get (Container, [Right, Middle, Right]) = 8, "bad value for 8");
   end Test_Add_Parent_Non_Root;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Add_Parent (root)",
                     Test_Add_Parent_Root'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Add_Parent (non-root)",
                     Test_Add_Parent_Non_Root'Access));
   end Add_To_Suite;

end Functional_Tree_Tests.Add_Parent_Tests;