--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Functional_Tree_Tests.Add_Parent_Tests is
   use Integer_3Way_Trees;

   N_Left               : constant Path_Type := Child (Root, Left);
   N_Middle             : constant Path_Type := Child (Root, Middle);
   N_Right              : constant Path_Type := Child (Root, Right);
   N_Left_Left          : constant Path_Type := Child (N_Left, Left);
   N_Left_Middle        : constant Path_Type := Child (N_Left, Middle);
   N_Left_Middle_Left   : constant Path_Type := Child (N_Left_Middle, Left);
   N_Left_Middle_Middle : constant Path_Type := Child (N_Left_Middle, Middle);
   N_Right_Middle       : constant Path_Type := Child (N_Right, Middle);
   N_Right_Middle_Right : constant Path_Type := Child (N_Right_Middle, Right);

   --------------------------
   -- Test_Add_Parent_Root --
   --------------------------

   --  This test checks that adding a parent to the root node correctly
   --  updates the tree structure.

   procedure Test_Add_Parent_Root (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;

   begin
      Container := Add (Container, 1, Root);
      Container := Add_Parent (Container, 2, Root, Middle);

      Assert (Get (Container, Root) = 2, "root node is not 2");
      Assert (Get (Container, N_Middle) = 1, "middle node is not 1");
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
      Container := Add (Container, 1, Root);
      Container := Add (Container, 2, N_Left);
      Container := Add (Container, 3, N_Left_Left);
      Container := Add (Container, 4, N_Left_Middle);
      Container := Add (Container, 5, N_Middle);
      Container := Add (Container, 6, N_Right);
      Container := Add (Container, 7, N_Right_Middle);
      Container := Add (Container, 8, N_Right_Middle_Right);

      Container := Add_Parent (Container, 9, N_Left, Middle);

      Assert (Get (Container, Root)                 = 1, "bad value for 1");
      Assert (Get (Container, N_Left)               = 9, "bad value for 9");
      Assert (Get (Container, N_Left_Middle)        = 2, "bad value for 2");
      Assert (Get (Container, N_Left_Middle_Left)   = 3, "bad value for 3");
      Assert (Get (Container, N_Left_Middle_Middle) = 4, "bad value for 4");
      Assert (Get (Container, N_Middle)             = 5, "bad value for 5");
      Assert (Get (Container, N_Right)              = 6, "bad value for 6");
      Assert (Get (Container, N_Right_Middle)       = 7, "bad value for 7");
      Assert (Get (Container, N_Right_Middle_Right) = 8, "bad value for 8");
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