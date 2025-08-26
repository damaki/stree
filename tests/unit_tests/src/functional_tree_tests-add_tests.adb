--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Functional_Tree_Tests.Add_Tests is
   use Integer_3Way_Trees;

   N_Left               : constant Path_Type := Child (Root, Left);
   N_Middle             : constant Path_Type := Child (Root, Middle);
   N_Right              : constant Path_Type := Child (Root, Right);
   N_Left_Left          : constant Path_Type := Child (N_Left, Left);
   N_Left_Middle        : constant Path_Type := Child (N_Left, Middle);
   N_Right_Middle       : constant Path_Type := Child (N_Right, Middle);
   N_Right_Middle_Right : constant Path_Type := Child (N_Right_Middle, Right);

   -------------------------
   -- Test_Add_From_Empty --
   -------------------------

   --  This test checks that Add correctly adds the root node from an empty
   --  tree.

   procedure Test_Add_From_Empty (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;
   begin
      Container := Add (Container, 1, Root);

      Assert (Contains (Container, Root),
              "Root not in container");
      Assert ((for all N of Container => Get (Container, N) = 1),
              "Unexpected element in container");
   end Test_Add_From_Empty;

   -----------------------
   -- Test_Add_Multiple --
   -----------------------

   --
   --  The tree in this example is:
   --            1
   --          / | \
   --         2  5  6
   --       / |     |
   --      3  4     7
   --                \
   --                 8

   procedure Test_Add_Multiple (T : in out Test_Fixture) is
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

      Assert (Get (Container, Root)                 = 1, "bad value for 1");
      Assert (Get (Container, N_Left)               = 2, "bad value for 2");
      Assert (Get (Container, N_Left_Left)          = 3, "bad value for 3");
      Assert (Get (Container, N_Left_Middle)        = 4, "bad value for 4");
      Assert (Get (Container, N_Middle)             = 5, "bad value for 5");
      Assert (Get (Container, N_Right)              = 6, "bad value for 6");
      Assert (Get (Container, N_Right_Middle)       = 7, "bad value for 7");
      Assert (Get (Container, N_Right_Middle_Right) = 8, "bad value for 8");
   end Test_Add_Multiple;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Add (from empty)",
                     Test_Add_From_Empty'Access));
      S.Add_Test (Caller.Create
                    ("Functional_Multiway_Trees.Add (multiple nodes)",
                     Test_Add_Multiple'Access));
   end Add_To_Suite;

end Functional_Tree_Tests.Add_Tests;