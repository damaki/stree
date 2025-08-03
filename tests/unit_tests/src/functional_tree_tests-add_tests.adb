--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Functional_Tree_Tests.Add_Tests is
   use Integer_3Way_Trees;

   -------------------------
   -- Test_Add_From_Empty --
   -------------------------

   --  This test checks that Add correctly adds the root node from an empty
   --  tree.

   procedure Test_Add_From_Empty (T : in out Test_Fixture) is
      Container : Tree := Empty_Tree;
   begin
      Container := Add (Container, 1, Root_Node);

      Assert (Contains (Container, Root_Node),
              "Root_Node not in container");
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
      Container := Add (Container, 1, []);
      Container := Add (Container, 2, [Left]);
      Container := Add (Container, 3, [Left, Left]);
      Container := Add (Container, 4, [Left, Middle]);
      Container := Add (Container, 5, [Middle]);
      Container := Add (Container, 6, [Right]);
      Container := Add (Container, 7, [Right, Middle]);
      Container := Add (Container, 8, [Right, Middle, Right]);

      Assert (Get (Container, [])                     = 1, "bad value for 1");
      Assert (Get (Container, [Left])                 = 2, "bad value for 2");
      Assert (Get (Container, [Left, Left])           = 3, "bad value for 3");
      Assert (Get (Container, [Left, Middle])         = 4, "bad value for 4");
      Assert (Get (Container, [Middle])               = 5, "bad value for 5");
      Assert (Get (Container, [Right])                = 6, "bad value for 6");
      Assert (Get (Container, [Right, Middle])        = 7, "bad value for 7");
      Assert (Get (Container, [Right, Middle, Right]) = 8, "bad value for 8");
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