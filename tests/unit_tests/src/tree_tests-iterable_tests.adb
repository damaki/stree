--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Iterable_Tests is
   use Integer_3Way_Trees;

   -------------------------
   -- Test_Iterable_Empty --
   -------------------------

   procedure Test_Iterable_Empty (T : in out Test_Fixture) is
   begin
      for I of Empty_Tree loop
         Assert (False, "Empty tree contained an element");
      end loop;
   end Test_Iterable_Empty;

   ------------------------------
   -- Test_Iterable_Valid_Tree --
   ------------------------------

   --  Test that iterating over a valid tree iterates in the correct sequence
   --  (depth-first).
   --
   --  The tree in this example is:
   --            1
   --          / | \
   --         2  5  6
   --       / |     |
   --      3  4     7
   --                \
   --                 8
   --
   --  Where:
   --   * 2 is the Left child of 1
   --   * 5 is the Middle child of 1
   --   * 6 is the Right child of 1
   --   * and so on...
   --
   --  And the expected sequence of elements is: 1, 2, 3, 4, 5, 6, 7, 8

   procedure Test_Iterable_Valid_Tree (T : in out Test_Fixture) is
      Container : Tree;
      N         : Cursor;
      I         : Integer := 1;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 5, Root (Container), Middle);
      Insert_Child (Container, 6, Root (Container), Right);

      N := Child (Container, Root (Container), Left);
      Insert_Child (Container, 3, N, Left);
      Insert_Child (Container, 4, N, Middle);

      N := Child (Container, Root (Container), Right);
      Insert_Child (Container, 7, N, Middle);

      N := Child (Container, N, Middle);
      Insert_Child (Container, 8, N, Right);

      N := Last (Container);

      for J of Container loop
         Assert (J = I, "Incorrect value at iteration" & I'Image);
         I := I + 1;
      end loop;

   end Test_Iterable_Valid_Tree;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Iterable (Empty)",
                                 Test_Iterable_Empty'Access));
      S.Add_Test (Caller.Create ("Iterable (Valid tree)",
                                 Test_Iterable_Valid_Tree'Access));
   end Add_To_Suite;

end Tree_Tests.Iterable_Tests;