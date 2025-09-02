--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Prev_Tests is
   use Integer_3Way_Trees;

   --------------------------
   -- Test_Prev_No_Element --
   --------------------------

   --  Test that calling Prev with No_Element returns No_Element.

   procedure Test_Prev_No_Element (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);

      Assert (Prev (Container, No_Element) = No_Element,
              "Prev of No_Element was not No_Element");
   end Test_Prev_No_Element;

   ------------------------------
   -- Test_Prev_Invalid_Cursor --
   ------------------------------

   --  Test that calling Prev on an invalid cursor returns No_Element.

   procedure Test_Prev_Invalid_Cursor (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);

      Assert (Prev (Container, Cursor'(Node => 2)) = No_Element,
              "Prev of invalid cursor was not No_Element");
   end Test_Prev_Invalid_Cursor;

   --------------------------
   -- Test_Prev_Valid_Tree --
   --------------------------

   --  Test that Prev returns the correct depth-first sequence (in reverse
   --  order) of nodes over a valid tree.
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
   --  And the expected sequence of elements is: 8, 7, 6, 5, 4, 3, 2, 1

   procedure Test_Prev_Valid_Tree (T : in out Test_Fixture) is
      Container : Tree;
      N         : Cursor;
      J         : Integer;
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

      for I in reverse Integer range 1 .. 8 loop
         Assert (Has_Element (Container, N),
                 "Invalid cursor at iteration" & I'Image);

         J := Element (Container, N);
         Assert (J = I, "Invalid value" & J'Image & " at iteration" & I'Image);

         N := Prev (Container, N);
      end loop;

      Assert (N = No_Element, "Cursor was not No_Element at end of sequence");
   end Test_Prev_Valid_Tree;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Prev (No_Element)",
                     Test_Prev_No_Element'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Prev (Invalid cursor)",
                     Test_Prev_Invalid_Cursor'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Prev (Valid tree)",
                     Test_Prev_Valid_Tree'Access));
   end Add_To_Suite;

end Tree_Tests.Prev_Tests;