--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Is_Root_Tests is
   use Integer_3Way_Trees;

   -----------------------------
   -- Test_Is_Root_No_Element --
   -----------------------------

   --  Test that Is_Root returns false for No_Element

   procedure Test_Is_Root_No_Element (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Assert (not Is_Root (Container, No_Element),
              "Is_Root returned True for No_Element");
   end Test_Is_Root_No_Element;

   -----------------------
   -- Test_Is_Root_Root --
   -----------------------

   --  Test that Is_Root returns true for the root node

   procedure Test_Is_Root_Root (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Assert (Is_Root (Container, Root (Container)),
              "Is_Root returned False for the root node");
   end Test_Is_Root_Root;

   ---------------------------
   -- Test_Is_Root_Non_Root --
   ---------------------------

   --  Test that Is_Root returns false for a non-root node that is otherwise
   --  valid.

   procedure Test_Is_Root_Non_Root (T : in out Test_Fixture) is
      Container : Tree;
      N         : Cursor;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);

      N := Child (Container, Root (Container), Left);

      Assert (Has_Element (Container, N), "Could not create a valid cursor");

      Assert (not Is_Root (Container, N),
              "Is_Root returned True for a non-root node");
   end Test_Is_Root_Non_Root;

   ---------------------------------
   -- Test_Is_Root_Invalid_Cursor --
   ---------------------------------

   --  Test that Is_Root returns false for an invalid cursor

   procedure Test_Is_Root_Invalid_Cursor (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Assert (not Is_Root (Container, Cursor'(Node => 100)),
              "Is_Root returned True for an invalid cursor");
   end Test_Is_Root_Invalid_Cursor;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Is_Root (No_Element)",
                     Test_Is_Root_No_Element'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Is_Root (Root)",
                     Test_Is_Root_Root'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Is_Root (Non-root)",
                     Test_Is_Root_Non_Root'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Is_Root (Invalid cursor)",
                     Test_Is_Root_Invalid_Cursor'Access));
   end Add_To_Suite;

end Tree_Tests.Is_Root_Tests;