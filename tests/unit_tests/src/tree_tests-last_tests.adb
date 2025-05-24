--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Tree_Tests.Last_Tests is
   use Integer_3Way_Trees;

   ---------------------
   -- Test_Last_Empty --
   ---------------------

   --  Test that Last returns No_Element for an empty tree.

   procedure Test_Last_Empty (T : in out Test_Fixture) is
   begin
      Assert (Last (Empty_Tree) = No_Element, "Last was not No_Element");
   end Test_Last_Empty;

   -------------------------
   -- Test_Last_Not_Empty --
   -------------------------

   --  Test that Last returns the last node for a tree with multiple nodes.

   procedure Test_Last_Not_Empty (T : in out Test_Fixture) is
      Container : Tree;
      Expected  : Cursor;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Middle);
      Insert_Child (Container, 4, Root (Container), Right);

      Expected := Child (Container, Root (Container), Right);

      Assert (Last (Container) = Expected, "Incorrect last node");
   end Test_Last_Not_Empty;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Last (Empty)",
                                 Test_Last_Empty'Access));
      S.Add_Test (Caller.Create ("Last (Not empty)",
                                 Test_Last_Not_Empty'Access));
   end Add_To_Suite;

end Tree_Tests.Last_Tests;