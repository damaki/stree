--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Containers;
with AUnit.Assertions; use AUnit.Assertions;

with Support;

package body Tree_Tests.Delete_Tests is
   use Integer_3Way_Trees;

   use type Ada.Containers.Count_Type;

   package S is new Support (Integer_3Way_Trees);

   ---------------------------
   -- Test_Delete_Leaf_Node --
   ---------------------------

   --  This test checks that Delete correctly updates the tree when a single
   --  leaf node is deleted.
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
   --  Node 4 is deleted.

   procedure Test_Delete_Leaf_Node (T : in out Test_Fixture) is
      Nodes : S.Cursor_Map (1 .. 8);

      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Nodes (1) := Root (Container);

      S.Insert_Child (Container, 2, 1, Left, Nodes);
      S.Insert_Child (Container, 3, 2, Left, Nodes);
      S.Insert_Child (Container, 4, 2, Middle, Nodes);
      S.Insert_Child (Container, 5, 1, Middle, Nodes);
      S.Insert_Child (Container, 6, 1, Right, Nodes);
      S.Insert_Child (Container, 7, 6, Middle, Nodes);
      S.Insert_Child (Container, 8, 7, Right, Nodes);

      Delete (Container, Nodes (4));

      Assert (Length (Container) = 7,
              "Container has wrong after before deletion");

      S.Check_Has_Elements
        (Container         => Container,
         Nodes             => Nodes,
         Expected_Elements => [1 .. 3 | 5 .. 8 => True,
                               4               => False]);

      S.Check_Elements (Container, Nodes);

      S.Check_Parents
        (Container        => Container,
         Nodes            => Nodes,
         Expected_Parents => [1 => No_Element,
                              2 => Nodes (1),
                              3 => Nodes (2),
                              4 => No_Element, --  Node 4 deleted
                              5 => Nodes (1),
                              6 => Nodes (1),
                              7 => Nodes (6),
                              8 => Nodes (7)]);

      S.Check_Children
        (Container         => Container,
         Nodes             => Nodes,
         Expected_Children => [1 => [Left   => Nodes (2),
                                     Middle => Nodes (5),
                                     Right  => Nodes (6)],
                               2 => [Left   => Nodes (3),
                                     Middle => No_Element,  --  Deleted child
                                     Right  => No_Element],
                               3 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element],
                               4 => [Left   => No_Element,  --  Node 4 deleted
                                     Middle => No_Element,
                                     Right  => No_Element],
                               5 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element],
                               6 => [Left   => No_Element,
                                     Middle => Nodes (7),
                                     Right  => No_Element],
                               7 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => Nodes (8)],
                               8 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element]]);
   end Test_Delete_Leaf_Node;

   -------------------------
   -- Test_Delete_Subtree --
   -------------------------

   --  This test checks that Delete correctly updates the tree when a subtree
   --  is deleted.
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
   --  Node 6 is deleted.

   procedure Test_Delete_Subtree (T : in out Test_Fixture) is
      Nodes : S.Cursor_Map (1 .. 8);

      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Nodes (1) := Root (Container);

      S.Insert_Child (Container, 2, 1, Left, Nodes);
      S.Insert_Child (Container, 3, 2, Left, Nodes);
      S.Insert_Child (Container, 4, 2, Middle, Nodes);
      S.Insert_Child (Container, 5, 1, Middle, Nodes);
      S.Insert_Child (Container, 6, 1, Right, Nodes);
      S.Insert_Child (Container, 7, 6, Middle, Nodes);
      S.Insert_Child (Container, 8, 7, Right, Nodes);

      Delete (Container, Nodes (6));

      Assert (Length (Container) = 5,
              "Container has wrong after before deletion");

      S.Check_Has_Elements
        (Container         => Container,
         Nodes             => Nodes,
         Expected_Elements => [1 .. 5 => True,
                               6 .. 8 => False]);

      S.Check_Elements (Container, Nodes (1 .. 5));

      S.Check_Parents
        (Container        => Container,
         Nodes            => Nodes (1 .. 5),
         Expected_Parents => [1 => No_Element,
                              2 => Nodes (1),
                              3 => Nodes (2),
                              4 => Nodes (2),
                              5 => Nodes (1)]);

      S.Check_Children
        (Container         => Container,
         Nodes             => Nodes (1 .. 5),
         Expected_Children => [1 => [Left   => Nodes (2),
                                     Middle => Nodes (5),
                                     Right  => No_Element], --  Deleted child
                               2 => [Left   => Nodes (3),
                                     Middle => Nodes (4),
                                     Right  => No_Element],
                               3 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element],
                               4 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element],
                               5 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element]]);
   end Test_Delete_Subtree;

   -----------------------------
   -- Test_Delete_Entire_Tree --
   -----------------------------

   --  This test checks that the tree is empty after deleting the root node.

   procedure Test_Delete_Entire_Tree (T : in out Test_Fixture) is
      Nodes : S.Cursor_Map (1 .. 8);

      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Nodes (1) := Root (Container);

      S.Insert_Child (Container, 2, 1, Left, Nodes);
      S.Insert_Child (Container, 3, 2, Left, Nodes);
      S.Insert_Child (Container, 4, 2, Middle, Nodes);
      S.Insert_Child (Container, 5, 1, Middle, Nodes);
      S.Insert_Child (Container, 6, 1, Right, Nodes);
      S.Insert_Child (Container, 7, 6, Middle, Nodes);
      S.Insert_Child (Container, 8, 7, Right, Nodes);

      Delete (Container, Nodes (1));

      Assert (Length (Container) = 0,
              "Container has wrong after before deletion");

      Assert (Is_Empty (Container), "Is_Empty returned False");

      S.Check_Has_Elements
        (Container         => Container,
         Nodes             => Nodes,
         Expected_Elements => [1 .. 8 => False]);

      for C in Container loop
         Assert (False, "Empty tree is iterable");
      end loop;
   end Test_Delete_Entire_Tree;

   --------------------------------
   -- Test_Delete_Invalid_Cursor --
   --------------------------------

   --  This test checks that Delete raises Constraint_Error when given an
   --  invalid cursor.

   procedure Test_Delete_Invalid_Cursor (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 3, Root (Container), Left);
      Insert_Child (Container, 4, Root (Container), Middle);

      declare
      begin
         Delete (Container, Cursor'(Node => 123));

         Assert (False, "Constraint_Error was not raised");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Delete_Invalid_Cursor;

   ------------------
   -- Add_To_Suite --
   ------------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Delete (Leaf node)",
                     Test_Delete_Leaf_Node'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Delete (Subtree)",
                     Test_Delete_Subtree'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Delete (Root node)",
                     Test_Delete_Entire_Tree'Access));
      S.Add_Test (Caller.Create
                    ("Unbounded_Multiway_Trees.Delete (Invalid cursor)",
                     Test_Delete_Invalid_Cursor'Access));
   end Add_To_Suite;

end Tree_Tests.Delete_Tests;