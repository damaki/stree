--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;
with Support;

package body Tree_Tests.Move_Subtree_Tests is
   use Integer_3Way_Trees;

   package S is new Support (Integer_3Way_Trees);

   ---------------------------------
   -- Test_Move_Subtree_Single_Node --
   ---------------------------------

   --  This test checks that Move_Subtree correctly moves a subtree
   --  consisting of a single node.
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
   --  Node 5 is spliced to be the Middle child of node 4.

   procedure Test_Move_Subtree_Single_Node (T : in out Test_Fixture) is
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

      Move_Subtree
        (Container    => Container,
         Subtree_Root => Nodes (5),
         New_Parent   => Nodes (4),
         Way          => Middle);

      S.Check_Parents
        (Container        => Container,
         Nodes            => Nodes,
         Expected_Parents => [1 => No_Element,
                              2 => Nodes (1),
                              3 => Nodes (2),
                              4 => Nodes (2),
                              5 => Nodes (4),
                              6 => Nodes (1),
                              7 => Nodes (6),
                              8 => Nodes (7)]);

      S.Check_Children
        (Container         => Container,
         Nodes             => Nodes,
         Expected_Children => [1 => [Left   => Nodes (2),
                                     Middle => No_Element,
                                     Right  => Nodes (6)],
                               2 => [Left   => Nodes (3),
                                     Middle => Nodes (4),
                                     Right  => No_Element],
                               3 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element],
                               4 => [Left   => No_Element,
                                     Middle => Nodes (5),
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
   end Test_Move_Subtree_Single_Node;

   --------------------------------------
   -- Test_Move_Subtree_Multiple_Nodes --
   --------------------------------------

   --  This test checks that Move_Subtree correctly moves a subtree
   --  consisting of multiple nodes.
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
   --  Node 2 is spliced to be the Middle child of node 7.

   procedure Test_Move_Subtree_Multiple_Nodes (T : in out Test_Fixture) is
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

      Move_Subtree
        (Container    => Container,
         Subtree_Root => Nodes (2),
         New_Parent   => Nodes (7),
         Way          => Middle);

      S.Check_Parents
        (Container        => Container,
         Nodes            => Nodes,
         Expected_Parents => [1 => No_Element,
                              2 => Nodes (7),
                              3 => Nodes (2),
                              4 => Nodes (2),
                              5 => Nodes (1),
                              6 => Nodes (1),
                              7 => Nodes (6),
                              8 => Nodes (7)]);

      S.Check_Children
        (Container         => Container,
         Nodes             => Nodes,
         Expected_Children => [1 => [Left   => No_Element,
                                     Middle => Nodes (5),
                                     Right  => Nodes (6)],
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
                                     Right  => No_Element],
                               6 => [Left   => No_Element,
                                     Middle => Nodes (7),
                                     Right  => No_Element],
                               7 => [Left   => No_Element,
                                     Middle => Nodes (2),
                                     Right  => Nodes (8)],
                               8 => [Left   => No_Element,
                                     Middle => No_Element,
                                     Right  => No_Element]]);
   end Test_Move_Subtree_Multiple_Nodes;

   ----------------------------------------
   -- Test_Move_Subtree_Bad_Subtree_Root --
   ----------------------------------------

   --  Test that Move_Subtree raises Constraint_Error when the Subtree_Root
   --  parameter references an invalid node.

   procedure Test_Move_Subtree_Bad_Subtree_Root (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 5, Root (Container), Middle);
      Insert_Child (Container, 6, Root (Container), Right);

      declare
      begin
         Move_Subtree
           (Container    => Container,
            Subtree_Root => Cursor'(Node => 123),
            New_Parent   => Child (Container, Root (Container), Left),
            Way          => Middle);

         Assert (False, "Constraint_Error was not raised");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Move_Subtree_Bad_Subtree_Root;

   --------------------------------------
   -- Test_Move_Subtree_Bad_New_Parent --
   --------------------------------------

   --  Test that Move_Subtree raises Constraint_Error when the New_Parent
   --  parameter references an invalid node.

   procedure Test_Move_Subtree_Bad_New_Parent (T : in out Test_Fixture) is
      Container : Tree;
   begin
      Insert_Root (Container, 1);
      Insert_Child (Container, 2, Root (Container), Left);
      Insert_Child (Container, 5, Root (Container), Middle);
      Insert_Child (Container, 6, Root (Container), Right);

      declare
      begin
         Move_Subtree
           (Container    => Container,
            Subtree_Root => Child (Container, Root (Container), Left),
            New_Parent   => Cursor'(Node => 123),
            Way          => Middle);

         Assert (False, "Constraint_Error was not raised");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Move_Subtree_Bad_New_Parent;

   -----------------------------------
   -- Test_Move_Subtree_Cyclic_Tree --
   -----------------------------------

   --  Test that Move_Subtree raises Constraint_Error when the the New_Parent
   --  is equal to Subtree_Root.
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
   --  An attempt is made to splice node 6 to be a child of node 8

   procedure Test_Move_Subtree_Cyclic_Tree (T : in out Test_Fixture) is
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

      declare
      begin
         Move_Subtree
           (Container    => Container,
            Subtree_Root => Nodes (6),
            New_Parent   => Nodes (8),
            Way          => Middle);

         Assert (False, "Constraint_Error was not raised");
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Move_Subtree_Cyclic_Tree;

   ----------------
   -- Add_To_Suite --
   ----------------

   procedure Add_To_Suite (S : in out Test_Suite'Class) is
   begin
      S.Add_Test (Caller.Create ("Move_Subtree (Single node)",
                                 Test_Move_Subtree_Single_Node'Access));
      S.Add_Test (Caller.Create ("Move_Subtree (Multiple nodes)",
                                 Test_Move_Subtree_Multiple_Nodes'Access));
      S.Add_Test (Caller.Create ("Move_Subtree (Invalid Subtree_Root)",
                                 Test_Move_Subtree_Bad_Subtree_Root'Access));
      S.Add_Test (Caller.Create ("Move_Subtree (Invalid New_Parent)",
                                 Test_Move_Subtree_Bad_New_Parent'Access));
      S.Add_Test (Caller.Create ("Move_Subtree (Cycle)",
                                 Test_Move_Subtree_Cyclic_Tree'Access));
   end Add_To_Suite;

end Tree_Tests.Move_Subtree_Tests;