--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with SPARK.Containers;
with SPARK.Containers.Functional.Sets;

package body SPARK_Multiway_Trees with
  SPARK_Mode => On
is

   package body Formal_Model is

      package I_Sets is new SPARK.Containers.Functional.Sets (Index_Type, "=");

      function All_Indexes return I_Sets.Set with
        Global => null,
        Post   => (for all I in Index_Type =>
                     I_Sets.Contains (All_Indexes'Result, I))
                  and then I_Sets.Length (All_Indexes'Result)
                           = To_Big_Integer (Max_Size);

      -------------------------
      -- Element_Logic_Equal --
      -------------------------

      function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
      with SPARK_Mode => Off
      is
      begin
         SPARK.Containers.Check_Or_Fail;
         return Left = Right;
      end Element_Logic_Equal;

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element (F : Forest; C : Cursor) return Boolean is
        (C.Node /= Empty and then Contains (F.Nodes, C.Node));

      -------------
      -- Element --
      -------------

      function Element (F : Forest; C : Cursor) return Element_Type is
        (Element (F.Nodes, C.Node).Element);

      ------------
      -- Parent --
      ------------

      function Parent (F : Forest; C : Cursor) return Cursor is
        (Cursor'(Node => Element (F.Nodes, C.Node).Parent));

      -----------
      -- Child --
      -----------

      function Child
        (F : Forest;
         C : Cursor;
         W : Way_Type)
         return Cursor
      is
        (Cursor'(Node => Element (F.Nodes, C.Node).Ways (W)));

      -------------
      -- Is_Root --
      -------------

      function Is_Root (F : Forest; C : Cursor) return Boolean is
        (Has_Element (F, C) and then Element (F.Nodes, C.Node).Position = Top);

      --------------
      -- Position --
      --------------

      function Position (F : Forest; I : Index_Type) return Way_Type is
        (Element (F.Nodes, I).Position);

      -----------
      -- Model --
      -----------

      function Model (F : Forest; Root : Cursor) return Model_Type is
         use I_Sets;

         type Boolean_Array is array (Index_Type) of Boolean;

         function Next (Todo : Boolean_Array) return Extended_Index_Type with
           Post => (if Next'Result = Empty
                    then (for all T of Todo => not T)
                    else Todo (Next'Result));

         function Next (Todo : Boolean_Array) return Extended_Index_Type is
         begin
            for I in Todo'Range loop
               pragma Loop_Invariant (for all J in 1 .. I - 1 => not Todo (J));
               if Todo (I) then
                  return I;
               end if;
            end loop;

            return Empty;
         end Next;

         Todo : Boolean_Array := [others => False];

         Unseen : I_Sets.Set := All_Indexes;

         M : Model_Type;
         I : Extended_Index_Type := Root.Node;

      begin
         --  Insert the root node in the todo list

         Todo (Root.Node)      := True;
         M (Root.Node).In_Tree := True;

         while I /= Empty loop
            pragma Loop_Variant (Decreases => Length (Unseen));

            --  Node I is in the todo list and in the tree
            pragma Loop_Invariant (Todo (I) and then M (I).In_Tree);

            --  All nodes in the todo list are in the tree
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if Todo (J) then M (J).In_Tree));

            --  All nodes in the tree are in the forest
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if M (J).In_Tree then Contains (F.Nodes, J)));

            --  All nodes in the todo list are in the forest
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if Todo (J) then Contains (F.Nodes, J)));

            --  The path of a node in the todo list is maximal w.r.t. other
            --  nodes which are known to be in the tree at this stage.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if Todo (J) then
                    (for all K in Index_Type =>
                       (if M (K).In_Tree
                        then not (M (J).Path < M (K).Path)))));

            --  Children of nodes in the todo list are not yet in the tree
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if Todo (J) then
                    (for all W of Element (F.Nodes, J).Ways =>
                       (if W /= Empty then
                          not M (W).In_Tree and then not Todo (W)))));

            --  The root is in tree with an empty path
            pragma Loop_Invariant
              (M (Root.Node).In_Tree and then Length (M (Root.Node).Path) = 0);

            --  Non-root nodes in the tree don't have position Top and have a
            --  parent.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if M (J).In_Tree and then J /= Root.Node
                  then Element (F.Nodes, J).Position /= Top
                       and then Element (F.Nodes, J).Parent /= Empty));

            --  Non-root nodes are in the tree iff their parent is in the tree
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if M (J).In_Tree and then J /= Root.Node
                  then Element (F.Nodes, J).Parent /= Empty
                       and then M (Element (F.Nodes, J).Parent).In_Tree));

            --  The path from the root to non-root tree nodes is equal to the
            --  path to their parent extended by the last direction to get to
            --  the node. For all other nodes, the path is empty.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if M (J).In_Tree and then J /= Root.Node
                  then Is_Add (M (Element (F.Nodes, J).Parent).Path,
                               Element (F.Nodes, J).Position,
                               M (J).Path)));

            --  A node known to be in the tree is either the root node, or a
            --  node whose parent is known to be in the tree. In that case, the
            --  node is either known to be in the tree, or the parent is still
            --  in the todo list.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if J /= Root.Node and then Contains (F.Nodes, J) then
                    (if Element (F.Nodes, J).Parent /= Empty
                        and then M (Element (F.Nodes, J).Parent).In_Tree
                     then M (J).In_Tree
                          or else Todo (Element (F.Nodes, J).Parent)
                     else not M (J).In_Tree)));

            --  A node known to be in the tree does not have its parent in the
            --  Todo list.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if M (J).In_Tree and then J /= Root.Node
                  then not Todo (Element (F.Nodes, J).Parent)));

            --  Nodes in the tree all have different associated paths
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if M (J).In_Tree then
                    (for all K in Index_Type =>
                       (if M (K).In_Tree and then M (K).Path = M (J).Path
                        then J = K))));

            --  The longest path stored so far is of size
            --  Max_Size - Length (Unseen) at most.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (Length (M (J).Path)
                  <= To_Big_Integer (Max_Size) - Length (Unseen)));

            --  Nodes that have not been handled yet are either not in the tree
            --  or in the todo list.
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (Contains (Unseen, J)
                  = (not M (J).In_Tree or else Todo (J))));

            --  Every node not in the tree has no path
            pragma Loop_Invariant
              (for all J in Index_Type =>
                 (if not M (J).In_Tree then Length (M (J).Path) = 0));

            Unseen := Remove (Unseen, I);

            declare
               J : Extended_Index_Type;
            begin
               J := Element (F.Nodes, I).Ways (1);
               if J /= Empty then
                  pragma Assert
                    (Element (F.Nodes, Element (F.Nodes, I).Ways (1)).Parent
                     = I);
                  pragma Assert (not Is_Empty (Unseen));

                  Todo (J) := True;
                  M (J)    := (In_Tree => True,
                               Path    => Add (M (I).Path, 1));
               end if;
            end;

            declare
               J : Extended_Index_Type;
            begin
               J := Element (F.Nodes, I).Ways (2);
               if J /= Empty then
                  pragma Assert (not Is_Empty (Unseen));

                  Todo (J) := True;
                  M (J)    := (In_Tree => True,
                               Path    => Add (M (I).Path, 2));
               end if;
            end;

            --  Nothing more to do for node I

            Todo (I) := False;
            I        := Next (Todo);
         end loop;

         return M;
      end Model;

      -----------------
      -- All_Indexes --
      -----------------

      function All_Indexes return I_Sets.Set is
         use I_Sets;
         S : I_Sets.Set;
      begin
         for I in Index_Type loop
            pragma Loop_Invariant (for all J in 1 .. I - 1 => Contains (S, J));
            pragma Loop_Invariant (for all J of S => J < I);
            pragma Loop_Invariant
              (Length (S) = To_Big_Integer (Integer (I - Index_Type'First)));

            S := Add (S, I);
         end loop;

         return S;
      end All_Indexes;

   end Formal_Model;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Container : Forest;
      Position  : Cursor) return Boolean
   is
     (Position.Node /= Empty
      and then Contains (Container.Nodes, Position.Node));

   -------------
   -- Element --
   -------------

   function Element
     (Container : Forest;
      Position  : Cursor)
      return Element_Type
   is
      E_Acc : constant access constant Node_Type :=
                  Constant_Reference (Container.Nodes, Position.Node);
   begin
      return E_Acc.all.Element;
   end Element;

   -------------
   -- Is_Root --
   -------------

   function Is_Root
     (Container : Forest;
      Position  : Cursor)
      return Boolean
   is
      E_Acc : constant access constant Node_Type :=
                Constant_Reference (Container.Nodes, Position.Node);
   begin
      return E_Acc.all.Position = Top;
   end Is_Root;

   ------------
   -- Parent --
   ------------

   function Parent
     (Container : Forest;
      Position  : Cursor)
      return Cursor
   is
      E_Acc : constant access constant Node_Type :=
                Constant_Reference (Container.Nodes, Position.Node);
   begin
      return Cursor'(Node => E_Acc.all.Parent);
   end Parent;

   -----------
   -- Child --
   -----------

   function Child
     (Container : Forest;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   is
      E_Acc : constant access constant Node_Type :=
                Constant_Reference (Container.Nodes, Position.Node);
   begin
      return Cursor'(Node => E_Acc.all.Ways (Way));
   end Child;

   function Constant_Reference
     (Container : Forest;
      Position  : Cursor) return not null access constant Element_Type
   is
     (Constant_Reference (Container.Nodes, Position.Node).all.Element'Access);

   --------------------
   -- Tree_Structure --
   --------------------

   function Tree_Structure (F : Node_Maps.Map) return Boolean is
     (
      --  The parent of a node is either Empty or references another, valid
      --  node.
      (for all I in Index_Type =>
         (if Contains (F, I) and then Element (F, I).Parent /= Empty
          then Contains (F, Element (F, I).Parent)))

      --  Each way of a node is either Empty or references another, valid node.
      and then
        (for all I in Index_Type =>
           (if Contains (F, I) then
              (for all W of Element (F, I).Ways =>
                 (if W /= Empty then Contains (F, W)))))

      --  If a node has position Top then it has no parent, otherwise it
      --  has a valid parent
      and then
        (for all I in Index_Type =>
           (if Contains (F, I) then
              (if Element (F, I).Position = Top
               then Element (F, I).Parent = Empty
               else Element (F, I).Parent /= Empty
                    and then Contains (F, Element (F, I).Parent))))

      --  If a node is a child (has a position), then it is the child of its
      --  parent.
      and then
        (for all I in Index_Type =>
           (if Contains (F, I) and then Element (F, I).Position /= Top then
              Element (F, Element (F, I).Parent).Ways (Element (F, I).Position)
                = I))

      --  Every child of node I has I as its parent
      and then
        (for all I in Index_Type =>
           (if Contains (F, I) then
              (for all W in Way_Type =>
                 (if Element (F, I).Ways (W) /= Empty then
                    Element (F, Element (F, I).Ways (W)).Position = W
                    and then Element (F, Element (F, I).Ways (W)).Parent
                             = I))))
     );

end SPARK_Multiway_Trees;