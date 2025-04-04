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

      package I_Sets is new SPARK.Containers.Functional.Sets
        (Valid_Cursor_Range, "=");

      function All_Indexes return I_Sets.Set with
        Global => null,
        Post   => (for all I in Valid_Cursor_Range =>
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
        (C /= No_Element and then Contains (F.Nodes, C));

      -------------
      -- Element --
      -------------

      function Element (F : Forest; C : Cursor) return Element_Type is
        (Element (F.Nodes, C).Element);

      ------------
      -- Parent --
      ------------

      function Parent (F : Forest; C : Cursor) return Cursor is
        (Element (F.Nodes, C).Parent);

      -----------
      -- Child --
      -----------

      function Child
        (F : Forest;
         C : Cursor;
         W : Way_Type)
         return Cursor
      is
        (Element (F.Nodes, C).Ways (W));

      -------------
      -- Is_Root --
      -------------

      function Is_Root (F : Forest; C : Cursor) return Boolean is
        (Has_Element (F, C) and then Element (F.Nodes, C).Position = Top);

      --------------
      -- Position --
      --------------

      function Position (F : Forest; C : Cursor) return Way_Type is
        (Element (F.Nodes, C).Position);

      -----------
      -- Model --
      -----------

      function Model (F : Forest; Root : Cursor) return Model_Type is
         use I_Sets;

         type Boolean_Array is array (Valid_Cursor_Range) of Boolean;

         function Next (Todo : Boolean_Array) return Cursor with
           Post => (if Next'Result = No_Element
                    then (for all T of Todo => not T)
                    else Todo (Next'Result));

         function Next (Todo : Boolean_Array) return Cursor is
         begin
            for I in Todo'Range loop
               pragma Loop_Invariant (for all J in 1 .. I - 1 => not Todo (J));
               if Todo (I) then
                  return I;
               end if;
            end loop;

            return No_Element;
         end Next;

         Todo : Boolean_Array := [others => False];

         Unseen : I_Sets.Set := All_Indexes;

         M : Model_Type;
         I : Cursor := Root;

      begin
         --  Insert the root node in the todo list

         Todo (Root)      := True;
         M (Root).In_Tree := True;

         while I /= No_Element loop
            pragma Loop_Variant (Decreases => Length (Unseen));

            --  Node I is in the todo list and in the tree
            pragma Loop_Invariant (Todo (I) and then M (I).In_Tree);

            --  All nodes in the todo list are in the tree
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if Todo (J) then M (J).In_Tree));

            --  All nodes in the tree are in the forest
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if M (J).In_Tree then Contains (F.Nodes, J)));

            --  All nodes in the todo list are in the forest
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if Todo (J) then Contains (F.Nodes, J)));

            --  The path of a node in the todo list is maximal w.r.t. other
            --  nodes which are known to be in the tree at this stage.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if Todo (J) then
                    (for all K in Valid_Cursor_Range =>
                       (if M (K).In_Tree
                        then not (M (J).Path < M (K).Path)))));

            --  Children of nodes in the todo list are not yet in the tree
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if Todo (J) then
                    (for all W of Element (F.Nodes, J).Ways =>
                       (if W /= No_Element then
                          not M (W).In_Tree and then not Todo (W)))));

            --  The root is in tree with an empty path
            pragma Loop_Invariant
              (M (Root).In_Tree and then Length (M (Root).Path) = 0);

            --  Non-root nodes in the tree don't have position Top and have a
            --  parent.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if M (J).In_Tree and then J /= Root
                  then Element (F.Nodes, J).Position /= Top
                       and then Element (F.Nodes, J).Parent /= No_Element));

            --  Non-root nodes are in the tree iff their parent is in the tree
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if M (J).In_Tree and then J /= Root
                  then Element (F.Nodes, J).Parent /= No_Element
                       and then M (Element (F.Nodes, J).Parent).In_Tree));

            --  The path from the root to non-root tree nodes is equal to the
            --  path to their parent extended by the last direction to get to
            --  the node. For all other nodes, the path is empty.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if M (J).In_Tree and then J /= Root
                  then Is_Add (M (Element (F.Nodes, J).Parent).Path,
                               Element (F.Nodes, J).Position,
                               M (J).Path)));

            --  A node known to be in the tree is either the root node, or a
            --  node whose parent is known to be in the tree. In that case, the
            --  node is either known to be in the tree, or the parent is still
            --  in the todo list.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if J /= Root and then Contains (F.Nodes, J) then
                    (if Element (F.Nodes, J).Parent /= No_Element
                        and then M (Element (F.Nodes, J).Parent).In_Tree
                     then M (J).In_Tree
                          or else Todo (Element (F.Nodes, J).Parent)
                     else not M (J).In_Tree)));

            --  A node known to be in the tree does not have its parent in the
            --  Todo list.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if M (J).In_Tree and then J /= Root
                  then not Todo (Element (F.Nodes, J).Parent)));

            --  Nodes in the tree all have different associated paths
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if M (J).In_Tree then
                    (for all K in Valid_Cursor_Range =>
                       (if M (K).In_Tree and then M (K).Path = M (J).Path
                        then J = K))));

            --  The longest path stored so far is of size
            --  Max_Size - Length (Unseen) at most.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (Length (M (J).Path)
                  <= To_Big_Integer (Max_Size) - Length (Unseen)));

            --  Nodes that have not been handled yet are either not in the tree
            --  or in the todo list.
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (Contains (Unseen, J)
                  = (not M (J).In_Tree or else Todo (J))));

            --  Every node not in the tree has no path
            pragma Loop_Invariant
              (for all J in Valid_Cursor_Range =>
                 (if not M (J).In_Tree then Length (M (J).Path) = 0));

            Unseen := Remove (Unseen, I);

            --  Add each child to the tree and the todo list

            for W in Way_Type loop
               --  All children processed so far are in the tree and in the
               --  todo list, and its path extends its parent path by the
               --  way taken to the node.
               pragma Loop_Invariant
                 (for all V in Way_Type'First .. W - 1 =>
                    (if Element (F.Nodes, I).Ways (V) /= No_Element then
                       M (Element (F.Nodes, I).Ways (V)).In_Tree
                       and then Todo (Element (F.Nodes, I).Ways (V))
                       and then M (Element (F.Nodes, I).Ways (V)).Path
                                = Add (M (I).Path, V)));

               --  Nodes in the tree all have different associated paths
               pragma Loop_Invariant
                 (for all J in Valid_Cursor_Range =>
                    (if M (J).In_Tree then
                       (for all K in Valid_Cursor_Range =>
                          (if M (K).In_Tree and then M (K).Path = M (J).Path
                             then J = K))));

               --  All nodes that have not been seen as a child of I are
               --  unchanged in the tree and todo list.
               pragma Loop_Invariant
                 (for all J in Valid_Cursor_Range =>
                    (if (for all V in Way_Type'First .. W - 1 =>
                           Element (F.Nodes, I).Ways (V) /= J)
                     then M (J) = M'Loop_Entry (J)
                          and then Todo (J) = Todo'Loop_Entry (J)));

               declare
                  J : constant Cursor :=
                        Element (F.Nodes, I).Ways (W);
               begin
                  if J /= No_Element then
                     pragma Assert (Element (F.Nodes, J).Parent = I);
                     pragma Assert (not Is_Empty (Unseen));

                     Todo (J) := True;
                     M (J)    := (In_Tree => True,
                                  Path    => Add (M (I).Path, W));
                  end if;
               end;
            end loop;

            --  Nothing more to do for node I

            Todo (I) := False;
            I        := Next (Todo);
         end loop;

         return M;
      end Model;

      ------------------
      -- Is_Reachable --
      ------------------

      function Is_Reachable
        (F : Forest; R : Cursor; C : Cursor)
         return Boolean
      is
        (Model (F, R) (C).In_Tree);

      -----------
      -- Depth --
      -----------

      function Depth
        (F : Forest; R : Cursor; C : Cursor)
         return Ada.Containers.Count_Type
      is
        (Ada.Containers.Count_Type
           (To_Integer (Length (Model (F, R) (C).Path))));

      -----------------
      -- All_Indexes --
      -----------------

      function All_Indexes return I_Sets.Set is
         use I_Sets;
         S : I_Sets.Set;
      begin
         for I in Valid_Cursor_Range loop
            pragma Loop_Invariant (for all J in 1 .. I - 1 => Contains (S, J));
            pragma Loop_Invariant (for all J of S => J < I);
            pragma Loop_Invariant
              (Length (S)
               = To_Big_Integer (Integer (I - Valid_Cursor_Range'First)));

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
     (Position /= No_Element
      and then Contains (Container.Nodes, Position));

   -------------
   -- Element --
   -------------

   function Element
     (Container : Forest;
      Position  : Cursor)
      return Element_Type
   is
      E_Acc : constant access constant Node_Type :=
                  Constant_Reference (Container.Nodes, Position);
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
                Constant_Reference (Container.Nodes, Position);
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
                Constant_Reference (Container.Nodes, Position);
   begin
      return E_Acc.all.Parent;
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
                Constant_Reference (Container.Nodes, Position);
   begin
      return E_Acc.all.Ways (Way);
   end Child;

   function Constant_Reference
     (Container : Forest;
      Position  : Cursor) return not null access constant Element_Type
   is
     (Constant_Reference (Container.Nodes, Position).all.Element'Access);

   --------------------
   -- Tree_Structure --
   --------------------

   function Tree_Structure (F : Node_Maps.Map) return Boolean is
     (
      --  The parent of a node is either No_Element or references another,
      --  valid node.
      (for all I in Valid_Cursor_Range =>
         (if Contains (F, I) and then Element (F, I).Parent /= No_Element
          then Contains (F, Element (F, I).Parent)))

      --  Each way of a node is either No_Element or references another,
      --  valid node.
      and then
        (for all I in Valid_Cursor_Range =>
           (if Contains (F, I) then
              (for all W of Element (F, I).Ways =>
                 (if W /= No_Element then Contains (F, W)))))

      --  If a node has position Top then it has no parent, otherwise it
      --  has a valid parent
      and then
        (for all I in Valid_Cursor_Range =>
           (if Contains (F, I) then
              (if Element (F, I).Position = Top
               then Element (F, I).Parent = No_Element
               else Element (F, I).Parent /= No_Element
                    and then Contains (F, Element (F, I).Parent))))

      --  If a node is a child (has a position), then it is the child of its
      --  parent.
      and then
        (for all I in Valid_Cursor_Range =>
           (if Contains (F, I) and then Element (F, I).Position /= Top then
              Element (F, Element (F, I).Parent).Ways (Element (F, I).Position)
                = I))

      --  Every child of node I has I as its parent
      and then
        (for all I in Valid_Cursor_Range =>
           (if Contains (F, I) then
              (for all W in Way_Type =>
                 (if Element (F, I).Ways (W) /= No_Element then
                    Element (F, Element (F, I).Ways (W)).Position = W
                    and then Element (F, Element (F, I).Ways (W)).Parent
                             = I))))
     );

end SPARK_Multiway_Trees;