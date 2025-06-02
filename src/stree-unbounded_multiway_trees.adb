--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
package body Stree.Unbounded_Multiway_Trees with
  SPARK_Mode => Off
is

   package body Formal_Model is

      function All_Cursors (Container : Tree) return Cursor_Sets.Set is
         S : Cursor_Sets.Set;
      begin
         for I in 1 .. Node_Vectors.Last_Index (Container.Nodes) loop
            S := Add (S, Cursor'(Node => I));
         end loop;

         return S;
      end All_Cursors;

      function Model (Container : Tree) return Model_Type is
         use Node_Vectors;

         Todo : Cursor_Sets.Set;
         C    : Cursor;
         N    : Node_Type;

         M : Model_Type :=
           [others => (Path     => Way_Sequences.Empty_Sequence,
                       Parent   => No_Element,
                       Way      => Way_Type'First,
                       Children => [others => No_Element],
                       In_Tree  => False)];

      begin
         if not Is_Empty (Container) then
            Todo := [Container.Root];

            M (Container.Root.Node) :=
              (Path     => Way_Sequences.Empty_Sequence,
               Parent   => No_Element,
               Way      => Way_Type'First,
               In_Tree  => True,
               Children =>
                 Element (Container.Nodes, Root (Container).Node).Ways);

            while not Is_Empty (Todo) loop
               C    := Choose (Todo);
               Todo := Remove (Todo, C);
               N    := Element (Container.Nodes, C.Node);

               for W in Way_Type loop
                  if N.Ways (W) /= No_Element then
                     Todo := Add (Todo, N.Ways (W));

                     M (N.Ways (W).Node) :=
                       (Path     => Add (M (C.Node).Path, W),
                        Parent   => C,
                        Way      => W,
                        Children => Element (Container.Nodes, N.Ways (W).Node)
                                      .Ways,
                        In_Tree  => True);
                  end if;
               end loop;
            end loop;
         end if;

         return M;
      end Model;

   end Formal_Model;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree is
     ((Nodes => Node_Vectors.Empty_Vector,
       Root  => No_Element));

   ------------
   -- Length --
   ------------

   function Length (Container : Tree) return Count_Type is
     (Node_Vectors.Length (Container.Nodes));

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Tree) return Boolean is
     (Container.Root = No_Element);

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   is
     (Node_Vectors.Has_Element (Container.Nodes, Position.Node));

   -------------
   -- Element --
   -------------

   function Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   is
     (Node_Vectors.Element (Container.Nodes, Position.Node).Element);

   -------------
   -- Is_Root --
   -------------

   function Is_Root
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   is
     (not Is_Empty (Container)
      and then Position = Container.Root);

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   is
   begin
      if not Has_Element (Container, Position) then
         return False;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);
      begin
         return (for all C of Node_Acc.all.Ways => C = No_Element);
      end;
   end Is_Leaf;

   ----------
   -- Root --
   ----------

   function Root (Container : Tree) return Cursor is (Container.Root);

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Tree) return Element_Type is
      Node : constant Cursor := First (Container);
   begin
      if Node = No_Element then
         raise Constraint_Error with "Tree is empty";
      end if;

      return Element (Container, Node);
   end First_Element;

   ---------------------
   -- Last_In_Subtree --
   ---------------------

   function Last_In_Subtree
     (Container    : Tree;
      Subtree_Root : Cursor)
      return Cursor
   is
      Node : Cursor := Subtree_Root;
      Next : Cursor;
   begin
      while Node /= No_Element loop
         declare
            Node_Acc : constant not null access constant Node_Type :=
                         Node_Vectors.Constant_Reference
                           (Container.Nodes, Node.Node);
         begin
            --  Find the last child
            for W in reverse Way_Type loop
               Next := Node_Acc.all.Ways (W);
               exit when Next /= No_Element;
            end loop;
         end;

         if Next = No_Element then
            return Node;
         end if;

         Node := Next;
      end loop;

      return Node;
   end Last_In_Subtree;

   ----------
   -- Last --
   ----------

   function Last (Container : Tree) return Cursor is
   begin
      return Last_In_Subtree (Container, Root (Container));
   end Last;

   ----------
   -- Next --
   ----------

   function Next
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
      Node : Cursor;
      Dir  : Way_Type;

   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      end if;

      declare
         Pos_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);

      begin
         --  Find the first child

         for W in Way_Type loop
            Node := Pos_Acc.all.Ways (W);

            if Node /= No_Element then
               return Node;
            end if;
         end loop;

         --  No child was found. Try and find the next sibling (if the node
         --  is not the root).

         if Pos_Acc.all.Parent = No_Element then
            return No_Element;
         end if;

         declare
            Parent_Acc : constant not null access constant Node_Type :=
                           Node_Vectors.Constant_Reference
                             (Container.Nodes, Pos_Acc.all.Parent.Node);
         begin
            if Pos_Acc.all.Position /= Way_Type'Last then
               for W in Way_Type'Succ (Pos_Acc.all.Position) .. Way_Type'Last
               loop
                  Node := Parent_Acc.all.Ways (W);

                  if Node /= No_Element then
                     return Node;
                  end if;
               end loop;
            end if;
         end;

         --  No sibling was found. Walk up the tree to find the next sibling
         --  of an ancestor.

         Dir  := Direction (Container, Position);
         Node := Parent (Container, Position);

         while Node /= No_Element loop
            declare
               Node_Acc : constant not null access constant Node_Type :=
                           Node_Vectors.Constant_Reference
                             (Container.Nodes, Node.Node);
            begin
               if Dir /= Way_Type'Last then
                  for W in Way_Type'Succ (Dir) .. Way_Type'Last loop
                     if Node_Acc.all.Ways (W) /= No_Element then
                        return Node_Acc.all.Ways (W);
                     end if;
                  end loop;
               end if;

               Dir  := Node_Acc.all.Position;
               Node := Node_Acc.all.Parent;
            end;
         end loop;

         return No_Element;
      end;
   end Next;

   ----------
   -- Prev --
   ----------

   function Prev
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
      Node : Cursor;

   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      end if;

      declare
         Pos_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);

      begin
         --  Try and find the previous sibling (if the node
         --  is not the root).

         if Pos_Acc.all.Parent = No_Element then
            return No_Element;
         end if;

         declare
            Parent_Acc : constant not null access constant Node_Type :=
                           Node_Vectors.Constant_Reference
                             (Container.Nodes, Pos_Acc.all.Parent.Node);
         begin
            if Pos_Acc.all.Position /= Way_Type'First then
               for W in reverse Way_Type'First ..
                                Way_Type'Pred (Pos_Acc.all.Position)
               loop
                  Node := Parent_Acc.all.Ways (W);

                  if Node /= No_Element then
                     return Last_In_Subtree (Container, Node);
                  end if;
               end loop;
            end if;
         end;

         --  No previous siblings, so return the parent.

         return Parent (Container, Position);
      end;
   end Prev;

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);
      begin
         for C of Node_Acc.all.Ways loop
            if C /= No_Element then
               return C;
            end if;
         end loop;
      end;

      return No_Element;
   end First_Child;

   -------------------------
   -- First_Child_Element --
   -------------------------

   function First_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   is
      Node : constant Cursor := First_Child (Container, Position);
   begin
      if Node = No_Element then
         raise Constraint_Error with "Node has no children";
      end if;

      return Element (Container, Node);
   end First_Child_Element;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);
      begin
         for C of reverse Node_Acc.all.Ways loop
            if C /= No_Element then
               return C;
            end if;
         end loop;
      end;

      return No_Element;
   end Last_Child;

   ------------------------
   -- Last_Child_Element --
   ------------------------

   function Last_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   is
      Node : constant Cursor := Last_Child (Container, Position);
   begin
      if Node = No_Element then
         raise Constraint_Error with "Node has no children";
      end if;

      return Element (Container, Node);
   end Last_Child_Element;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);
      begin
         if Node_Acc.all.Parent = No_Element
            or else Node_Acc.all.Position = Way_Type'Last
         then
            return No_Element;
         end if;

         declare
            Parent_Acc : constant not null access constant Node_Type :=
                           Node_Vectors.Constant_Reference
                             (Container.Nodes, Node_Acc.all.Parent.Node);
         begin
            for W in Way_Type'Succ (Node_Acc.all.Position) .. Way_Type'Last
            loop
               if Parent_Acc.all.Ways (W) /= No_Element then
                  return Parent_Acc.all.Ways (W);
               end if;
            end loop;
         end;
      end;

      return No_Element;
   end Next_Sibling;

   ------------------
   -- Prev_Sibling --
   ------------------

   function Prev_Sibling
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);
      begin
         if Node_Acc.all.Parent = No_Element
            or else Node_Acc.all.Position = Way_Type'First
         then
            return No_Element;
         end if;

         declare
            Parent_Acc : constant not null access constant Node_Type :=
                           Node_Vectors.Constant_Reference
                             (Container.Nodes, Node_Acc.all.Parent.Node);
         begin
            for W in reverse Way_Type'First ..
                             Way_Type'Pred (Node_Acc.all.Position)
            loop
               if Parent_Acc.all.Ways (W) /= No_Element then
                  return Parent_Acc.all.Ways (W);
               end if;
            end loop;
         end;
      end;

      return No_Element;
   end Prev_Sibling;

   ------------------
   -- Root_Element --
   ------------------

   function Root_Element (Container : Tree) return Element_Type is
   begin
      if Is_Empty (Container) then
         raise Constraint_Error with "Invalid root";
      end if;

      return Element (Container, Root (Container));
   end Root_Element;

   ------------
   -- Parent --
   ------------

   function Parent
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      else
         declare
            Node_Acc : constant not null access constant Node_Type :=
                        Node_Vectors.Constant_Reference
                           (Container.Nodes, Position.Node);
         begin
            return Node_Acc.all.Parent;
         end;
      end if;
   end Parent;

   -----------
   -- Child --
   -----------

   function Child
     (Container : Tree;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   is
   begin
      if not Has_Element (Container, Position) then
         return No_Element;
      else
         declare
            Node_Acc : constant not null access constant Node_Type :=
                        Node_Vectors.Constant_Reference
                           (Container.Nodes, Position.Node);
         begin
            return Node_Acc.all.Ways (Way);
         end;
      end if;
   end Child;

   ---------------
   -- Direction --
   ---------------

   function Direction
     (Container : Tree;
      Position  : Cursor)
      return Way_Type
   is
   begin
      if not Has_Element (Container, Position) then
         return Way_Type'First;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node);
      begin
         return Node_Acc.all.Position;
      end;
   end Direction;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor
     (Container : Tree;
      Ancestor  : Cursor;
      Child     : Cursor)
      return Boolean
   is
      Node : Cursor := Parent (Container, Child);
   begin
      while Node /= No_Element and then Node /= Ancestor loop
         Node := Parent (Container, Node);
      end loop;

      return Node /= No_Element;
   end Is_Ancestor;

   -----------
   -- Depth --
   -----------

   function Depth
     (Container : Tree;
      Position  : Cursor)
      return Count_Type
   is
      Node  : Cursor;
      Count : Count_Type := 0;
   begin
      if Has_Element (Container, Position) then
         Node := Parent (Container, Position);

         while Node /= No_Element loop
            Count := Count + 1;
            Node  := Parent (Container, Node);
         end loop;
      end if;

      return Count;
   end Depth;

   -----------------
   -- Insert_Root --
   -----------------

   procedure Insert_Root
     (Container : in out Tree;
      New_Item  :        Element_Type)
   is
   begin
      Container.Nodes := [(Element  => New_Item,
                           Parent   => No_Element,
                           Position => Way_Type'First,
                           Ways     => [others => No_Element])];

      Container.Root :=
        Cursor'(Node => Node_Vectors.First_Index (Container.Nodes));
   end Insert_Root;

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Container : in out Tree;
      New_Item  :        Element_Type;
      Position  :        Cursor;
      Way       :        Way_Type)
   is
   begin
      Node_Vectors.Append
        (Container => Container.Nodes,
         New_Item  => Node_Type'(Element  => New_Item,
                                 Parent   => Position,
                                 Position => Way,
                                 Ways     => [others => No_Element]));

      declare
         Parent_Acc : constant not null access Node_Type :=
                        Node_Vectors.Reference
                          (Container.Nodes'Access, Position.Node);
      begin
         Parent_Acc.all.Ways (Way) :=
           Cursor'(Node => Node_Vectors.Last_Index (Container.Nodes));
      end;
   end Insert_Child;

   -------------------
   -- Insert_Parent --
   -------------------

   procedure Insert_Parent
     (Container : in out Tree;
      New_Item  :        Element_Type;
      Position  :        Cursor;
      Way       :        Way_Type)
   is
      Parent_Pos : constant Cursor := Parent (Container, Position);
      New_Node   : Cursor;
   begin
      --  Create the new node

      Node_Vectors.Append
        (Container => Container.Nodes,
         New_Item  => Node_Type'
                        (Element  => New_Item,
                         Parent   => Parent_Pos,
                         Position => Direction (Container, Parent_Pos),
                         Ways     => [others => No_Element]));

      New_Node := Cursor'(Node => Node_Vectors.Last_Index (Container.Nodes));

      --  Add the node at Position as a child of the new node

      declare
         New_Node_Acc : constant not null access Node_Type :=
                          Node_Vectors.Reference
                            (Container.Nodes'Access, New_Node.Node);
      begin
         New_Node_Acc.Ways (Way) := Position;
      end;

      --  Replace Position with New_Node as a child of the parent

      declare
         Parent_Acc : constant not null access Node_Type :=
                        Node_Vectors.Reference
                          (Container.Nodes'Access, Parent_Pos.Node);
      begin
         for W of Parent_Acc.all.Ways loop
            if W = Position then
               W := Cursor'(Node => Node_Vectors.Last_Index (Container.Nodes));
            end if;
         end loop;
      end;

      --  Set New_Node as the parent of Position

      declare
         Position_Acc : constant not null access Node_Type :=
                        Node_Vectors.Reference
                          (Container.Nodes'Access, Position.Node);
      begin
         Position_Acc.all.Parent := New_Node;
      end;

      if Parent_Pos = No_Element then
         Container.Root := New_Node;
      end if;
   end Insert_Parent;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Tree;
      Position  : Cursor)
      return not null access constant Element_Type
   is
   begin
      return Node_Vectors.Constant_Reference
               (Container.Nodes, Position.Node)
               .all.Element'Access;
   end Constant_Reference;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : not null access Tree;
      Position  : Cursor)
      return not null access Element_Type
   is
   begin
      return Node_Vectors.Reference
               (Container.all.Nodes'Access, Position.Node)
               .all.Element'Access;
   end Reference;

end Stree.Unbounded_Multiway_Trees;
