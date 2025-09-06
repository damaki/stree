--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
package body SPARK.Containers.Formal.Unbounded_Multiway_Trees with
  SPARK_Mode => Off
is

   function Get_Path
     (Container : Node_Vectors.Vector;
      Node      : Index_Type) return M.Path_Type
   with
     Ghost;
   --  Get the path to a node

   function Find_Node
     (Container : Tree;
      Path      : M.Path_Type) return Cursor
   with
     Ghost;
   --  Find the node in the tree with the specified path

   function Last_Node_In_Subtree
     (Container : Tree;
      Position  : Cursor) return Cursor;
   --  Get the last node that will be visited in the subtree when iterating

   function Same_Path
     (Left, Right : Tree;
      L_Position : Cursor;
      R_Position : Cursor) return Boolean
   with
     Pre => Has_Element (Left, L_Position)
            and then Has_Element (Right, R_Position);
   --  Returns True if the path to nodes L_Position and R_Position are the same
   --  in trees Left and Right respectively.

   procedure Alloc_From_Free_List
     (Container : in out Tree;
      Node      :    out Cursor);
   --  Try and allocate a node from the free list.
   --
   --  If the free list is empty, then No_Element is returned.

   procedure Add_To_Free_List_Recursive
     (Container : in out Tree;
      Node      :        Cursor);
   --  Add a node (and its children) to the free list

   --  The following _Impl subprograms are versions of the subprograms
   --  without contracts so that they can be safely called from any subprogram
   --  without the risk of introducing accidental unbounded recursion via
   --  subprogram calls in contracts.
   --
   --  For example, the function Model can't call function Next in its
   --  implementation, since Next calls Model in its postcondition (indirectly
   --  via function Positions). Model can, however, call Next_Impl since that
   --  function doesn't cause recursion.

   function Has_Element_Impl
     (Container : Tree;
      Position  : Cursor) return Boolean;
   --  Implementation of Has_Element but without contracts

   function First_Impl (Container : Tree) return Cursor;
   --  Implementation of First but without contracts

   function Parent_Impl
     (Container : Tree;
      Position  : Cursor) return Cursor;
   --  Implementation of Parent but without contracts

   function Next_Impl
     (Container : Tree;
      Position  : Cursor) return Cursor;
   --  Implementation of Next but without contracts

   function First_Child_Impl
     (Container : Tree;
      Position  : Cursor) return Cursor;
   --  Implementation of First_Child but without contracts

   function Next_Sibling_Impl
     (Container : Tree;
      Position  : Cursor) return Cursor
   with
     Pre => Has_Element_Impl (Container, Position);
   --  Implementation of Next_Sibling but without contracts

   ------------------
   -- Formal_Model --
   ------------------

   package body Formal_Model is

      -------------------------
      -- Element_Logic_Equal --
      -------------------------

      function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
      is (Left = Right);

      -----------------------
      -- Mapping_Preserved --
      -----------------------

      function Mapping_Preserved (Left, Right : Tree) return Boolean is
         L, R : Cursor;
      begin
         for I in 1 .. Node_Vectors.Last_Index (Left.Nodes) loop
            L := Cursor'(Node => I);
            R := L;

            --  Skip over freed nodes

            if Has_Element_Impl (Left, L) then

               --  Check that the same cursor exists in Right.

               if not Has_Element_Impl (Right, R) then
                  return False;
               end if;

               --  Check that both nodes have the same path

               if not Same_Path (Left, Right, L, R) then
                  return False;
               end if;
            end if;
         end loop;

         return True;
      end Mapping_Preserved;

      --------------------------------------
      -- Mapping_Preserved_Except_Subtree --
      --------------------------------------

      function Mapping_Preserved_Except_Subtree
        (Left, Right : Tree;
         Position    : Cursor) return Boolean
      is
         L, R : Cursor;
      begin
         for I in 1 .. Node_Vectors.Last_Index (Left.Nodes) loop
            L := Cursor'(Node => I);
            R := L;

            --  Skip over freed nodes, and nodes in the specified subtree

            if Has_Element_Impl (Left, L)
               and then not In_Subtree (Left, Position, L)
            then

               --  Check that the same cursor exists in Right.

               if not Has_Element_Impl (Right, R) then
                  return False;
               end if;

               --  Check that both nodes have the same path

               if not Same_Path (Left, Right, L, R) then
                  return False;
               end if;
            end if;
         end loop;

         return True;
      end Mapping_Preserved_Except_Subtree;

      -------------------------
      -- Same_Mapping_Except --
      -------------------------

      function Same_Mapping_Except
        (Left, Right : Tree;
         Position    : Cursor) return Boolean
      is
         L, R : Cursor;
      begin
         for I in 1 .. Node_Vectors.Last_Index (Left.Nodes) loop
            L := Cursor'(Node => I);
            R := L;

            --  Skip over freed nodes, and the specified node

            if L /= Position and then Has_Element_Impl (Left, L) then

               --  Check that the same cursor exists in Right.

               if not Has_Element_Impl (Right, R) then
                  return False;
               end if;

               --  Check that both nodes have the same path

               if not Same_Path (Left, Right, L, R) then
                  return False;
               end if;
            end if;
         end loop;

         --  Check for any other nodes in Right that are not in Left

         if Node_Vectors.Last_Index (Left.Nodes)
            < Node_Vectors.Last_Index (Right.Nodes)
         then
            for I in Node_Vectors.Last_Index (Left.Nodes) + 1 ..
                     Node_Vectors.Last_Index (Right.Nodes)
            loop
               if I /= Position.Node
                  and then Has_Element_Impl (Right, Cursor'(Node => I))
               then
                  return False;
               end if;
            end loop;
         end if;

         return True;
      end Same_Mapping_Except;

      ---------------------------------
      -- Same_Mapping_Except_Subtree --
      ---------------------------------

      function Same_Mapping_Except_Subtree
        (Left, Right : Tree;
         Position    : Cursor) return Boolean
      is
         L, R : Cursor;
      begin
         for I in 1 .. Node_Vectors.Last_Index (Left.Nodes) loop
            L := Cursor'(Node => I);
            R := L;

            --  Skip over freed nodes, and nodes in the specified subtree

            if Has_Element_Impl (Left, L)
               and then not In_Subtree (Left, Position, L)
            then

               --  Check that the same cursor exists in Right.

               if not Has_Element_Impl (Right, R) then
                  return False;
               end if;

               --  Check that both nodes have the same path

               if not Same_Path (Left, Right, L, R) then
                  return False;
               end if;
            end if;
         end loop;

         --  Check for any other nodes in Right that are not in Left and not
         --  in the specified subtree.

         if Node_Vectors.Last_Index (Left.Nodes)
            < Node_Vectors.Last_Index (Right.Nodes)
         then
            for I in Node_Vectors.Last_Index (Left.Nodes) + 1 ..
                     Node_Vectors.Last_Index (Right.Nodes)
            loop
               R := Cursor'(Node => I);
               if Has_Element_Impl (Right, R)
                  and then not In_Subtree (Right, Position, R)
               then
                  return False;
               end if;
            end loop;
         end if;

         return True;
      end Same_Mapping_Except_Subtree;

      -----------------------------
      -- Subtree_Mapping_Shifted --
      -----------------------------

      function Subtree_Mapping_Shifted
        (Left, Right  : Tree;
         Subtree_Root : M.Path_Type;
         Way          : Way_Type) return Boolean
      is
        (Subtree_Remapped
          (Left        => Left,
           Right       => Right,
           Old_Subtree => Subtree_Root,
           New_Subtree => M.Child (Subtree_Root, Way)));

      ----------------------
      -- Subtree_Remapped --
      ----------------------

      function Subtree_Remapped
        (Left, Right : Tree;
         Old_Subtree : M.Path_Type;
         New_Subtree : M.Path_Type) return Boolean
      is
         L_Node : Cursor;
         R_Node : Cursor;

         L_Last : Cursor;
         R_Last : Cursor;
      begin
         L_Node := Find_Node (Left,  Old_Subtree);
         R_Node := Find_Node (Right, New_Subtree);

         if L_Node = No_Element or else R_Node = No_Element then
            return (L_Node = No_Element) and then (R_Node = No_Element);
         elsif L_Node /= R_Node then
            return False;
         end if;

         --  Iterate through each node in the subtree and verify that the
         --  cursors are the same at each step, and they have the same path
         --  from their parent.

         L_Last := Last_Node_In_Subtree (Left,  L_Node);
         R_Last := Last_Node_In_Subtree (Right, R_Node);

         if (L_Node /= L_Last) and then (R_Node /= R_Last) then

            --  Don't test the first node, as it is the subtree root.

            L_Node := Next (Left,  L_Node);
            R_Node := Next (Right, R_Node);

            loop
               if L_Node /= R_Node
                  or else
                    Direction (Left, L_Node) /= Direction (Right, R_Node)
                  or else
                    Parent_Impl (Left, L_Node) /= Parent_Impl (Right, R_Node)
               then
                  return False;
               end if;

               exit when L_Node = L_Last or else R_Node = R_Last;

               L_Node := Next (Left,  L_Node);
               R_Node := Next (Right, R_Node);
            end loop;
         end if;

         return (L_Node = L_Last) and then (R_Node = R_Last);
      end Subtree_Remapped;

      ------------------------
      -- Ancestry_Preserved --
      ------------------------

      function Ancestry_Preserved (Left, Right : Tree) return Boolean is
         C1 : Cursor;
         C2 : Cursor;

      begin
         --  Check that every cursor in Left is in Right
         C1 := Left.Root;
         while C1 /= No_Element loop
            if not Has_Element_Impl (Right, C1) then
               return False;
            end if;
            C1 := Next_Impl (Left, C1);
         end loop;

         --  Check ancestry of every combination of nodes. There's probably a
         --  more efficient way to check the property, but since this is a
         --  Ghost function it's unlikely this will be used outside of testing.

         C1 := Left.Root;

         while C1 /= No_Element loop
            C2 := Left.Root;

            while C2 /= No_Element loop
               if Is_Ancestor (Left, C1, C2) then
                  if not Is_Ancestor (Right, C1, C2) then
                     return False;
                  end if;
               end if;

               C2 := Next_Impl (Left, C2);
            end loop;

            C1 := Next_Impl (Left, C1);
         end loop;

         return True;
      end Ancestry_Preserved;

      -----------
      -- Paths --
      -----------

      function Paths (Container : Tree) return P.Map is
         Result : P.Map := P.Empty_Map;
      begin
         for I in 1 .. Node_Vectors.Length (Container.Nodes) loop
            if not Node_Vectors.Constant_Reference (Container.Nodes, I).Free
            then
               Result := P.Add (Result,
                                Cursor'(Node => I),
                                Get_Path (Container.Nodes, I));
            end if;
         end loop;

         return Result;
      end Paths;

      -----------
      -- Model --
      -----------

      function Model (Container : Tree) return M.Tree is
         Result : M.Tree := M.Empty_Tree;
         Node   : Cursor := Container.Root;
      begin
         while Node /= No_Element loop
            declare
               Node_Acc : constant access constant Node_Type :=
                            Node_Vectors.Constant_Reference
                              (Container.Nodes, Node.Node).Element;
            begin
               Result :=
                 M.Add (Container => Result,
                        New_Item  => Node_Acc.all.Element,
                        New_Node  => Get_Path (Container.Nodes, Node.Node));
            end;

            Node := Next_Impl (Container, Node);
         end loop;

         return Result;
      end Model;

      ------------
      -- M_Path --
      ------------

      function M_Path
        (Container : Tree;
         Position  : Cursor) return M.Path_Type
      is
      begin
         if not Has_Element_Impl (Container, Position) then
            raise Constraint_Error;
         end if;

         return Get_Path (Container.Nodes, Position.Node);
      end M_Path;

      ---------------
      -- Positions --
      ---------------

      function Positions (Container : Tree) return S.Map is
         Result : S.Map               := S.Empty_Map;
         Node   : Cursor              := Container.Root;
         I      : Positive_Count_Type := 1;

      begin
         while Node /= No_Element loop
            Result := S.Add (Result, Node, I);
            I      := I + 1;
            Node   := Next_Impl (Container, Node);
         end loop;

         return Result;
      end Positions;

   end Formal_Model;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree is
     ((Nodes     => Node_Vectors.Empty_Vector,
       Root      => No_Element,
       Free_List => No_Element,
       Length    => 0));

   ------------
   -- Length --
   ------------

   function Length (Container : Tree) return Count_Type is
     (Container.Length);

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
     (Has_Element_Impl (Container, Position));

   ----------------------
   -- Has_Element_Impl --
   ----------------------

   function Has_Element_Impl
     (Container : Tree;
      Position  : Cursor) return Boolean
   is
   begin
      if Position.Node not in 1 .. Node_Vectors.Last_Index (Container.Nodes)
      then
         return False;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node)
                        .Element;
      begin
         return not Node_Acc.all.Free;
      end;
   end Has_Element_Impl;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   is
     (Node_Vectors.Element (Container.Nodes, Position.Node).Element);

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Tree;
      Position  :        Cursor;
      New_Item  :        Element_Type)
   is
   begin
      if not Has_Element_Impl (Container, Position) then
         raise Constraint_Error with "Invalid cursor";
      end if;

      declare
         Node_Acc : constant not null access Node_Type :=
                      Node_Vectors.Reference
                        (Container.Nodes, Position.Node)
                        .Element;
      begin
         Node_Acc.all.Element := New_Item;
      end;
   end Replace_Element;

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
      if not Has_Element_Impl (Container, Position) then
         return False;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node)
                        .Element;
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

   ----------
   -- Last --
   ----------

   function Last (Container : Tree) return Cursor is
     (Last_Node_In_Subtree (Container, Container.Root));

   ----------
   -- Next --
   ----------

   function Next
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
     (Next_Impl (Container, Position));

   -----------------
   -- First_Child --
   -----------------

   function First_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
     (First_Child_Impl (Container, Position));

   ----------------------
   -- First_Child_Impl --
   ----------------------

   function First_Child_Impl
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node)
                        .Element;
      begin
         for C of Node_Acc.all.Ways loop
            if C /= No_Element then
               return C;
            end if;
         end loop;
      end;

      return No_Element;
   end First_Child_Impl;

   -------------------------
   -- First_Child_Element --
   -------------------------

   function First_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   is
      Node : constant Cursor := First_Child_Impl (Container, Position);
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
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node)
                        .Element;
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
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      end if;

      return Next_Sibling_Impl (Container, Position);
   end Next_Sibling;

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
     (Parent_Impl (Container, Position));

   -----------------
   -- Parent_Impl --
   -----------------

   function Parent_Impl
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   is
   begin
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      else
         declare
            Node_Acc : constant not null access constant Node_Type :=
                        Node_Vectors.Constant_Reference
                           (Container.Nodes, Position.Node)
                           .Element;
         begin
            return Node_Acc.all.Parent;
         end;
      end if;
   end Parent_Impl;

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
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      else
         declare
            Node_Acc : constant not null access constant Node_Type :=
                         Node_Vectors.Constant_Reference
                           (Container.Nodes, Position.Node)
                           .Element;
         begin
            return Node_Acc.all.Ways (Way);
         end;
      end if;
   end Child;

   -------------
   -- Sibling --
   -------------

   function Sibling
     (Container : Tree;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   is
   begin
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      else
         declare
            Node_Acc : constant not null access constant Node_Type :=
                         Node_Vectors.Constant_Reference
                           (Container.Nodes, Position.Node)
                           .Element;
         begin
            if Node_Acc.all.Parent = No_Element then
               return No_Element;
            end if;

            declare
               Parent_Acc : constant not null access constant Node_Type :=
                              Node_Vectors.Constant_Reference
                                (Container.Nodes, Node_Acc.all.Parent.Node)
                                .Element;
            begin
               return Parent_Acc.all.Ways (Way);
            end;
         end;
      end if;
   end Sibling;

   ---------------
   -- Direction --
   ---------------

   function Direction
     (Container : Tree;
      Position  : Cursor)
      return Way_Type
   is
   begin
      if not Has_Element_Impl (Container, Position) then
         return Way_Type'First;
      end if;

      declare
         Node_Acc : constant not null access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node)
                        .Element;
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
      Node : Cursor := Parent_Impl (Container, Child);
   begin
      while Node /= No_Element and then Node /= Ancestor loop
         Node := Parent_Impl (Container, Node);
      end loop;

      return Node /= No_Element;
   end Is_Ancestor;

   ----------------
   -- In_Subtree --
   ----------------

   function In_Subtree
     (Container    : Tree;
      Subtree_Root : Cursor;
      Position     : Cursor)
      return Boolean
   is
     (Has_Element_Impl (Container, Subtree_Root)
      and then Has_Element_Impl (Container, Position)
      and then (Position = Subtree_Root
                or else Is_Ancestor (Container, Subtree_Root, Position)));

   ---------------
   -- In_Branch --
   ---------------

   function In_Branch
     (Container    : Tree;
      Ancestor     : Cursor;
      Position     : Cursor;
      Way          : Way_Type)
      return Boolean
   is
      C : Cursor;
   begin
      if not Has_Element_Impl (Container, Ancestor)
         or else not Has_Element_Impl (Container, Position)
      then
         return False;
      end if;

      C := Child (Container, Ancestor, Way);

      return Position = C or else Is_Ancestor (Container, C, Position);
   end In_Branch;

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
      if Has_Element_Impl (Container, Position) then
         Node := Parent_Impl (Container, Position);

         while Node /= No_Element loop
            Count := Count + 1;
            Node  := Parent_Impl (Container, Node);
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
                           Ways     => [others => No_Element],
                           Free     => False)];

      Container.Root :=
        Cursor'(Node => Node_Vectors.First_Index (Container.Nodes));

      Container.Length := 1;
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
      Node : Cursor;
   begin
      Alloc_From_Free_List (Container, Node);

      if Node /= No_Element then
         declare
            Node_Acc : constant not null access Node_Type :=
                         Node_Vectors.Reference
                           (Container.Nodes, Node.Node)
                           .Element;
         begin
            Node_Acc.all := Node_Type'(Element  => New_Item,
                                       Parent   => Position,
                                       Position => Way,
                                       Ways     => [others => No_Element],
                                       Free     => False);
         end;
      else
         --  Free list is empty. Create a new node.

         Node_Vectors.Append
           (Container => Container.Nodes,
            New_Item  => Node_Type'(Element  => New_Item,
                                    Parent   => Position,
                                    Position => Way,
                                    Ways     => [others => No_Element],
                                    Free     => False));
      end if;

      Container.Length := Container.Length + 1;

      declare
         Parent_Acc : constant not null access Node_Type :=
                        Node_Vectors.Reference
                          (Container.Nodes, Position.Node)
                          .Element;
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
      Parent_Pos : constant Cursor := Parent_Impl (Container, Position);
      New_Node   : Cursor;
   begin
      --  Create the new node

      Alloc_From_Free_List (Container, New_Node);

      if New_Node /= No_Element then
         declare
            Node_Acc : constant not null access Node_Type :=
                         Node_Vectors.Reference
                           (Container.Nodes, New_Node.Node)
                          .Element;
         begin
            Node_Acc.all := Node_Type'
                              (Element  => New_Item,
                               Parent   => Parent_Pos,
                               Position => Direction (Container, Parent_Pos),
                               Ways     => [others => No_Element],
                               Free     => False);
         end;
      else
         --  Free list is empty. Create a new node.

         Node_Vectors.Append
           (Container => Container.Nodes,
            New_Item  => Node_Type'
                           (Element  => New_Item,
                            Parent   => Parent_Pos,
                            Position => Direction (Container, Parent_Pos),
                            Ways     => [others => No_Element],
                            Free     => False));

         New_Node :=
           Cursor'(Node => Node_Vectors.Last_Index (Container.Nodes));
      end if;

      Container.Length := Container.Length + 1;

      --  Add the node at Position as a child of the new node

      declare
         New_Node_Acc : constant not null access Node_Type :=
                          Node_Vectors.Reference
                            (Container.Nodes, New_Node.Node)
                            .Element;
      begin
         New_Node_Acc.Ways (Way) := Position;
      end;

      --  Replace Position with New_Node as a child of the parent

      declare
         Parent_Acc : constant not null access Node_Type :=
                        Node_Vectors.Reference
                          (Container.Nodes, Parent_Pos.Node)
                          .Element;
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
                          (Container.Nodes, Position.Node)
                          .Element;
      begin
         Position_Acc.all.Parent := New_Node;
      end;

      if Parent_Pos = No_Element then
         Container.Root := New_Node;
      end if;
   end Insert_Parent;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Container : in out Tree;
      Position  :        Cursor)
   is
   begin
      if not Has_Element_Impl (Container, Position) then
         raise Constraint_Error with "Invalid cursor";
      end if;

      if Position = Root (Container) then
         Container := Empty_Tree;
      else
         --  Remove the node from its parent

         declare
            Node_Acc   : constant not null access constant Node_Type :=
                           Node_Vectors.Constant_Reference
                             (Container.Nodes, Position.Node)
                             .Element;

            Parent_Acc : constant not null access Node_Type :=
                           Node_Vectors.Reference
                             (Container.Nodes,
                              Node_Acc.all.Parent.Node)
                             .Element;
         begin
            Parent_Acc.all.Ways (Node_Acc.all.Position) := No_Element;
         end;

         --  Delete the node and its children

         Add_To_Free_List_Recursive (Container, Position);
      end if;
   end Delete;

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
               .Element.all.Element'Access;
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
               (Container.all.Nodes, Position.Node)
               .Element.all.Element'Access;
   end Reference;

   --------------------------
   -- Alloc_From_Free_List --
   --------------------------

   procedure Alloc_From_Free_List
     (Container : in out Tree;
      Node      :    out Cursor)
   is
   begin
      Node := Container.Free_List;

      if Node /= No_Element then
         declare
            Node_Acc : constant not null access Node_Type :=
                         Node_Vectors.Reference
                           (Container.Nodes, Node.Node)
                           .Element;
         begin
            Node_Acc.all.Free   := False;
            Container.Free_List := Node_Acc.all.Ways (Way_Type'First);
         end;
      end if;
   end Alloc_From_Free_List;

   --------------------------------
   -- Add_To_Free_List_Recursive --
   --------------------------------

   procedure Add_To_Free_List_Recursive
     (Container : in out Tree;
      Node      :        Cursor)
   is
      Node_Acc : constant not null access Node_Type :=
                   Node_Vectors.Reference (Container.Nodes, Node.Node).Element;
   begin
      --  Delete all child nodes

      for C of Node_Acc.all.Ways loop
         if C /= No_Element then
            Add_To_Free_List_Recursive (Container, C);
         end if;
      end loop;

      --  Mark as free

      Node_Acc.all.Free := True;

      --  Add this node to the free list

      Node_Acc.all.Ways (Way_Type'First) := Container.Free_List;
      Container.Free_List                := Node;
      Container.Length                   := Container.Length - 1;
   end Add_To_Free_List_Recursive;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Container : Node_Vectors.Vector;
      Node      : Index_Type) return M.Path_Type
   is
      Result : M.Path_Type := M.Root;
      Next   : Count_Type  := Node;

   begin
      while Next /= 0 loop
         declare
            Node_Acc : constant access constant Node_Type :=
                         Node_Vectors.Constant_Reference (Container, Next)
                         .Element;
         begin
            exit when Node_Acc.all.Parent = No_Element;

            Result := M.Insert (Result, 0, Node_Acc.all.Position);
            Next   := Node_Acc.all.Parent.Node;
         end;
      end loop;

      return Result;
   end Get_Path;

   ---------------
   -- Same_Path --
   ---------------

   function Same_Path
     (Left, Right : Tree;
      L_Position : Cursor;
      R_Position : Cursor) return Boolean
   is
      L : Cursor := L_Position;
      R : Cursor := R_Position;

   begin

      --  Walk up both trees, starting and L and R in Left and Right
      --  respectively, checking that the path to the node from their
      --  parents are the same at each step.

      while L /= No_Element loop

         --  Check if R hit the root before L

         if R = No_Element then
            return False;
         end if;

         declare
            L_Acc : constant not null access constant Node_Type :=
                        Node_Vectors.Constant_Reference
                          (Left.Nodes, L.Node).Element;
            R_Acc : constant not null access constant Node_Type :=
                        Node_Vectors.Constant_Reference
                          (Right.Nodes, R.Node).Element;
         begin
            --  Check that both nodes have the same position w.r.t
            --  their parent.

            if L_Acc.all.Position /= R_Acc.all.Position then
               return False;
            end if;

            --  Go up and check the parent

            L := L_Acc.all.Parent;
            R := R_Acc.all.Parent;
         end;
      end loop;

      --  L and R should both have hit the root at the same time if they have
      --  the same path.

      return R = No_Element;
   end Same_Path;

   ----------------
   -- First_Impl --
   ----------------

   function First_Impl (Container : Tree) return Cursor is
     (Container.Root);

   ---------------
   -- Next_Impl --
   ---------------

   function Next_Impl
     (Container : Tree;
      Position  : Cursor) return Cursor
   is
      Node : Cursor;
      Next : Cursor;

   begin
      if not Has_Element_Impl (Container, Position) then
         return No_Element;
      end if;

      Next := First_Child_Impl (Container, Position);

      if Next = No_Element then
         Node := Position;

         while Node /= No_Element loop
            Next := Next_Sibling_Impl (Container, Node);

            exit when Next /= No_Element;

            Node := Parent_Impl (Container, Node);
         end loop;
      end if;

      return Next;
   end Next_Impl;

   -----------------------
   -- Next_Sibling_Impl --
   -----------------------

   function Next_Sibling_Impl
     (Container : Tree;
      Position  : Cursor) return Cursor
   is
      Next : Cursor := No_Element;

   begin
      declare
         Node_Acc : constant access constant Node_Type :=
                      Node_Vectors.Constant_Reference
                        (Container.Nodes, Position.Node).Element;
         Way      : constant Way_Type := Node_Acc.all.Position;
      begin
         if Way < Way_Type'Last and then Node_Acc.all.Parent /= No_Element then
            declare
               Parent_Acc : constant access constant Node_Type :=
                              Node_Vectors.Constant_Reference
                                (Container.Nodes, Node_Acc.all.Parent.Node)
                                .Element;
            begin
               for W in Way_Type'Succ (Way) .. Way_Type'Last loop
                  Next := Parent_Acc.all.Ways (W);
                  exit when Next /= No_Element;
               end loop;
            end;
         end if;
      end;

      return Next;
   end Next_Sibling_Impl;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (Container : Tree;
      Path      : M.Path_Type) return Cursor
   is
      C : Cursor                         := Container.Root;
      I : SPARK.Big_Integers.Big_Integer := 1;

   begin
      while C /= No_Element and then I <= M.Length (Path) loop
         C := Child (Container, C, M.Get (Path, I));
         I := I + 1;
      end loop;

      return C;
   end Find_Node;

   --------------------------
   -- Last_Node_In_Subtree --
   --------------------------

   function Last_Node_In_Subtree
     (Container : Tree;
      Position  : Cursor) return Cursor
   is
      Node : Cursor := Position;
      Next : Cursor;

   begin
      while Node /= No_Element loop
         Next := Last_Child (Container, Node);
         exit when Next = No_Element;
         Node := Next;
      end loop;

      return Node;
   end Last_Node_In_Subtree;

end SPARK.Containers.Formal.Unbounded_Multiway_Trees;
