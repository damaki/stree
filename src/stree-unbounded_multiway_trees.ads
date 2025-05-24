--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with SPARK.Big_Integers;                  use SPARK.Big_Integers;
with SPARK.Containers.Functional.Sets;
with SPARK.Containers.Types;              use SPARK.Containers.Types;
with SPARK.Containers.Functional.Vectors;

private with SPARK.Containers.Formal.Unbounded_Vectors;

generic
   type Element_Type is private;
   type Way_Type is (<>);
   with function Equivalent_Elements
                   (Left, Right : Element_Type) return Boolean is "=";
package Stree.Unbounded_Multiway_Trees with
  SPARK_Mode => On
is

   --  Contracts in this unit are meant for analysis only, not for run-time
   --  checking.

   pragma Assertion_Policy (Pre => Ignore);
   pragma Assertion_Policy (Post => Ignore);
   pragma Assertion_Policy (Contract_Cases => Ignore);

   type Tree is private with
     Default_Initial_Condition => Is_Empty (Tree),
     Iterable                  => (First       => First,
                                   Next        => Next,
                                   Has_Element => Has_Element,
                                   Element     => Element);

   type Cursor is record
      Node : Count_Type;
   end record;

   type Way_Cursor_Array is array (Way_Type) of Cursor;

   No_Element : constant Cursor := (Node => 0);

   function Empty_Tree return Tree with
     Global => null,
     Post   => Is_Empty (Empty_Tree'Result);
   --  Create an empty tree

   function Is_Empty (Container : Tree) return Boolean with
     Global => null;
   --  Query if the tree contains no elements

   function Length (Container : Tree) return Count_Type with
     Global => null,
     Post   => Is_Empty (Container) = (Length'Result = 0);
   --  Get the number of elements in the tree

   function Has_Element
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => (if Position = No_Element or else Is_Empty (Container) then
                  not Has_Element'Result);
   --  Query if the tree has an element at the specified cursor

   function Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Global => null,
     Pre => Has_Element (Container, Position);
   --  Get the element at the specified position in the tree

   pragma Unevaluated_Use_Of_Old (Allow);

   ------------------
   -- Formal Model --
   ------------------

   package Formal_Model with Ghost is

      subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

      package Count_Type_Conversions is new
        SPARK.Big_Integers.Signed_Conversions (Count_Type);
      use Count_Type_Conversions;

      function To_Cursor (I : Positive_Count_Type) return Cursor is
        (Cursor'(Node => I));

      -------------
      -- Cursors --
      -------------

      package Cursor_Sets is new SPARK.Containers.Functional.Sets
        (Element_Type => Cursor);
      use Cursor_Sets;

      function All_Cursors (Container : Tree) return Cursor_Sets.Set with
        Global => null,
        Post   => --  The set contains only cursors that are in the container
                  (for all I in Count_Type =>
                     Has_Element (Container, Cursor'(Node => I)) =
                       Contains (All_Cursors'Result, Cursor'(Node => I)))

                  and then Length (All_Cursors'Result) =
                             To_Big_Integer (Length (Container));
      --  Get the set of all valid cursors in the tree

      -----------
      -- Paths --
      -----------

      package Way_Sequences is new SPARK.Containers.Functional.Vectors
        (Index_Type   => Positive_Count_Type,
         Element_Type => Way_Type,
         "="          => "=");
      use Way_Sequences;

      function Is_Concat (Q, V, P : Way_Sequences.Sequence) return Boolean is
        (Length (P) - Length (V) = Length (Q)
         and then (for all I in 1 .. Last (Q) => Get (P, I) = Get (Q, I))
         and then (for all I in 1 .. Last (V) =>
                    Get (P, I + Last (Q)) = Get (V, I))
         and then (for all I in Last (Q) + 1 .. Last (P) =>
                    Get (V, I - Last (Q)) = Get (P, I)))
      with
        Pre =>
          Length (Q) < To_Big_Integer (Integer (Positive_Count_Type'Last));
      --  Returns True if P is the concatenation of Q & V.

      function Is_Insert
        (V, P : Way_Sequences.Sequence;
         I    : Positive_Count_Type;
         E    : Way_Type)
         return Boolean
      is
        (Length (P) = Length (V) + 1
         and then (for all J in 1 .. Last (P) =>
                     (if    J < I then Get (P, J) = Get (V, J)
                      elsif J = I then Get (P, J) = E
                      else             Get (P, J) = Get (V, J - 1))))
      with
        Pre => I in 1 .. Last (P)
               and then Length (P) <=
                          To_Big_Integer (Integer (Positive_Count_Type'Last));
      --  Returns True if P is equal to V with element E inserted at index I

      function Is_Add
        (S1 : Way_Sequences.Sequence;
         W  : Way_Type;
         S2 : Way_Sequences.Sequence)
         return Boolean
      is
        (Length (S2) - 1 = Length (S1)
         and then S1 < S2
         and then Get (S2, Last (S2)) = W);
      --  Returns True if S2 is equal to S1 with W appended to it

      ------------------
      -- Formal Model --
      ------------------

      --  The formal tree model is represented as an array of nodes, where
      --  each index maps to a cursor. Each node has references (cursors) to
      --  its parent and child nodes. The postcondition of the Model function
      --  constrains these references to organise them into a tree structure.

      type Model_Node is record
         Path     : Way_Sequences.Sequence;
         --  The path from the root to this node

         Parent   : Cursor;
         --  The parent node. This is No_Element for the root node.

         Way      : Way_Type;
         --  Which direction is taken from the parent to get to this node.

         Children : Way_Cursor_Array;
         --  References to each child node

         In_Tree  : Boolean;
         --  True if this node exists in the tree. This is false for nodes that
         --  are not in the tree.
      end record;

      type Model_Type is array (Positive_Count_Type) of Model_Node with
        Predicate => (for all N of Model_Type =>
                        Way_Sequences.Length (N.Path)
                        < To_Big_Integer (Integer (Positive_Count_Type'Last)));

      function Model (Container : Tree) return Model_Type with
        Global => null,
        Post   =>
          --  In_Tree reflects whether the tree has an element at the cursor
          (for all I in Model'Result'Range =>
             Model'Result (I).In_Tree =
               Has_Element (Container, To_Cursor (I)))

          --  If the tree is empty then there are no nodes in the tree,
          --  otherwise there is at least one node.
          and then
            Is_Empty (Container) = (for all N of Model'Result => not N.In_Tree)

          --  All nodes not in the tree have no parent or children, and have
          --  an empty path
          and then
            (for all N of Model'Result =>
               (if not N.In_Tree then
                  N.Parent = No_Element
                  and then Length (N.Path) = 0
                  and then (for all C of N.Children => C = No_Element)))

          --  For the root node (i.e. the node with no parent), the path to
          --  itself is empty. For all other nodes the path is non-empty.
          and then
            (for all I in Model'Result'Range =>
               (Model'Result (I).Parent = No_Element)
               =
               (Length (Model'Result (I).Path) = 0))

          --  If the tree is not empty, then there is a root
          and then
            (if (for some N of Model'Result => N.In_Tree) then
               (for some N of Model'Result =>
                  N.In_Tree and then N.Parent = No_Element))

          --  The parent of all non-root nodes is in the tree, and the node is
          --  is the child of its parent at the specified way
          and then
            (for all I in Model'Result'Range =>
               (if Model'Result (I).In_Tree
                   and then Model'Result (I).Parent /= No_Element
                then (Model'Result (Model'Result (I).Parent.Node).In_Tree
                      and then Model'Result
                                 (Model'Result (I).Parent.Node)
                                 .Children (Model'Result (I).Way)
                                 .Node = I)))

          --  If a node is in the tree, then its children are also in the tree
          --  and that node is its parent
          and then
            (for all I in Model'Result'Range =>
               (if Model'Result (I).In_Tree then
                  (for all W in Way_Type =>
                     (if Model'Result (I).Children (W) /= No_Element then
                        Model'Result (Model'Result
                                        (I).Children (W).Node).In_Tree
                        and then Model'Result
                                   (Model'Result (I).Children (W).Node)
                                   .Parent.Node = I))))

          --  The path from the root to non-root tree nodes is equal to the
          --  path to their parent extended by the last way to get to the
          --  node. For other nodes, the path is empty.
          and then
            (for all I in Model'Result'Range =>
               (if Model'Result (I).In_Tree
                   and then Model (Container) (I).Parent /= No_Element
                then Is_Add (Model'Result (Model'Result (I).Parent.Node).Path,
                             Model'Result (I).Way,
                             Model'Result (I).Path)))

          --  Nodes in the tree all have different associated paths
          and then
            (for all I in Model'Result'Range =>
               (if Model'Result (I).In_Tree then
                  (for all J in Model'Result'Range =>
                     (if Model'Result (J).In_Tree
                         and then Model'Result (J).Path =
                                  Model'Result (I).Path
                      then J = I))));

      ---------------------
      -- Model Functions --
      ---------------------

      function M_Is_Root
        (Container : Tree;
         Position  : Cursor)
         return Boolean
      is
        (Position /= No_Element
         and then Model (Container) (Position.Node).In_Tree
         and then Last (Model (Container) (Position.Node).Path) = 0)
      with
        Global => null,
        Post   => (if M_Is_Root'Result then
                     Model (Container) (Position.Node).Parent = No_Element
                     and then Has_Element (Container, Position));

      function M_Parent
        (Container : Tree;
         Position  : Cursor)
         return Cursor
      is
        (Model (Container) (Position.Node).Parent)
      with
        Global => null,
        Pre  => Has_Element (Container, Position),
        Post => (if M_Is_Root (Container, Position)
                 then M_Parent'Result = No_Element
                 else Has_Element (Container, M_Parent'Result));

      function M_Child
        (Container : Tree;
         Position  : Cursor;
         Way       : Way_Type)
         return Cursor
      is
        (Model (Container) (Position.Node).Children (Way))
      with
        Global => null,
        Pre  => Has_Element (Container, Position),
        Post => (if M_Child'Result /= No_Element then
                   Has_Element (Container, M_Child'Result));

      function M_Has_Child
        (Container : Tree;
         Position  : Cursor;
         Way       : Way_Type)
         return Boolean
      is
        (M_Child (Container, Position, Way) /= No_Element)
      with
        Global => null,
        Pre  => Has_Element (Container, Position);

      function M_Has_Sibling
        (Container : Tree;
         Position  : Cursor;
         Way       : Way_Type)
         return Boolean
      is
        (Has_Element (Container, Position)
         and then not M_Is_Root (Container, Position)
         and then M_Has_Child (Container, M_Parent (Container, Position), Way))
      with
        Global => null;

      function M_Is_Ancestor
        (Container : Tree;
         Ancestor  : Cursor;
         Child     : Cursor)
         return Boolean
      is
        (Model (Container) (Ancestor.Node).Path <
           Model (Container) (Child.Node).Path)
      with
        Global => null,
        Pre    => Has_Element (Container, Ancestor)
                  and then Has_Element (Container, Child);

      function M_Depth
        (Container : Tree;
         Position  : Cursor)
         return Count_Type
      is
        (Count_Type
           (To_Integer (Length (Model (Container) (Position.Node).Path))))
      with
        Global => null,
        Pre    => Position /= No_Element;

      function M_Has_Children
        (Container : Tree;
         Position  : Cursor)
         return Boolean
      is
        (Position /= No_Element
         and then (for some C of Model (Container) (Position.Node).Children =>
                     C /= No_Element))
      with
        Global => null;

      function M_Is_Leaf
        (Container : Tree;
         Position  : Cursor)
         return Boolean
      is
        (Position /= No_Element
         and then Model (Container) (Position.Node).In_Tree
         and then (for all C of Model (Container) (Position.Node).Children =>
                     C = No_Element))
      with
        Global => null;

      function M_Equivalent_Trees (Left, Right : Tree) return Boolean is
        (Model (Left) = Model (Right));
      --  Left and Right have the same tree structure

      function M_Equivalent_Trees_Except
        (Left, Right : Tree;
         Position    : Cursor)
         return Boolean
      is
        (for all I in Positive_Count_Type =>
           (if I /= Position.Node then
              Model (Left) (I) = Model (Right) (I)));
      --  Left and Right have the same tree structure, except for the node at
      --  the specified Position.

      function M_Elements_Unchanged
        (Left, Right : Tree)
         return Boolean
      is
        (for all I in Positive_Count_Type =>
           (if Model (Left) (I).In_Tree then
              Model (Right) (I).In_Tree
              and then Equivalent_Elements (Element (Left,  To_Cursor (I)),
                                            Element (Right, To_Cursor (I)))));
      --  Elements in Left are unchanged in Right, and at the same positions

   end Formal_Model;

   use Formal_Model;
   use Formal_Model.Cursor_Sets;
   use Formal_Model.Count_Type_Conversions;

   use type Formal_Model.Way_Sequences.Sequence;

   function Is_Root
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Is_Root'Result = M_Is_Root (Container, Position);
   --  Query if a cursor references the root of the tree.

   function Is_Leaf
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Is_Leaf'Result = M_Is_Leaf (Container, Position);

   function Root (Container : Tree) return Cursor with
     Global => null,
     Post   => (if Is_Empty (Container)
                then Root'Result = No_Element
                else M_Is_Root (Container, Root'Result));
   --  Get a cursor to the root of the tree.
   --
   --  No_Element is returned iff the tree is empty, otherwise a cursor to the
   --  root node is returned.

   function First (Container : Tree) return Cursor renames Root;
   --  Get a cursor to the first element in the tree.

   function First_Element (Container : Tree) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);
   --  Get the first element in the tree.

   function Last (Container : Tree) return Cursor with
     Global => null,
     Contract_Cases =>
       (Is_Empty (Container) => Last'Result = No_Element,
        others               => Has_Element (Container, Last'Result));

   function Last_Element (Container : Tree) return Element_Type is
     (Element (Container, Last (Container)))
   with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Next
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   => --  The next of an invalid cursor is No_Element
               (if not Has_Element (Container, Position) then
                  Next'Result = No_Element)

               --  If the result is not No_Element, then a valid cursor is
               --  returned.
               and then (if Next'Result /= No_Element then
                           Has_Element (Container, Position));

   function Prev
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   => --  The prev of an invalid cursor is No_Element
               (if not Has_Element (Container, Position) then
                  Prev'Result = No_Element)

               --  If the result is not No_Element, then a valid cursor is
               --  returned.
               and then (if Prev'Result /= No_Element then
                           Has_Element (Container, Position));

   function First_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   =>
       --  No_Element is returned if an invalid cursor is given or the
       --  node at that Position is not a leaf node. Otherwise, a valid
       --  cursor is returned.
       (First_Child'Result = No_Element)
       =
       (not Has_Element (Container, Position)
        or else M_Is_Leaf (Container, Position))

       and then
         (if First_Child'Result /= No_Element then
            --  A valid cursor is returned
            Has_Element (Container, First_Child'Result)

            --  The referenced node is a child of the node at Position.
            and then M_Parent (Container, First_Child'Result) = Position

            --  The returned child is the first child
            and then (for all W in Way_Type =>
                        (if W < Direction (Container, First_Child'Result) then
                           M_Child (Container, Position, W) = No_Element)));

   function First_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Pre => Has_Element (Container, Position)
            and then M_Has_Children (Container, Position),
     Post => First_Child_Element'Result =
               Element (Container, First_Child (Container, Position));

   function Last_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   =>
       --  No_Element is returned if an invalid cursor is given or the
       --  node at that Position is not a leaf node. Otherwise, a valid
       --  cursor is returned.
       (Last_Child'Result = No_Element)
       =
       (not Has_Element (Container, Position)
        or else M_Is_Leaf (Container, Position))

       and then
         (if Last_Child'Result /= No_Element then
            --  A valid cursor is returned
            Has_Element (Container, Last_Child'Result)

            --  The referenced node is a child of the node at Position.
            and then M_Parent (Container, Last_Child'Result) = Position

            --  The returned child is the last child
            and then (for all W in Way_Type =>
                        (if W > Direction (Container, Last_Child'Result) then
                           M_Child (Container, Position, W) = No_Element)));

   function Last_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Pre => Has_Element (Container, Position)
            and then M_Has_Children (Container, Position),
     Post => Last_Child_Element'Result =
               Element (Container, Last_Child (Container, Position));

   function Next_Sibling
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   =>
       --  A non-null cursor is returned iff Position references a valid
       --  non-root node, and there exists a sibling node with a higher way
       --  than the node at Position. Otherwise, No_Element is returned.
       (Next_Sibling'Result /= No_Element)
       =
       (Has_Element (Container, Position)
        and then not Is_Root (Container, Position)
        and then (for some W in Way_Type =>
                    W > Direction (Container, Position)
                    and then M_Has_Sibling (Container, Position, W)))

       --  If a non-null cursor is returned, then that cursor references the
       --  next sibling node.
       and then
         (if Next_Sibling'Result /= No_Element then
            Has_Element (Container, Next_Sibling'Result)

            --  The result has the same parent as the node at Position
            and then M_Parent (Container, Next_Sibling'Result) =
                     M_Parent (Container, Position)

            --  A next sibling is returned
            and then Direction (Container, Next_Sibling'Result) >
                     Direction (Container, Position)

            --  The returned sibling is the closest next one.
            --  That is, there are no other siblings between the returned
            --  one and the one at Position.
            and then
              (for all W in Way_Type =>
                 (if W < Direction (Container, Next_Sibling'Result)
                    and then W > Direction (Container, Position)
                  then not M_Has_Sibling (Container, Position, W))));

   function Prev_Sibling
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   =>
       --  A non-null cursor is returned iff Position references a valid
       --  non-root node, and there exists a sibling node with a lower way
       --  than the node at Position. Otherwise, No_Element is returned.
       (Prev_Sibling'Result /= No_Element)
       =
       (Has_Element (Container, Position)
        and then not Is_Root (Container, Position)
        and then (for some W in Way_Type =>
                    W < Direction (Container, Position)
                    and then M_Has_Sibling (Container, Position, W)))

       --  If a non-null cursor is returned, then that cursor references the
       --  previous sibling node.
       and then
         (if Prev_Sibling'Result /= No_Element then
            Has_Element (Container, Prev_Sibling'Result)

            --  The result has the same parent as the node at Position
            and then M_Parent (Container, Prev_Sibling'Result) =
                     M_Parent (Container, Position)

            --  A previous sibling is returned
            and then Direction (Container, Prev_Sibling'Result) <
                     Direction (Container, Position)

            --  The returned sibling is the closest previous one.
            --  That is, there are no other siblings between the returned
            --  one and the one at Position.
            and then
              (for all W in Way_Type =>
                 (if W > Direction (Container, Prev_Sibling'Result)
                    and then W < Direction (Container, Position)
                  then not M_Has_Sibling (Container, Position, W))));

   function Root_Element (Container : Tree) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);
   --  Get the element at the root of the tree.
   --
   --  This may only be called when the tree is not empty.

   function Parent
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Inline,
     Global => null,
     Post   => (if not Has_Element (Container, Position)
                then Parent'Result = No_Element
                else Parent'Result = M_Parent (Container, Position));
   --  Get a cursor to the parent of a node.
   --
   --  No_Element is returned iff the Position references the root node.

   function Child
     (Container : Tree;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   with
     Inline,
     Global => null,
     Post   => (if not Has_Element (Container, Position)
                then Child'Result = No_Element
                else Child'Result = M_Child (Container, Position, Way));
   --  Get a cursor to a child element of a node.
   --
   --  No_Element is returned if the node at the given Position does not have a
   --  child at the specified Way.

   function Direction
     (Container : Tree;
      Position  : Cursor)
      return Way_Type
   with
     Inline,
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   => Direction'Result = Model (Container) (Position.Node).Way;
   --  Get the direction (way) to the node at the given Position from its
   --  parent node.

   function Is_Ancestor
     (Container : Tree;
      Ancestor  : Cursor;
      Child     : Cursor)
      return Boolean
   with
     Global => null,
     Pre    => Has_Element (Container, Ancestor)
               and then Has_Element (Container, Child),
     Post   => Is_Ancestor'Result = M_Is_Ancestor (Container, Ancestor, Child);
   --  Query if Ancestor is an ancestor node of Child.

   function Depth
     (Container : Tree;
      Position  : Cursor)
      return Count_Type
   with
     Global => null,
     Pre    => Position /= No_Element,
     Post   => Depth'Result = M_Depth (Container, Position);
   --  Get the depth of a node in the tree.
   --
   --  The root node has depth 0.
   --
   --  If Position does not reference a valid element, then 0 is returned.

   procedure Insert_Root
     (Container : in out Tree;
      New_Item  :        Element_Type)
   with
     Global => null,
     Pre    => Is_Empty (Container),
     Post   =>
       Length (Container) = 1
       and then Equivalent_Elements (New_Item, Root_Element (Container))
       and then not M_Has_Children (Container, Root (Container));

   procedure Insert_Child
     (Container : in out Tree;
      New_Item  :        Element_Type;
      Position  :        Cursor;
      Way       :        Way_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position)
               and then Length (Container) < Count_Type'Last
               and then M_Child (Container, Position, Way) = No_Element,
     Post   =>
       --  The length of the container has incremented
       Length (Container) = Length (Container'Old) + 1

       --  A new node has been inserted as a child of the specified Position
       and then
         Model (Container) (M_Child (Container, Position, Way).Node) =
           (Path     => Way_Sequences.Add
                          (Model (Container'Old) (Position.Node).Path, Way),
            Parent   => Position,
            Way      => Way,
            Children => [others => No_Element],
            In_Tree  => True)

       --  The element at the new node is the New_Item
       and then Equivalent_Elements
                  (New_Item,
                   Element (Container, M_Child (Container, Position, Way)))

       --  All previously existing elements are unchanged
       and then M_Elements_Unchanged (Container'Old, Container)

       --  The tree structure is unchanged, except for the node at the
       --  specified position.
       and then M_Equivalent_Trees_Except (Container'Old, Container, Position)

       --  The node at the specified Position is the same, except for the
       --  addition of the new child node.
       and then
         Model (Container) (Position.Node) =
           (Model (Container'Old) (Position.Node)
            with delta
              Children => (Model (Container'Old) (Position.Node).Children
                           with delta
                             Way => M_Child (Container, Position, Way)));

   procedure Insert_Parent
     (Container : in out Tree;
      New_Item  :        Element_Type;
      Position  :        Cursor;
      Way       :        Way_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position)
               and then Length (Container) < Count_Type'Last,
     Post   =>
       --  The length of the container has increased by one
       Length (Container) = Length (Container'Old) + 1

       --  A new cursor has been added which references the new node
       and then
         All_Cursors (Container) = Add (All_Cursors (Container'Old),
                                        M_Parent (Container, Position))

       --  A new node has been inserted at the requested position in the tree
       and then Has_Element (Container, M_Parent (Container, Position))
       and then Equivalent_Elements
                  (New_Item,
                   Element (Container, M_Parent (Container, Position)))
       and then
         Model (Container) (M_Parent (Container, Position).Node) =
           (Path     => Way_Sequences.Add
                          (Model (Container'Old) (Position.Node).Path, Way),
            Parent   => M_Parent (Container'Old, Position),
            Way      => Way,
            Children => [Way => Position],
            In_Tree  => True)

       and then
         (for all I in Positive_Count_Type =>
            (if Model (Container'Old) (I).In_Tree then
               (
                --  Nodes that are in the affected subtree have their paths
                --  extended.
                if Model (Container'Old) (Position.Node).Path <=
                     Model (Container'Old) (I).Path
                then Is_Insert (Model (Container'Old) (I).Path,
                                Model (Container) (I).Path,
                                From_Big_Integer
                                  (Way_Sequences.Length (Model (Container'Old)
                                                               (Position.Node)
                                                               .Path) + 1),
                                Way)
                --  Otherwise the path to the node is unchanged
                else Model (Container) (I) = Model (Container'Old) (I))));
   --  Insert a new item as a parent of the node at the specified Position.
   --
   --  For example, adding node F as the parent of C changes the tree as
   --  follows:
   --         A              A
   --        / \            / \
   --       B   C    ===>  B  *F*
   --          / \              \
   --         D   E              C
   --                           / \
   --                          D   E

   function At_End (E : access constant Tree) return access constant Tree is
     (E)
   with
     Ghost,
     Annotate => (GNATprove, At_End_Borrow);

   function At_End
     (E : access constant Element_Type) return access constant Element_Type
   is
     (E)
   with
     Ghost,
     Annotate => (GNATprove, At_End_Borrow);

   function Constant_Reference
     (Container : aliased Tree;
      Position  : Cursor)
      return not null access constant Element_Type
   with
     Inline,
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   => Equivalent_Elements
                 (Constant_Reference'Result.all,
                  Element (Container, Position));

   function Reference
     (Container : not null access Tree;
      Position  : Cursor)
      return not null access Element_Type
   with
     Inline,
     Global => null,
     Pre    => Has_Element (Container.all, Position),
     Post   => --  Cursors are preserved
               All_Cursors (At_End (Container).all) =
                 All_Cursors (Container.all)

               --  The tree structure is preserved
               and then
                 M_Equivalent_Trees (Container.all'Old, At_End (Container).all)

               --  Other elements are preserved
               and then
                 (for all I in Positive_Count_Type =>
                    (if Model (Container.all'Old) (I).In_Tree
                        and then I /= Position.Node
                     then Equivalent_Elements
                            (Element (Container.all'Old, To_Cursor (I)),
                             Element (Container.all, To_Cursor (I)))))

               --  The value designated by the result of Reference is now
               --  equivalent to the element at the specified Position in the
               --  tree.
               and then
                 Equivalent_Elements
                   (At_End (Reference'Result).all,
                    Element (At_End (Container).all, Position));

private
   pragma SPARK_Mode (Off);

   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;

   type Node_Type is record
      Element  : aliased Element_Type;
      Parent   : Cursor;
      Position : Way_Type;
      Ways     : Way_Cursor_Array;
   end record;

   package Node_Vectors is new SPARK.Containers.Formal.Unbounded_Vectors
     (Index_Type   => Index_Type,
      Element_Type => Node_Type,
      "="          => "=");

   type Tree is record
      Nodes : aliased Node_Vectors.Vector := Node_Vectors.Empty_Vector;
      Root  : Cursor                      := No_Element;
   end record;

   function Last_In_Subtree
     (Container    : Tree;
      Subtree_Root : Cursor)
      return Cursor;

end Stree.Unbounded_Multiway_Trees;