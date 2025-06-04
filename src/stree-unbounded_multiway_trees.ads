--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with SPARK.Big_Integers;                  use SPARK.Big_Integers;
with SPARK.Containers.Types;              use SPARK.Containers.Types;
with SPARK.Containers.Functional.Vectors;

private with SPARK.Containers.Formal.Unbounded_Vectors;

--  This package implements an unbounded multi-way tree container.
--
--  The order of the tree is determined by the range of the type Way_Type,
--  which can be any discrete type (integer, modular, or enumeration type).
--
--  Each node in the tree holds exactly one element.
--
--  Nodes are inserted into the tree using the following procedures:
--   * Insert_Root: Creates the initial (root) node in an empty tree.
--   * Insert_Child: Create a new node that is a child of another node.
--   * Insert_Parent: Create a new node and insert it as the parent of another
--     node.
--
--  Iteration over the tree is available using the First, Next, and Has_Element
--  procedures. Iteration always occurs in depth-first order. The tree can
--  be iterated over using iterator loops. "for .. in" loops iterate over
--  cursors, and "for .. of" loops iterate over elements.
--
--  Reverse iteration is also possible using the functions Last, Prev, and
--  Has_Element.
--
--  Quantification over trees is also available using "for all" and "for some".

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

   pragma Unevaluated_Use_Of_Old (Allow);

   ------------------
   -- Formal Model --
   ------------------

   package Formal_Model with Ghost is

      subtype Valid_Cursor_Range is Count_Type range 1 .. Count_Type'Last;

      package Count_Type_Conversions is new
        SPARK.Big_Integers.Signed_Conversions (Count_Type);

      function To_Cursor (I : Count_Type) return Cursor is
        (Cursor'(Node => I))
      with
        Annotate => (GNATprove, Inline_For_Proof);

      -----------
      -- Paths --
      -----------

      package Way_Sequences is new SPARK.Containers.Functional.Vectors
        (Index_Type   => Valid_Cursor_Range,
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
          Length (Q) < To_Big_Integer (Integer (Valid_Cursor_Range'Last));
      --  Returns True if P is the concatenation of Q & V.

      function Is_Insert
        (V, P : Way_Sequences.Sequence;
         I    : Valid_Cursor_Range;
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
                          To_Big_Integer (Integer (Valid_Cursor_Range'Last));
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

      type Model_Type is array (Valid_Cursor_Range) of Model_Node with
        Predicate => (for all N of Model_Type =>
                        Way_Sequences.Length (N.Path)
                        < To_Big_Integer (Integer (Valid_Cursor_Range'Last)));

      function Parent (M : Model_Type; C : Cursor) return Cursor is
        (M (C.Node).Parent)
      with
        Pre => C /= No_Element,
        Annotate => (GNATprove, Inline_For_Proof);

      function Path (M : Model_Type; C : Cursor) return Way_Sequences.Sequence
      is
        (M (C.Node).Path)
      with
        Pre => C /= No_Element,
        Annotate => (GNATprove, Inline_For_Proof);

      function Has_Children
        (M : Model_Type;
         C : Cursor)
         return Boolean
      is
        (C /= No_Element
         and then (for some Ch of M (C.Node).Children => Ch /= No_Element))
      with
        Annotate => (GNATprove, Inline_For_Proof);

      function Child (M : Model_Type; C : Cursor; W : Way_Type) return Cursor
      is
        (M (C.Node).Children (W))
      with
        Pre => C /= No_Element,
        Annotate => (GNATprove, Inline_For_Proof);

      function Children (M : Model_Type; C : Cursor) return Way_Cursor_Array is
        (M (C.Node).Children)
      with
        Pre => C /= No_Element,
        Annotate => (GNATprove, Inline_For_Proof);

      function Has_Sibling
        (M : Model_Type;
         C : Cursor;
         W : Way_Type)
         return Boolean
      is
        (C /= No_Element
         and then Parent (M, C) /= No_Element
         and then Child (M, Parent (M, C), W) /= No_Element);

      function Sibling
        (M : Model_Type;
         C : Cursor;
         W : Way_Type)
         return Cursor
      is
        (M (M (C.Node).Parent.Node).Children (W))
      with
        Pre => Has_Element (M, C) and then not Is_Root (M, C),
        Annotate => (GNATprove, Inline_For_Proof);

      function Has_Element (M : Model_Type; C : Cursor) return Boolean is
        (C /= No_Element and then M (C.Node).In_Tree)
      with
        Annotate => (GNATprove, Inline_For_Proof);

      function Direction (M : Model_Type; C : Cursor) return Way_Type is
        (M (C.Node).Way)
      with
        Pre => C /= No_Element,
        Annotate => (GNATprove, Inline_For_Proof);

      function Is_Root (M : Model_Type; C : Cursor) return Boolean is
        (C /= No_Element
         and then M (C.Node).In_Tree
         and then M (C.Node).Parent = No_Element)
      with
        Annotate => (GNATprove, Inline_For_Proof);

      function Is_Leaf (M : Model_Type; C : Cursor) return Boolean is
        (C /= No_Element
         and then M (C.Node).In_Tree
         and then (for all Ch of M (C.Node).Children => Ch = No_Element));

      function Is_Ancestor
        (M          : Model_Type;
         Ancestor   : Cursor;
         Descendant : Cursor)
         return Boolean
      is
        (Has_Element (M, Ancestor)
         and then Has_Element (M, Descendant)
         and then M (Ancestor.Node).Path < M (Descendant.Node).Path);

      function In_Subtree
        (M            : Model_Type;
         Subtree_Root : Cursor;
         Position     : Cursor)
         return Boolean
      is
        (Has_Element (M, Subtree_Root)
         and then Has_Element (M, Position)
         and then Path (M, Subtree_Root) <= Path (M, Position));

      function Depth (M : Model_Type; C : Cursor) return Count_Type is
        (Count_Type (To_Integer (Length (Path (M, C)))))
      with
        Global => null,
        Pre    => C /= No_Element;

      function Model (Container : Tree) return Model_Type with
        Global => null,
        Post   =>
          --  If the tree is empty then there are no nodes in the tree,
          --  otherwise there is at least one node.
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
            (for all N of Model'Result =>
               (N.Parent = No_Element) = (Length (N.Path) = 0))

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
                        Has_Element (Model'Result,
                                     Model'Result (I).Children (W))
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
                         and then Model'Result (J).Path = Model'Result (I).Path
                      then J = I))))

           --  There is exactly one root in the tree
           and then
             (for all I in Model'Result'Range =>
                (for all J in Model'Result'Range =>
                   (if Is_Root (Model'Result, To_Cursor (I))
                       and then Is_Root (Model'Result, To_Cursor (J))
                    then I = J)))

           --  Nodes not in the tree have no children, no parent, and an empty
           --  path.
           and then
             (for all N of Model'Result =>
                (if not N.In_Tree then
                   N.Parent = No_Element
                   and then N.Path = Empty_Sequence
                   and then N.Way = Way_Type'First
                   and then (for all C of N.Children => C = No_Element)));

   end Formal_Model;

   use Formal_Model;

   use type Formal_Model.Way_Sequences.Sequence;

   function Has_Element
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Has_Element'Result = Has_Element (Model (Container), Position);
   --  Query if the tree has an element at the specified cursor

   function Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Global => null,
     Pre => Has_Element (Container, Position);
   --  Get the element at the specified position in the tree

   procedure Replace_Element
     (Container : in out Tree;
      Position  :        Cursor;
      New_Item  :        Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Model (Container), Position),
     Post   => --  The element is updated with the new value
               Equivalent_Elements (Element (Container, Position), New_Item)

               --  The tree structure is unchanged.
               and then Model (Container) = Model (Container'Old)

               --  All other elements are unchanged
               and then (for all I in Valid_Cursor_Range =>
                           (if To_Cursor (I) /= Position then
                              Equivalent_Elements
                                (Element (Container'Old, To_Cursor (I)),
                                 Element (Container, To_Cursor (I)))));

   function Is_Root
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Is_Root'Result = Is_Root (Model (Container), Position);
   --  Query if a cursor references the root of the tree.

   function Is_Leaf
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Is_Leaf'Result = Is_Leaf (Model (Container), Position);
   --  Query if a cursor references a leaf node in the tree.
   --
   --  A leaf node is any node in the tree that has no children.

   function Root (Container : Tree) return Cursor with
     Global => null,
     Post   => (if Is_Empty (Container)
                then Root'Result = No_Element
                else Is_Root (Model (Container), Root'Result));
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
        others               => Has_Element (Model (Container), Last'Result));
   --  Get a cursor to the last element in the tree.

   function Last_Element (Container : Tree) return Element_Type is
     (Element (Container, Last (Container)))
   with
     Global => null,
     Pre    => not Is_Empty (Container);
   --  Get the last element in the tree.

   function Next
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   => --  The next of an invalid cursor is No_Element
               (if not Has_Element (Model (Container), Position) then
                  Next'Result = No_Element)

               --  If the result is not No_Element, then a valid cursor is
               --  returned.
               and then (if Next'Result /= No_Element then
                           Has_Element (Model (Container), Position));
   --  Get the next element in the tree.
   --
   --  The next node is retrieved in depth-first order.

   function Prev
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Post   => --  The prev of an invalid cursor is No_Element
               (if not Has_Element (Model (Container), Position) then
                  Prev'Result = No_Element)

               --  If the result is not No_Element, then a valid cursor is
               --  returned.
               and then (if Prev'Result /= No_Element then
                           Has_Element (Model (Container), Position));
   --  Get the previous element in the tree.
   --
   --  The previous node is retrieved in reverse depth-first order.

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
       (not Has_Element (Model (Container), Position)
        or else Is_Leaf (Model (Container), Position))

       and then
         (if First_Child'Result /= No_Element then
            --  A valid cursor is returned
            Has_Element (Model (Container), First_Child'Result)

            --  The referenced node is a child of the node at Position.
            and then Parent (Model (Container), First_Child'Result) = Position

            --  The returned child is the first child
            and then
              (for all W in Way_Type =>
                  (if W < Direction (Model (Container), First_Child'Result)
                   then Child (Model (Container), Position, W) = No_Element)));

   function First_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Pre => Has_Element (Container, Position)
            and then Has_Children (Model (Container), Position),
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
       (not Has_Element (Model (Container), Position)
        or else Is_Leaf (Model (Container), Position))

       and then
         (if Last_Child'Result /= No_Element then
            --  A valid cursor is returned
            Has_Element (Model (Container), Last_Child'Result)

            --  The referenced node is a child of the node at Position.
            and then Parent (Model (Container), Last_Child'Result) = Position

            --  The returned child is the last child
            and then
              (for all W in Way_Type =>
                 (if W > Direction (Model (Container), Last_Child'Result) then
                    Child (Model (Container), Position, W) = No_Element)));

   function Last_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Pre => Has_Element (Model (Container), Position)
            and then Has_Children (Model (Container), Position),
     Post => Equivalent_Elements
               (Last_Child_Element'Result,
                Element (Container, Last_Child (Container, Position)));

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
       (Has_Element (Model (Container), Position)
        and then not Is_Root (Model (Container), Position)
        and then (for some W in Way_Type =>
                    W > Direction (Model (Container), Position)
                    and then Has_Sibling (Model (Container), Position, W)))

       --  If a non-null cursor is returned, then that cursor references the
       --  next sibling node.
       and then
         (if Next_Sibling'Result /= No_Element then
            Has_Element (Container, Next_Sibling'Result)

            --  The result has the same parent as the node at Position
            and then Parent (Model (Container), Next_Sibling'Result) =
                     Parent (Model (Container), Position)

            --  A next sibling is returned
            and then Direction (Model (Container), Next_Sibling'Result) >
                     Direction (Model (Container), Position)

            --  The returned sibling is the closest next one.
            --  That is, there are no other siblings between the returned
            --  one and the one at Position.
            and then
              (for all W in Way_Type =>
                 (if W < Direction (Model (Container), Next_Sibling'Result)
                    and then W > Direction (Model (Container), Position)
                  then not Has_Sibling (Model (Container), Position, W))));

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
       (Has_Element (Model (Container), Position)
        and then not Is_Root (Model (Container), Position)
        and then (for some W in Way_Type =>
                    W < Direction (Model (Container), Position)
                    and then Has_Sibling (Model (Container), Position, W)))

       --  If a non-null cursor is returned, then that cursor references the
       --  previous sibling node.
       and then
         (if Prev_Sibling'Result /= No_Element then
            Has_Element (Model (Container), Prev_Sibling'Result)

            --  The result has the same parent as the node at Position
            and then Parent (Model (Container), Prev_Sibling'Result) =
                     Parent (Model (Container), Position)

            --  A previous sibling is returned
            and then Direction (Model (Container), Prev_Sibling'Result) <
                     Direction (Model (Container), Position)

            --  The returned sibling is the closest previous one.
            --  That is, there are no other siblings between the returned
            --  one and the one at Position.
            and then
              (for all W in Way_Type =>
                 (if W > Direction (Model (Container), Prev_Sibling'Result)
                    and then W < Direction (Model (Container), Position)
                  then not Has_Sibling (Model (Container), Position, W))));

   function Root_Element (Container : Tree) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   => Root_Element'Result = Element (Container, Root (Container));
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
     Post   => (if not Has_Element (Model (Container), Position)
                then Parent'Result = No_Element
                else Parent'Result = Parent (Model (Container), Position));
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
     Post   => (if Has_Element (Model (Container), Position)
                then Child'Result = Child (Model (Container), Position, Way)
                else Child'Result = No_Element);
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
     Post   =>
       (if not Has_Element (Model (Container), Position)
        then Direction'Result = Way_Type'First
        else Direction'Result = Direction (Model (Container), Position));
   --  Get the direction (way) to the node at the given Position from its
   --  parent node.

   function Is_Ancestor
     (Container : Tree;
      Ancestor  : Cursor;
      Child     : Cursor)
      return Boolean
   with
     Global => null,
     Pre    => Has_Element (Model (Container), Ancestor)
               and then Has_Element (Model (Container), Child),
     Post   =>
       Is_Ancestor'Result = Is_Ancestor (Model (Container), Ancestor, Child);
   --  Query if Ancestor is an ancestor node of Child.

   function Depth
     (Container : Tree;
      Position  : Cursor)
      return Count_Type
   with
     Global => null,
     Pre    => Position /= No_Element,
     Post   => Depth'Result = Depth (Model (Container), Position);
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
       and then not Has_Children (Model (Container), Root (Container));

   procedure Insert_Child
     (Container : in out Tree;
      New_Item  :        Element_Type;
      Position  :        Cursor;
      Way       :        Way_Type)
   with
     Global => null,
     Pre    => Has_Element (Model (Container), Position)
               and then Length (Container) < Count_Type'Last
               and then Child (Model (Container), Position, Way) = No_Element,
     Post   =>
       --  The length of the container has incremented
       Length (Container) = Length (Container'Old) + 1

       --  All previous nodes are still in the tree
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
                Has_Element (Model (Container),     To_Cursor (I))))

       --  The paths to all previous nodes is unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               Path (Model (Container), To_Cursor (I)) =
                 Path (Model (Container'Old), To_Cursor (I))))

       --  The parents of all previous nodes are unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               Parent (Model (Container), To_Cursor (I)) =
                 Parent (Model (Container'Old), To_Cursor (I))))

       --  The direction to each node from its parent is unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               Direction (Model (Container), To_Cursor (I)) =
                 Direction (Model (Container'Old), To_Cursor (I))))

       --  The children of all nodes is unchanged, except for the node
       --  at the specified Position.
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               (if To_Cursor (I) /= Position then
                  Children (Model (Container), To_Cursor (I)) =
                    Children (Model (Container'Old), To_Cursor (I)))))

       --  The children of the node at Position are unchanged, except for
       --  the child at the specified Way.
       and then
         (for all W in Way_Type =>
            (if Child (Model (Container'Old), Position, W) /= No_Element
                or else W = Way
             then Child (Model (Container'Old), Position, W) /= No_Element
             else Child (Model (Container'Old), Position, W) = No_Element))

       --  The child of Position at the specified Way is now a valid node
       and then Has_Element (Model (Container),
                             Child (Model (Container), Position, Way))

       --  The new child has the specified element
       and then
         Equivalent_Elements
           (Element (Container, Child (Model (Container), Position, Way)),
            New_Item)

       --  All previous elements are unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               Equivalent_Elements
                 (Element (Container,     To_Cursor (I)),
                  Element (Container'Old, To_Cursor (I)))))

       --  The root of the tree has not changed.
       and then Root (Container) = Root (Container'Old);

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

       --  The new node has the specified element
       and then
         Equivalent_Elements
           (New_Item,
            Element (Container, Parent (Model (Container), Position)))

       --  All previous nodes still exist
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
                Has_Element (Model (Container),     To_Cursor (I))))

       --  All previously existing elements are unmodified
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               Equivalent_Elements
                 (Element (Container,     To_Cursor (I)),
                  Element (Container'Old, To_Cursor (I)))))

       --  All nodes not in the subtree rooted by the node at Position,
       --  or the previous parent of Position, are unchanged.
       and then
         (for all I in Valid_Cursor_Range =>
            (if not In_Subtree (Model (Container'Old), Position, To_Cursor (I))
                and then
                  To_Cursor (I) /= Parent (Model (Container'Old), Position)
             then Model (Container) (I) = Model (Container'Old) (I)))

       --  The paths to all nodes not in the affected subtree are unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               (if not In_Subtree
                         (Model (Container'Old), Position, To_Cursor (I))
                then Path (Model (Container),     To_Cursor (I)) =
                     Path (Model (Container'Old), To_Cursor (I)))))

       --  The parents of all nodes is unchanged, except for the node at
       --  Position.
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I))
                and then To_Cursor (I) /= Position
             then Parent (Model (Container), To_Cursor (I)) =
                  Parent (Model (Container'Old), To_Cursor (I))))

       --  The children of all nodes in the tree are unchanged, except for
       --  the old parent of the node at Position.
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container'Old), To_Cursor (I)) then
               (if To_Cursor (I) /= Parent (Model (Container'Old), Position)
                then Children (Model (Container),     To_Cursor (I)) =
                     Children (Model (Container'Old), To_Cursor (I)))))

       --  The children of the old parent of Position are unchanged, except
       --  for the child at Position which no longer references Position.
       and then
         (if not Is_Root (Model (Container), Position) then
            (for all W in Way_Type =>
               (if W /= Direction (Model (Container'Old), Position) then
                  Child (Model (Container),
                         Parent (Model (Container'Old), Position),
                         W)
                  = Child (Model (Container'Old),
                           Parent (Model (Container'Old), Position),
                           W))))

       --  The parent of the node at Position is now its grandparent
       and then
         Parent (Model (Container), Parent (Model (Container), Position)) =
           Parent (Model (Container'Old), Position)

       --  If the node at Position was the root, then it is not longer the root
       --  and its parent is the root. Otherwise, the root is unchanged.
       and then
         (if Is_Root (Model (Container'Old), Position)
          then not Is_Root (Model (Container), Position)
               and then Is_Root (Model (Container),
                                 Parent (Model (Container), Position))
               and then Root (Container) = Parent (Model (Container), Position)
          else Root (Container) = Root (Container'Old));
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

   procedure Splice_Subtree
     (Container    : in out Tree;
      Subtree_Root :        Cursor;
      New_Parent   :        Cursor;
      Way          :        Way_Type)
   with
     Global => null,
     Pre    =>
       Has_Element (Container, Subtree_Root)
       and then Has_Element (Container, New_Parent)

       --  The child slot in the new parent must be empty
       --  (cannot overwrite an existing node).
       and then Child (Model (Container), New_Parent, Way) = No_Element

       --  The new parent cannot be in the subtree that is being moved
       --  as this would lead to a cycle in the tree.
       and then not In_Subtree (Model (Container), New_Parent, Subtree_Root),
     Post =>
       --  No nodes are added or removed
       Length (Container) = Length (Container'Old)
       and then (for all I in Valid_Cursor_Range =>
                   Has_Element (Model (Container),     To_Cursor (I)) =
                   Has_Element (Model (Container'Old), To_Cursor (I)))

       --  All elements are unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if Has_Element (Model (Container), To_Cursor (I)) then
               Equivalent_Elements
                 (Element (Container,     To_Cursor (I)),
                  Element (Container'Old, To_Cursor (I)))))

       --  The path to all nodes not in the subtree is unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if not In_Subtree
                      (Model (Container'Old), Subtree_Root, To_Cursor (I))
             then Path (Model (Container), To_Cursor (I)) =
                  Path (Model (Container'Old), To_Cursor (I))))

       --  The parent of all nodes is unchanged, except for Subtree_Root
       and then
         (for all I in Valid_Cursor_Range =>
            (if To_Cursor (I) /= Subtree_Root then
               Parent (Model (Container),     To_Cursor (I)) =
               Parent (Model (Container'Old), To_Cursor (I))))

       --  The subtree root has a new parent
       and then Parent (Model (Container), Subtree_Root) = New_Parent

       --  The paths of all nodes not in the subtree are unchanged
       and then
         (for all I in Valid_Cursor_Range =>
            (if not In_Subtree
                      (Model (Container'Old), Subtree_Root, To_Cursor (I))
             then Path (Model (Container),     To_Cursor (I)) =
                  Path (Model (Container'Old), To_Cursor (I))))

       --  The paths between all nodes in the subtree is preserved
       and then
         (for all I in Valid_Cursor_Range =>
            (for all J in Valid_Cursor_Range =>
               (if In_Subtree (Model (Container'Old),
                               Subtree_Root,
                               To_Cursor (I))
                   and then Is_Ancestor (Model (Container'Old),
                                         To_Cursor (I),
                                         To_Cursor (J))
                then Is_Ancestor (Model (Container),
                                  To_Cursor (I),
                                  To_Cursor (J)))))

       --  If the subtree is being spliced to a different parent, then the
       --  subtree root is removed as the child of its old parent.
       and then
         (if Parent (Model (Container'Old), Subtree_Root) /= New_Parent then
            Child (Model (Container),
                   Parent (Model (Container'Old), Subtree_Root),
                   Direction (Model (Container'Old), Subtree_Root))
            = No_Element);
   --  Insert a new item as a parent of the node at the specified Position.
   --
   --  For example, splicing node C (Subtree_Root) to node B (New_Parent):
   --         A              A
   --        / \            /
   --       B   C    ===>  B
   --          / \          \
   --         D   E          C
   --                       / \
   --                      D   E

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
     Post   => --  The tree structure is preserved
               Model (Container.all'Old) = Model (At_End (Container).all)

               --  Other elements are preserved
               and then
                 (for all I in Valid_Cursor_Range =>
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