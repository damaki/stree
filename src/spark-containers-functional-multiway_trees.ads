--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with SPARK.Big_Integers;
with SPARK.Containers.Parameter_Checks;

private with Ada.Finalization;
private with SPARK.Containers.Functional.Infinite_Sequences;
private with SPARK.Containers.Formal.Unbounded_Ordered_Maps;

generic
   type Element_Type is private;
   type Way_Type is (<>);
   with function Equivalent_Elements
                   (Left, Right : Element_Type) return Boolean is "=";

   --  Ghost lemmas used to prove that "=" is an equivalence relation

   with procedure Eq_Reflexive (X : Element_Type) is null
     with Ghost;
   with procedure Eq_Symmetric (X, Y : Element_Type) is null
     with Ghost;
   with procedure Eq_Transitive (X, Y, Z : Element_Type) is null
     with Ghost;

   --  Ghost lemmas used to prove that Equivalent_Elements is an equivalence
   --  relation.

   with procedure Equivalent_Elements_Reflexive (X, Y : Element_Type) is null
     with Ghost;
   with procedure Equivalent_Elements_Symmetric (X, Y : Element_Type) is null
     with Ghost;
   with procedure Equivalent_Elements_Transitive
     (X, Y, Z : Element_Type) is null
     with Ghost;
package SPARK.Containers.Functional.Multiway_Trees with
  SPARK_Mode,
  Always_Terminates
is

   use all type SPARK.Big_Integers.Big_Integer;

   -----------
   -- Paths --
   -----------

   --  Nodes in the tree are referenced by the path taken through the tree
   --  to reach that node, starting from the root of the tree. The path is
   --  modelled as a sequence of directions taken from each node along the
   --  path.
   --
   --  For example, given a Way_Type defined as follows:
   --     type Way_Type is (Left, Right);
   --
   --  this results in two possible directions from each node:
   --        N
   --       / \
   --   Left   Right
   --
   --  then given the following binary tree:
   --
   --            1
   --           / \
   --          2   3
   --         / \
   --        4   5
   --       / \
   --      6   7
   --
   --  Then the path to node 7 is the sequence: [Left, Left, Right]

   type Path_Type is private with
     Iterable => (First       => First,
                  Has_Element => Has_Element,
                  Next        => Next,
                  Element     => Get);

   function First (Path : Path_Type) return SPARK.Big_Integers.Big_Integer with
     Inline,
     Global   => null,
     Post     => First'Result = 1,
     Annotate => (GNATprove, Inline_For_Proof);

   function Length (Path : Path_Type) return SPARK.Big_Integers.Big_Natural
   with
     Global => null;

   function Has_Element
     (Path     : Path_Type;
      Position : SPARK.Big_Integers.Big_Integer) return Boolean
   is
     (In_Range (Position, 1, Length (Path)))
   with
     Annotate => (GNATprove, Inline_For_Proof);

   function Last (Path : Path_Type) return SPARK.Big_Integers.Big_Integer is
     (Length (Path));

   function Next
     (Path     : Path_Type;
      Position : SPARK.Big_Integers.Big_Integer)
      return SPARK.Big_Integers.Big_Integer
   with
     Inline,
     Global   => null,
     Post     => Next'Result = Position + 1,
     Annotate => (GNATprove, Inline_For_Proof);

   function Get
     (Path     : Path_Type;
      Position : SPARK.Big_Integers.Big_Integer) return Way_Type
   with
     Global => null,
     Pre    => Has_Element (Path, Position);

   function Range_Equal
     (Left  : Path_Type;
      Right : Path_Type;
      Fst   : SPARK.Big_Integers.Big_Positive;
      Lst   : SPARK.Big_Integers.Big_Natural) return Boolean
   with
     Ghost,
     Global => null,
     Pre    => Lst <= Last (Left) and then Lst <= Last (Right),
     Post   =>
       Range_Equal'Result =
         (for all J in Left =>
            (if Fst <= J and then J <= Lst then
               Get (Left, J) = Get (Right, J))),
     Annotate => (GNATprove, Inline_For_Proof);

   function Range_Shifted
     (Left   : Path_Type;
      Right  : Path_Type;
      Fst    : SPARK.Big_Integers.Big_Positive;
      Lst    : SPARK.Big_Integers.Big_Natural;
      Offset : SPARK.Big_Integers.Big_Integer) return Boolean
   with
     Ghost,
     Global => null,
     Pre    => Lst <= Last (Left) and then Lst <= Last (Right),
     Post   =>
       Range_Shifted'Result =
         ((for all J in Left =>
             (if Fst <= J and then J <= Lst then
                Get (Left, J) = Get (Right, J + Offset)))
          and then
            (for all J in Right =>
               (if Fst + Offset <= J and then J <= Lst + Offset then
                  Get (Left, J - Offset) = Get (Right, J)))),
     Annotate => (GNATprove, Inline_For_Proof);

   function Path_Logic_Equal (Left, Right : Path_Type) return Boolean with
     Global   => null,
     Annotate => (GNATprove, Logical_Equal);

   overriding
   function "=" (Left, Right : Path_Type) return Boolean with
     Global => null,
     Post   => "="'Result =
                 (Length (Left) = Length (Right)
                  and then (for all I in Left =>
                              Get (Left, I) = Get (Right, I)))

               and then "="'Result = Path_Logic_Equal (Left, Right);

   function "<" (Left, Right : Path_Type) return Boolean with
     Global   => null,
     Post     =>
       "<"'Result =
         (Length (Left) < Length (Right)
          and then
            (for all I in Left =>
               Get (Left, I) = Get (Right, I))),
     Annotate => (GNATprove, Inline_For_Proof);

   function "<=" (Left, Right : Path_Type) return Boolean with
     Global   => null,
     Post     =>
       "<="'Result = (Left < Right or else Left = Right),
     Annotate => (GNATprove, Inline_For_Proof);

   function Root return Path_Type with
     Global => null,
     Post   => Length (Root'Result) = 0;

   function Is_Root (Path : Path_Type) return Boolean is
     (Length (Path) = 0)
   with
     Annotate => (GNATprove, Inline_For_Proof);
   --  Returns True if the given path references the root node

   function Parent (Path : Path_Type) return Path_Type with
   --  Get the path that references the immediate ancestor of Path
     Global => null,
     Pre    => not Is_Root (Path),
     Post   =>
       Length (Parent'Result) = Length (Path) - 1
       and then Range_Equal (Parent'Result, Path, 1, Length (Path) - 1)
       and then Parent'Result < Path;

   function Child (Path : Path_Type; Way : Way_Type) return Path_Type with
   --  Get the path that references the immediate descendant of Path in the
   --  specified direction.
     Global => null,
     Post   =>
       Length (Child'Result) = Length (Path) + 1
       and then Range_Equal (Child'Result, Path, 1, Length (Path))
       and then Get (Child'Result, Length (Child'Result)) = Way
       and then Path < Child'Result;

   function Sibling (Path : Path_Type; Way : Way_Type) return Path_Type is
     (Child (Parent (Path), Way))
   with
     Pre  => Length (Path) > 0,
     Post => Parent (Sibling'Result) = Parent (Path);

   function Way_From_Parent (Path : Path_Type) return Way_Type is
     (Get (Path, Length (Path)))
   with
     Pre  => Length (Path) > 0;

   function Is_Ancestor (Left, Right : Path_Type) return Boolean is
   --  Returns True if Left is an ancestor node of Right
     (Left < Right)
   with
     Annotate => (GNATprove, Inline_For_Proof);

   function In_Subtree (Left, Right : Path_Type) return Boolean is
   --  Returns True if Left is in the subtree rooted by Right
     (Right <= Left)
   with
     Annotate => (GNATprove, Inline_For_Proof);

   function Concat (Left, Right : Path_Type) return Path_Type with
   --  Concatenate two paths together
     Global => null,
     Post   =>
       Range_Equal
         (Left  => Left,
          Right => Concat'Result,
          Fst   => 1,
          Lst   => Length (Left))
       and then Range_Shifted
                  (Left   => Right,
                   Right  => Concat'Result,
                   Fst    => 1,
                   Lst    => Length (Right),
                   Offset => Length (Left));

   function Remove_Front
     (Path  : Path_Type;
      Count : SPARK.Big_Integers.Big_Natural) return Path_Type
   --  Remove the first Count elements from the front of a path.
   --
   --  Example: Remove_Front ([1, 2, 3, 4, 5], 3) = [4, 5]

   with
     Global => null,
     Pre    => Count <= Length (Path),
     Post   =>
       Length (Remove_Front'Result) = Length (Path) - Count
       and then Range_Shifted
                  (Left   => Remove_Front'Result,
                   Right  => Path,
                   Fst    => 1,
                   Lst    => Length (Path) - Count,
                   Offset => Count);

   function Insert
     (Path  : Path_Type;
      After : SPARK.Big_Integers.Big_Natural;
      Value : Way_Type) return Path_Type
   --  Insert an element in a path

   with
     Global => null,
     Pre    => After <= Length (Path),
     Post   => Length (Insert'Result) = Length (Path) + 1
               and then Range_Equal (Path, Insert'Result, 1, After)
               and then Get (Insert'Result, After + 1) = Value
               and then Range_Shifted
                          (Left   => Path,
                           Right  => Insert'Result,
                           Fst    => After + 1,
                           Lst    => Length (Path),
                           Offset => 1);

   function Remove
     (Path  : Path_Type;
      Index : SPARK.Big_Integers.Big_Positive) return Path_Type
   --  Remove one element from a path

   with
     Global => null,
     Pre    => Index <= Length (Path),
     Post   => Length (Remove'Result) = Length (Path) - 1
               and then Range_Equal (Path, Remove'Result, 1, Index - 1)
               and then Range_Shifted
                          (Left   => Path,
                           Right  => Remove'Result,
                           Fst    => Index + 1,
                           Lst    => Length (Path) - 1,
                           Offset => -1);

   function Splice_Path
     (Left_Subtree  : Path_Type;
      Left          : Path_Type;
      Right_Subtree : Path_Type) return Path_Type
   --  Splice a path from one subtree to another.
   --
   --  Example: Splice_Path ([A, A], [A, A, B, B], [C, C, C]) = [C, C, C, B, B]

   with
     Global => null,
     Pre    => In_Subtree (Left, Left_Subtree),
     Post   =>
       Range_Equal
         (Left  => Splice_Path'Result,
          Right => Right_Subtree,
          Fst   => 1,
          Lst   => Length (Right_Subtree))
       and then Range_Shifted
                  (Left   => Splice_Path'Result,
                   Right  => Left,
                   Fst    => Length (Right_Subtree) + 1,
                   Lst    => Length (Splice_Path'Result),
                   Offset => Length (Left_Subtree) - Length (Right_Subtree));

   procedure Lemma_Ancestor_Transitive (P1, P2, P3 : Path_Type) with
     Ghost,
     Global   => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => P1 < P2 and then P2 < P3,
     Post     => P1 < P3;

   ----------
   -- Tree --
   ----------

   type Tree is private with
     Default_Initial_Condition => Is_Empty (Tree),
     Iterable                  => (First       => Iter_First,
                                   Has_Element => Iter_has_Element,
                                   Next        => Iter_Next,
                                   Element     => Iter_Element);
   --  Maps are empty when default initialized.
   --  "For in" quantification over trees should not be used.
   --  "For of" quantification over trees iterates over paths to tree nodes.

   ----------------------
   -- Basic Operations --
   ----------------------

   function Contains
     (Container : Tree;
      Path      : Path_Type) return Boolean
   with
     Global   => null,
     Annotate => (GNATprove, Iterable_For_Proof, "Contains");

   function Choose (Container : Tree) return Path_Type with
   --  Return an arbitrary child node in a tree

     Global => null,
     Pre    => not Is_Empty (Container),
     Post   => Contains (Container, Choose'Result);

   function Get (Container : Tree; Path : Path_Type) return Element_Type with
     Global => null,
     Pre    => Contains (Container, Path);

   function Length (Container : Tree) return SPARK.Big_Integers.Big_Natural
   with
     Global => null;

   procedure Lemma_Contains_Root (Container : Tree) with
   --  A non-empty tree always contains at least the root node

     Ghost,
     Global   => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => not Is_Empty (Container),
     Post     => Contains (Container, Root);

   procedure Lemma_Contains_Transitive
     (Container : Tree;
      P1, P2    : Path_Type)
   --  If a tree contains P1, and P2 is equivalent to P1, then the tree
   --  contains P2.

   with
     Ghost,
     Global   => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => P1 = P2,
     Post     => Contains (Container, P1) = Contains (Container, P2);

   procedure Lemma_Get_Transitive
     (Container : Tree;
      P1, P2    : Path_Type)
   --  If a tree contains P1, and P2 is equivalent to P1, then the tree
   --  contains P2.

   with
     Ghost,
     Global   => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => Contains (Container, P1) and then P1 = P2,
     Post     => Element_Logic_Equal
                   (Get (Container, P1), Get (Container, P2));

   procedure Lemma_Contains_Ancestor
     (Container : Tree;
      P1, P2    : Path_Type)
   --  If a node is in the tree, then its ancestors are also in the tree

   with
     Ghost,
     Global => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => P1 < P2 and then Contains (Container, P2),
     Post     => Contains (Container, P1);

   procedure Lemma_Not_Contains_Descendants
     (Container : Tree;
      P1, P2    : Path_Type)
   --  If a node is not in the tree, then its descendants are also not in the
   --  tree.

   with
     Ghost,
     Global => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => P1 < P2 and then not Contains (Container, P1),
     Post     => not Contains (Container, P2);

   procedure Lemma_Not_Contains_Leaf_Descendants
     (Container : Tree;
      P1, P2    : Path_Type)
   --  Descendants of a leaf node are not in the tree

   with
     Ghost,
     Global => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => P1 < P2
                 and then Contains (Container, P1)
                 and then Is_Leaf (Container, P1),
     Post     => not Contains (Container, P2);

   procedure Lemma_Path_Length_Within_Container_Length
     (Container : Tree;
      Node      : Path_Type)
   --  The path to all nodes in the tree is always less than the Length of the
   --  tree.

   with
     Ghost,
     Global   => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => Contains (Container, Node),
     Post     => Length (Node) < Length (Container);

   ------------------------
   -- Property Functions --
   ------------------------

   function "<=" (Left, Right : Tree) return Boolean with
     Global => null,
     Post   => "<="'Result =
                 (for all Node of Left =>
                    Contains (Right, Node)
                    and then Get (Right, Node) = Get (Left, Node));

   overriding
   function "=" (Left, Right : Tree) return Boolean with
     Global => null,
     Post   => "="'Result =
                 ((for all Node of Left =>
                     Contains (Right, Node)
                     and then Get (Right, Node) = Get (Left, Node))
                  and then (for all Node of Right => Contains (Left, Node)));

   function Nodes_Included (Left, Right : Tree) return Boolean with
   --  Returns True if every node in Left is in Right

     Global   => null,
     Post     => Nodes_Included'Result =
                   (for all Node of Left => Contains (Right, Node)),
     Annotate => (GNATprove, Inline_For_Proof);

   function Nodes_Included_In_Subtrees
     (Left, Right   : Tree;
      Left_Subtree  : Path_Type;
      Right_Subtree : Path_Type) return Boolean
   --  Returns True if every node in a subtree of Left is in a subtree in Right

   with
     Global => null,
     Post   =>
       Nodes_Included_In_Subtrees'Result =
         (for all Node of Left =>
            (if In_Subtree (Node, Left_Subtree) then
               Contains
                 (Right, Splice_Path (Left_Subtree, Node, Right_Subtree)))),
     Annotate => (GNATprove, Inline_For_Proof);

   function Nodes_Removed
     (Left, Right  : Tree;
      Subtree_Root : Path_Type) return Boolean
   --  Returns True if all nodes in a subtree of Left are removed from Right

   with
     Global => null,
     Post   =>
       Nodes_Removed'Result =
         (for all Node of Left =>
            (if In_Subtree (Node, Subtree_Root) then
               not Contains (Right, Node))),
     Annotate => (GNATprove, Inline_For_Proof);

   function Same_Nodes (Left, Right : Tree) return Boolean with
   --  Returns True if Left and Right have the same tree structure

     Global => null,
     Post   => Same_Nodes'Result =
                 (Nodes_Included (Left, Right)
                  and then Nodes_Included (Left => Right, Right => Left));

   function Same_Nodes_In_Subtrees
     (Left, Right   : Tree;
      Left_Subtree  : Path_Type;
      Right_Subtree : Path_Type) return Boolean
   --  Returns True if Left and Right have the same tree structure within their
   --  respective subtrees.

   with
     Global => null,
     Post   => Same_Nodes_In_Subtrees'Result =
                 (Nodes_Included_In_Subtrees
                    (Left, Right, Left_Subtree, Right_Subtree)
                  and then Nodes_Included_In_Subtrees
                             (Left          => Right,
                              Right         => Left,
                              Left_Subtree  => Right_Subtree,
                              Right_Subtree => Left_Subtree)),
     Annotate => (GNATprove, Inline_For_Proof);

   function Nodes_Included_Except_Subtree
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   with
     Global => null,
     Post   =>
       Nodes_Included_Except_Subtree'Result =
         (for all Node of Left =>
            (if not In_Subtree (Node, Excluded_Node) then
               Contains (Right, Node))),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Returns True if Left contains only nodes of Right, except for
   --  Excluded_Node and its descendants.

   pragma Warnings (Off, "unused variable ""Node""");
   function Is_Empty (Container : Tree) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (for all Node of Container => False)
               and then (Is_Empty'Result = (Length (Container) = 0));
   pragma Warnings (On, "unused variable ""Node""");
   --  Returns True if Container contains no nodes

   function Is_Leaf
     (Container : Tree;
      Path      : Path_Type) return Boolean
   --  Returns True if the node referenced by Path has no child nodes

   with
     Global => null,
     Pre    => Contains (Container, Path),
     Post   => Is_Leaf'Result =
                 (for all Way in Way_Type =>
                    not Contains (Container, Child (Path, Way))),
     Annotate => (GNATprove, Inline_For_Proof);

   ----------------------------
   -- Construction Functions --
   ----------------------------

   --  For better efficiency of both proofs and execution, avoid using
   --  construction functions in annotations and rather use property functions.

   function Empty_Tree return Tree with
     Global => null,
     Post   => Is_Empty (Empty_Tree'Result);

   function Add
     (Container : Tree;
      New_Item  : Element_Type;
      New_Node  : Path_Type) return Tree
   --  Return a new container with a new leaf node inserted.
   --
   --  The tree must not already contain the desired New_Node.
   --  If the node is a non-root node, then its parent must exist in the tree.

   with
     Global => null,
     Pre    => not Contains (Container, New_Node)
               and then (if not Is_Root (New_Node) then
                           Contains (Container, Parent (New_Node))),
     Post   =>
       Length (Add'Result) = Length (Container) + 1
       and then Contains (Add'Result, New_Node)
       and then Element_Logic_Equal (Get (Add'Result, New_Node),
                                     Copy_Element (New_Item))
       and then Elements_Equal (Container, Add'Result)
       and then Nodes_Included_Except_Subtree (Add'Result, Container, New_Node)
       and then Is_Leaf (Add'Result, New_Node);

   function Add_Parent
     (Container : Tree;
      New_Item  : Element_Type;
      Node      : Path_Type;
      Way       : Way_Type) return Tree
   --  Return a new container with a new node inserted between Node and its
   --  parent. The new node replaces Node as the child of Node's old parent,
   --  and has the old node as a child with the specified Way.

   with
     Global => null,
     Pre    => Contains (Container, Node),
     Post   =>
       Length (Add_Parent'Result) = Length (Container) + 1
       and then Contains (Add_Parent'Result, Node)
       and then Element_Logic_Equal
                  (Copy_Element (New_Item),
                   Get (Add_Parent'Result, Node))
       and then Subtree_Elements_Shifted
                  (Left         => Container,
                   Right        => Add_Parent'Result,
                   Subtree_Root => Node,
                   Way          => Way)
       and then Elements_Equal_Except_Subtree
                  (Add_Parent'Result, Container, Node);

   function Remove (Container : Tree; Node : Path_Type) return Tree
   --  Return a new container without the specified node or its descendants

   with
     Global => null,
     Post   =>
       Length (Remove'Result) < Length (Container)
       and then (if Is_Leaf (Container, Node) then
                   Length (Remove'Result) = Length (Container) - 1)
       and then Nodes_Removed (Container, Remove'Result, Node)
       and then Elements_Equal (Remove'Result, Container)
       and then Elements_Equal_Except_Subtree (Container, Remove'Result, Node);

   function Set
     (Container : Tree;
      Node      : Path_Type;
      New_Item  : Element_Type) return Tree
   --  Returns Container, where the element associated with Node has been
   --  replaced by New_Item.

   with
     Global => null,
     Pre    => Contains (Container, Node),
     Post   =>
       Length (Set'Result) = Length (Container)
       and then Element_Logic_Equal (Get (Set'Result, Node),
                                     Copy_Element (New_Item))
       and then Same_Nodes (Container, Set'Result)
       and then Elements_Equal_Except (Container, Set'Result, Node);

   -----------------------------------
   -- Iteration on Functional Trees --
   -----------------------------------

   --  The Iterable aspect can be used to quantify over a functional tree.
   --  However, if it is used to create a for loop, it will not allow users to
   --  prove their loops as there is no way to speak about the elements which
   --  have or have not been traversed already in a loop invariant. The
   --  function Iterate returns an object of a type Iterable_Tree which can be
   --  used for iteration. The cursor is a functional tree containing all the
   --  elements which have not been traversed yet. The current element being
   --  traversed being the result of Choose on this tree.

   type Iterable_Tree is private with
     Iterable =>
       (First       => First,
        Has_Element => Has_Element,
        Next        => Next,
        Element     => Element);

   function Tree_Logic_Equal (Left, Right : Tree) return Boolean with
     Ghost,
     Annotate => (GNATprove, Logical_Equal);
   --  Logical equality on trees

   function Iterate (Container : Tree) return Iterable_Tree with
     Global => null,
     Post   => Tree_Logic_Equal (Get_Tree (Iterate'Result), Container);
   --  Return an iterator over a functional tree

   function Get_Tree (Iterator : Iterable_Tree) return Tree with
     Global => null;
   --  Retrieve the tree associated with an iterator

   function Valid_Subtree
     (Iterator : Iterable_Tree;
      Cursor   : Tree) return Boolean
   with
     Global => null,
     Post   =>
         Valid_Subtree'Result = Elements_Equal (Cursor, Get_Tree (Iterator)),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Return True on all trees which can be reached by iterating over
   --  Container.

   function Element
     (Iterator : Iterable_Tree;
      Cursor   : Tree) return Path_Type
   with
     Global => null,
     Pre    => not Is_Empty (Cursor),
     Post   => Element'Result = Choose (Cursor),
     Annotate => (GNATprove, Inline_For_Proof);
   --  The next element to be considered for the iteration is the result of
   --  choose on Cursor.

   function First (Iterator : Iterable_Tree) return Tree with
     Global => null,
     Post   => Tree_Logic_Equal (First'Result, Get_Tree (Iterator))
       and then Valid_Subtree (Iterator, First'Result);
   --  In the first iteration, the cursor is the tree associated with Iterator

   function Next (Iterator : Iterable_Tree; Cursor : Tree) return Tree with
     Global => null,
     Pre    => Valid_Subtree (Iterator, Cursor) and then not Is_Empty (Cursor),
     Post   =>
       Valid_Subtree (Iterator, Next'Result)
       and then Length (Next'Result) = Length (Cursor) - 1
       and then not Contains (Next'Result, Choose (Cursor))
       and then (for all C of Cursor =>
                   (if C /= Choose (Cursor) then
                      Contains (Next'Result, C)));
   --  At each iteration, remove the node selected by Choose from the Cursor
   --  tree.

   function Has_Element
     (Iterator : Iterable_Tree;
      Cursor   : Tree) return Boolean
   with
     Global => null,
     Post   => Has_Element'Result =
       (Valid_Subtree (Iterator, Cursor) and then not Is_Empty (Cursor)),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Return True on non-empty trees which can be reached by iterating over
   --  Container.

   -------------------------------------------------------------------------
   -- Ghost non-executable properties used only in internal specification --
   -------------------------------------------------------------------------

   --  Logical equality on elements cannot be safely executed on most element
   --  types. Thus, this package should only be instantiated with ghost code
   --  disabled. This is enforced by having a special imported procedure
   --  Check_Or_Fail that will lead to link-time errors otherwise.

   function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
   with
     Ghost,
     Global => null,
     Annotate => (GNATprove, Logical_Equal);

   function Elements_Equal (Left, Right : Tree) return Boolean
   --  Returns True if all the nodes of Left have the same elements in Right

   with
     Ghost,
     Global => null,
     Post   =>
       Elements_Equal'Result =
         (for all Node of Left =>
            Contains (Right, Node)
            and then Element_Logic_Equal
                       (Get (Left, Node), Get (Right, Node)));

   function Elements_Equal_In_Subtrees
     (Left, Right   : Tree;
      Left_Subtree  : Path_Type;
      Right_Subtree : Path_Type) return Boolean
   --  Returns True if all nodes in a subtree of Left have the same elements as
   --  the equivalent nodes in a subtree of Right.

   with
     Ghost,
     Global => null,
     Post   =>
       Elements_Equal_In_Subtrees'Result =
         (for all Node of Left =>
            (if In_Subtree (Node, Left_Subtree) then
               Contains (Right,
                         Splice_Path (Left_Subtree, Node, Right_Subtree))
               and then
                 Element_Logic_Equal
                   (Get (Left, Node),
                    Get (Right,
                         Splice_Path (Left_Subtree, Node, Right_Subtree)))));

   function Elements_Equal_Except
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   --  Returns True if all elements in Left are equal to the equivalent node in
   --  Right, except for the element at Excluded_Node in Left which is not
   --  checked.

   with
     Ghost,
     Global => null,
     Post   =>
       Elements_Equal_Except'Result =
         (for all Node of Left =>
            (if Excluded_Node /= Node then
               Contains (Right, Node)
               and then Element_Logic_Equal
                          (Get (Left, Node), Get (Right, Node))));

   function Elements_Equal_Except_Subtree
     (Left         : Tree;
      Right        : Tree;
      Subtree_Root : Path_Type) return Boolean
   --  Returns True if all elements in Left are equal to the equivalent node in
   --  Right, except for elements in Left that are within the subtree rooted at
   --  Subtree_Root.

   with
     Ghost,
     Global => null,
     Post   =>
       Elements_Equal_Except_Subtree'Result =
         (for all Node of Left =>
            (if not In_Subtree (Node, Subtree_Root) then
               Contains (Right, Node)
               and then Element_Logic_Equal
                          (Get (Left, Node), Get (Right, Node)))),
     Annotate => (GNATprove, Inline_For_Proof);

   function Subtree_Elements_Shifted
     (Left         : Tree;
      Right        : Tree;
      Subtree_Root : Path_Type;
      Way          : Way_Type) return Boolean
   --  Compares elements of two subtrees for equivalence, where the subtree in
   --  the Left tree is rooted at Subtree_Root, and the subtree in Right is
   --  rooted at Child (Subtree_Root, Way), i.e. the Right subtree is shifted
   --  down by one extra Way.

   with
     Ghost,
     Global => null,
     Post   =>
       Subtree_Elements_Shifted'Result =
         ((for all Node of Left =>
             (if In_Subtree (Node, Subtree_Root) then
                Contains (Right, Insert (Node, Length (Subtree_Root), Way))
                and then
                  Element_Logic_Equal
                    (Get (Left, Node),
                     Get (Right, Insert (Node, Length (Subtree_Root), Way)))))

          and then
            (for all Node of Right =>
               (if In_Subtree (Node, Child (Subtree_Root, Way)) then
                  Contains (Left, Remove (Node, Length (Subtree_Root) + 1))
                  and then
                    Element_Logic_Equal
                      (Get (Left, Remove (Node, Length (Subtree_Root) + 1)),
                       Get (Right, Node))))),
       Annotate => (GNATprove, Inline_For_Proof);

   --------------------------
   -- Instantiation Checks --
   --------------------------

   --  Check that the actual parameters follow the appropriate assumptions.

   function Copy_Element (Item : Element_Type) return Element_Type is (Item)
     with Annotate => (GNATprove, Inline_For_Proof);
   --  Elements of containers are copied by numerous primitives in this
   --  package. This function causes GNATprove to verify that such a copy is
   --  valid (in particular, it does not break the ownership policy of SPARK,
   --  i.e. it does not contain pointers that could be used to alias mutable
   --  data).
   --  This function is also used to model the value of new elements after
   --  insertion inside the container. Indeed, a copy of an object might not
   --  be logically equal to the object, in particular in case of view
   --  conversions of tagged types.

   package Eq_Checks is new
     SPARK.Containers.Parameter_Checks.Equivalence_Checks
       (T                   => Element_Type,
        Eq                  => "=",
        Param_Eq_Reflexive  => Eq_Reflexive,
        Param_Eq_Symmetric  => Eq_Symmetric,
        Param_Eq_Transitive => Eq_Transitive);
   --  Check that the actual parameter for "=" is an equivalence relation

   package Eq_Elements_Checks is new
     SPARK.Containers.Parameter_Checks.Equivalence_Checks_Eq
       (T                     => Element_Type,
        Eq                    => Equivalent_Elements,
        "="                   => "=",
        Param_Equal_Reflexive => Eq_Checks.Eq_Reflexive,
        Param_Eq_Reflexive    => Equivalent_Elements_Reflexive,
        Param_Eq_Symmetric    => Equivalent_Elements_Symmetric,
        Param_Eq_Transitive   => Equivalent_Elements_Transitive);
   --  Check that the actual parameter for Equivalent_Elements is an
   --  equivalence relation with respect to the equality "=".

   --------------------------------------------------
   -- Iteration Primitives Used For Quantification --
   --------------------------------------------------

   type Private_Path is private;

   function Iter_First (Container : Tree) return Private_Path with
     Global => null;

   function Iter_Has_Element
     (Container : Tree;
      Path      : Private_Path) return Boolean
   with
     Global => null;

   function Iter_Next
     (Container : Tree;
      Path      : Private_Path) return Private_Path
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Path);

   function Iter_Element
     (Container : Tree;
      Path : Private_Path) return Path_Type
   with
     Global => null,
     Pre    => Iter_Has_Element (Container, Path);

private

   pragma SPARK_Mode (Off);

   package WS is new SPARK.Containers.Functional.Infinite_Sequences
     (Element_Type => Way_Type);

   type Path_Type is record
      Seq : WS.Sequence;
   end record;

   function Path_Logic_Equal (Left, Right : Path_Type) return Boolean is
     (WS."=" (Left.Seq, Right.Seq));

   overriding
   function "=" (Left, Right : Path_Type) return Boolean is
     (WS."=" (Left.Seq, Right.Seq));

   function "<" (Left, Right : Path_Type) return Boolean is
     (WS."<" (Left.Seq, Right.Seq));

   function "<=" (Left, Right : Path_Type) return Boolean is
     (WS."<=" (Left.Seq, Right.Seq));

   function Length (Path : Path_Type) return SPARK.Big_Integers.Big_Natural is
     (WS.Length (Path.Seq));

   function Parent (Path : Path_Type) return Path_Type is
     (Path_Type'(Seq => WS.Remove (Path.Seq, Length (Path))));

   function Child (Path : Path_Type; Way : Way_Type) return Path_Type is
     (Path_Type'(Seq => WS.Add (Path.Seq, Way)));

   function Root return Path_Type is
     (Path_Type'(Seq => WS.Empty_Sequence));

   --------------
   -- Elements --
   --------------

   type Element_Access is access Element_Type;

   type Refcounted_Element is record
      Refcount : Natural        := 0;
      Element  : Element_Access := null;
   end record;

   type Refcounted_Element_Access is access Refcounted_Element;

   type Controlled_Element_Access is new Ada.Finalization.Controlled
   with record
      Ref : Refcounted_Element_Access;
   end record;

   overriding
   procedure Adjust (C_E : in out Controlled_Element_Access);

   overriding
   procedure Finalize (C_E : in out Controlled_Element_Access);

   ----------------
   -- Tree Nodes --
   ----------------

   type Node_Type;

   type Node_Access is access Node_Type;
   subtype Not_Null_Node_Access is not null Node_Access;

   package Node_Maps is new SPARK.Containers.Formal.Unbounded_Ordered_Maps
     (Key_Type     => Way_Type,
      Element_Type => Not_Null_Node_Access);

   type Node_Type is record
      Parent          : Node_Access;
      Way_From_Parent : Way_Type;
      Children        : Node_Maps.Map;
      Element         : Controlled_Element_Access;
   end record;

   -----------
   -- Trees --
   -----------

   type Refcounted_Tree is record
      Refcount  : Natural     := 0;
      Root_Node : Node_Access := null;
   end record;

   type Refcounted_Tree_Access is access Refcounted_Tree;

   type Controlled_Tree_Access is new Ada.Finalization.Controlled with record
      Ref : Refcounted_Tree_Access := null;
   end record;

   overriding
   procedure Adjust (C_T : in out Controlled_Tree_Access);

   overriding
   procedure Finalize (C_T : in out Controlled_Tree_Access);

   type Tree is record
      Ref : Controlled_Tree_Access;
   end record;

   type Iterable_Tree is record
      Container : Tree;
   end record;

   type Private_Path is record
      Path  : Path_Type;
      Valid : Boolean;
   end record;

   function No_Element return Private_Path is
     (Private_Path'(Path  => Root,
                    Valid => False));

   function Find_Node
     (Container : Tree;
      Path      : Path_Type) return Node_Access;
   --  Find the node in the tree with the specified path.
   --
   --  Null is returned if the specified node does not exist in the tree.

   --------------------
   -- Node Iteration --
   --------------------

   --  Iteration is performed by starting at the bottom-left of the tree
   --  (i.e. going down the first branch of each node until a leaf is found),
   --  then, for each node, the descendants of the node are visited before the
   --  node itself is visited.
   --
   --  For example, consider the following binary tree:
   --
   --          9
   --         / \
   --        4   8
   --       / \   \
   --      1   3   7
   --         /   / \
   --        2   5   6
   --
   --  The nodes are visited in the order: 1, 2, 3, 4, 5, 6, 7, 8, 9.

   function First_Node
     (Node : Not_Null_Node_Access) return Not_Null_Node_Access;
   --  Get the first node in the iteration, starting at the specified root Node

   function Next_Node (Node : Not_Null_Node_Access) return Node_Access;
   --  Get the next node in the iteration after the specified Node

   procedure First_Node_In_Both
     (Left  : in out Node_Access;
      Right : in out Node_Access;
      Success :    out Boolean)
   with
     Pre => Left /= null and then Right /= null;
   --  Get the first node in the iteration in the Left tree, following the same
   --  path in Right.
   --
   --  Left and Right are updated to point to the nodes that were found.
   --
   --  Success is True if the same node found in Left also exists in Right.
   --  Otherwise, it is set to False.

   procedure First_Node_In_Both_Except
     (Left          : in out Node_Access;
      Right         : in out Node_Access;
      Excluded_Node :        Node_Access;
      Success       :    out Boolean)
   with
     Pre  => Left /= null and then Right /= null,
     Post => (if Excluded_Node /= null then Left /= Excluded_Node);
   --  Get the first node in the iteration in the Left tree, skipping over the
   --  Excluded_Node if encountered in Left. The same path is followed in
   --  Right.
   --
   --  Left and Right are updated to point to the first nodes in each tree.
   --
   --  Success is True if the same node found in Left also exists in Right.
   --  Otherwise, it is set to False.

   procedure Next_Node_In_Both
     (Left  : in out Node_Access;
      Right : in out Node_Access;
      Success :    out Boolean)
   with
     Pre => Left /= null and then Right /= null;
   --  Get the next node in the iteration in the Left tree, following the same
   --  path in Right.
   --
   --  Left and Right are updated to point to the next nodes that were found.
   --
   --  Success is True if the same node found in Left also exists in Right.
   --  Otherwise, it is set to False.

   procedure Next_Node_In_Both_Except
     (Left          : in out Node_Access;
      Right         : in out Node_Access;
      Excluded_Node :        Node_Access;
      Success       :    out Boolean)
   with
     Pre  => Left /= null and then Right /= null,
     Post => (if Excluded_Node /= null then Left /= Excluded_Node);
   --  Get the next node in the iteration in the Left tree, skipping over
   --  Excluded_Node if encountered, and following the same path in Right.
   --
   --  Left and Right are updated to point to the next nodes that were found.
   --
   --  Success is True if the same node found in Left also exists in Right.
   --  Otherwise, it is set to False.

   function Path_To_Node (Node : Not_Null_Node_Access) return Path_Type;
   --  Get the path to the specified node from the root

   --------------------------
   -- Construction Helpers --
   --------------------------

   function Copy_Tree_Except_Subtree
     (Root_Acc : Not_Null_Node_Access;
      Exclude  : Node_Access) return Controlled_Tree_Access;
   --  Create a deep copy of a tree, except for the specified Exclude node
   --  and its descendants.
   --
   --  Set Exclude to null to copy the tree in its entirety.

   function Create_Tree (New_Item : Element_Type) return Tree;
   --  Create a tree with a single root node

end SPARK.Containers.Functional.Multiway_Trees;