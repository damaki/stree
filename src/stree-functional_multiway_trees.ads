--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with SPARK.Big_Integers;
with SPARK.Containers.Functional.Infinite_Sequences;
with SPARK.Containers.Parameter_Checks;

private with Ada.Finalization;
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
package Stree.Functional_Multiway_Trees with
  SPARK_Mode,
  Always_Terminates
is

   use type SPARK.Big_Integers.Big_Integer;

   -----------
   -- Paths --
   -----------

   --  Nodes in the tree are referenced by the path taken through the tree
   --  to reach that node, starting from the root of the tree. The path is a
   --  sequence of directions taken from each node along the path.
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

   package Way_Sequences is new SPARK.Containers.Functional.Infinite_Sequences
     (Element_Type => Way_Type);
   use all type Way_Sequences.Sequence;

   subtype Path_Type is Way_Sequences.Sequence;

   Root_Node : constant Path_Type := Way_Sequences.Empty_Sequence;

   function Is_Root (Path : Path_Type) return Boolean is
     (Way_Sequences.Length (Path) = 0);
   --  Returns True if the given path references the root node

   function Parent (Path : Path_Type) return Path_Type is
   --  Get the path that references the immediate ancestor of Path
     (Way_Sequences.Remove (Path, Way_Sequences.Length (Path)))
   with
     Pre  => not Is_Root (Path),
     Post => Parent'Result < Path
             and then Length (Parent'Result) = Length (Path) - 1;

   function Child (Path : Path_Type; Way : Way_Type) return Path_Type is
   --  Get the path that references the immediate descendant of Path in the
   --  specified direction.
     (Way_Sequences.Add (Path, Way))
   with
     Post => Path < Child'Result;

   function Is_Ancestor (Left, Right : Path_Type) return Boolean is
   --  Returns True if Left is an ancestor node of Right
     (Left < Right);

   function In_Subtree (Left, Right : Path_Type) return Boolean is
   --  Returns True if Left is in the subtree rooted by Right
     (Right <= Left);

   function Concat (Left, Right : Path_Type) return Path_Type with
   --  Concatenate two paths together
     Global => null,
     Post   =>
       Range_Equal (Left, Concat'Result, 1, Length (Left))
       and then Range_Shifted
                  (Right, Concat'Result, 1, Length (Right), Length (Left));

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

   function Splice_Path
     (Left_Subtree  : Path_Type;
      Left          : Path_Type;
      Right_Subtree : Path_Type) return Path_Type
   --  Splice a path from one subtree to another.
   --
   --  Example: Splice_Path ([A, A], [A, A, B, B], [C, C, C]) = [C, C, C, B, B]

   with
     Global => null,
     Pre    => Left_Subtree <= Left,
     Post   =>
       Range_Equal (Left  => Splice_Path'Result,
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

   procedure Lemma_Contains_Root (Container : Tree) with
   --  A non-empty tree always contains at least the root node

     Ghost,
     Global   => null,
     Annotate => (GNATprove, Automatic_Instantiation),
     Pre      => not Is_Empty (Container),
     Post     => Contains (Container, Root_Node);

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

     Global => null,
     Post   => Nodes_Included'Result =
                 (for all Node of Left => Contains (Right, Node));

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
            (if Left_Subtree <= Node then
               Contains
                 (Right, Splice_Path (Left_Subtree, Node, Right_Subtree))));

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
                              Right_Subtree => Left_Subtree));

   function Nodes_Included_Except_Subtree
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   with
     Global => null,
     Post   =>
       Nodes_Included_Except_Subtree'Result =
         (for all Node of Left =>
            (if not (Excluded_Node <= Node) then
               Contains (Right, Node)));
   --  Returns True if Left contains only nodes of Right, except for
   --  Excluded_Node and its descendants.

   pragma Warnings (Off, "unused variable ""Node""");
   function Is_Empty (Container : Tree) return Boolean with
     Global => null,
     Post   => Is_Empty'Result = (for all Node of Container => False);
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
                    not Contains (Container, Child (Path, Way)));

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
       Contains (Add'Result, New_Node)
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
      Contains (Add_Parent'Result, Node)
      and then Element_Logic_Equal (New_Item, Get (Add_Parent'Result, Node))
      and then Elements_Equal_In_Subtrees
                 (Left          => Container,
                  Left_Subtree  => Node,
                  Right         => Add_Parent'Result,
                  Right_Subtree => Child (Node, Way))
      and then Elements_Equal_In_Subtrees
                 (Left          => Add_Parent'Result,
                  Left_Subtree  => Child (Node, Way),
                  Right         => Container,
                  Right_Subtree => Node)
      and then Elements_Equal_Except_Subtree
                 (Add_Parent'Result, Container, Node);

   function Remove (Container : Tree; Node : Path_Type) return Tree
   --  Return a new container without the specified node or its descendants

   with
     Global => null,
     Post   =>
       not Contains (Remove'Result, Node)
       and then Elements_Equal (Remove'Result, Container)
       and then Nodes_Included_Except_Subtree (Container, Remove'Result, Node);

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
       Element_Logic_Equal (Get (Set'Result, Node), Copy_Element (New_Item))
       and then Same_Nodes (Container, Set'Result)
       and then Elements_Equal_Except (Container, Set'Result, Node);

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
            (if Left_Subtree <= Node then
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
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   with
     Ghost,
     Global => null,
     Post   =>
       Elements_Equal_Except_Subtree'Result =
         (for all Node of Left =>
            (if not (Excluded_Node <= Node) then
               Contains (Right, Node)
               and then Element_Logic_Equal
                          (Get (Left, Node), Get (Right, Node))));

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

   type Reference_Count is new Natural with Atomic;

   --------------
   -- Elements --
   --------------

   type Element_Access is access Element_Type;

   type Refcounted_Element is record
      Refcount : aliased Reference_Count := 0;
      Element   : Element_Access         := null;
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
      Refcount  : aliased Reference_Count := 0;
      Root_Node : Node_Access             := null;
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

   type Private_Path is record
      Path  : Path_Type;
      Valid : Boolean;
   end record;

   No_Element : constant Private_Path :=
     (Path  => Way_Sequences.Empty_Sequence,
      Valid => False);

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
   --  Get the first node in the iteration in the Left tree, while checking
   --  that the same path exists in Right.
   --
   --  Success is returned if the path to the first node in the iterations
   --  of Left and Right result in the same node, and both nodes are leaf
   --  nodes. Otherwise, False is returned.

   procedure First_Node_In_Both_Except
     (Left          : in out Node_Access;
      Right         : in out Node_Access;
      Excluded_Node :        Node_Access;
      Success       :    out Boolean)
   with
     Pre  => Left /= null and then Right /= null,
     Post => (if Excluded_Node /= null then Left /= Excluded_Node);

   procedure Next_Node_In_Both
     (Left  : in out Node_Access;
      Right : in out Node_Access;
      Success :    out Boolean)
   with
     Pre => Left /= null and then Right /= null;

   procedure Next_Node_In_Both_Except
     (Left          : in out Node_Access;
      Right         : in out Node_Access;
      Excluded_Node :        Node_Access;
      Success       :    out Boolean)
   with
     Pre  => Left /= null and then Right /= null,
     Post => (if Excluded_Node /= null then Left /= Excluded_Node);

   function Path_To_Node (Node : Not_Null_Node_Access) return Path_Type;

   --------------------------
   -- Construction Helpers --
   --------------------------

   function Copy_Tree_Except_Subtree
     (Root    : Not_Null_Node_Access;
      Exclude : Node_Access) return Controlled_Tree_Access;
   --  Create a deep copy of a tree, except for the specified Exclude node
   --  and its descendants.
   --
   --  Set Exclude to null to copy the tree in its entirety.

   function Create_Tree (New_Item : Element_Type) return Tree;
   --  Create a tree with a single root node

end Stree.Functional_Multiway_Trees;