--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with SPARK.Big_Integers;                  use SPARK.Big_Integers;
with SPARK.Containers.Functional.Maps;
with SPARK.Containers.Parameter_Checks;
with SPARK.Containers.Types;              use SPARK.Containers.Types;

with SPARK.Containers.Functional.Multiway_Trees;

private with Ada.Containers.Vectors;
private with SPARK.Containers.Formal.Holders;

--  This package implements an unbounded multi-way tree container.
--
--  The order of the tree (number of "ways" from each node) is determined by
--  the range of Way_Type, which can be any discrete type (integer, modular, or
--  enumeration type).
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
--  Quantification over trees is also available using "for all" and "for some".

generic
   type Element_Type is private;
   type Way_Type is (<>);
   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   --  Ghost lemmas used to prove that "=" is an equivalence relation

   with procedure Eq_Reflexive (X : Element_Type) is null
     with Ghost;
   with procedure Eq_Symmetric (X, Y : Element_Type) is null
     with Ghost;
   with procedure Eq_Transitive (X, Y, Z : Element_Type) is null
     with Ghost;

package SPARK.Containers.Formal.Unbounded_Multiway_Trees with
  SPARK_Mode => On,
  Always_Terminates
is

   --  Contracts in this unit are meant for analysis only, not for run-time
   --  checking.

   pragma Assertion_Policy (Ignore);

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

   function Length (Container : Tree) return Count_Type with
     Global => null;
   --  Get the number of elements in the tree

   pragma Unevaluated_Use_Of_Old (Allow);

   ------------------
   -- Formal Model --
   ------------------

   package Formal_Model with Ghost is

      package Count_Type_Conversions is new
        SPARK.Big_Integers.Signed_Conversions (Count_Type);
      use Count_Type_Conversions;

      --  Logical equality cannot be safely executed on most element or key
      --  types. Thus, this package should only be instantiated with ghost code
      --  disabled. This is enforced by having a special imported procedure
      --  Check_Or_Fail that will lead to link-time errors otherwise.

      function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
      with
        Global => null,
        Annotate => (GNATprove, Logical_Equal);

      --------------------------
      -- Instantiation Checks --
      --------------------------

      package Eq_Checks is new
        SPARK.Containers.Parameter_Checks.Equivalence_Checks
          (T                   => Element_Type,
           Eq                  => "=",
           Param_Eq_Reflexive  => Eq_Reflexive,
           Param_Eq_Symmetric  => Eq_Symmetric,
           Param_Eq_Transitive => Eq_Transitive);
      --  Check that the actual parameter for "=" is an equivalence relation

      package Lift_Eq is new
        SPARK.Containers.Parameter_Checks.Lift_Eq_Reflexive
          (T                  => Element_Type,
           "="                => Element_Logic_Equal,
           Eq                 => "=",
           Param_Eq_Reflexive => Eq_Checks.Eq_Reflexive);

      ------------------
      -- Formal Model --
      ------------------

      subtype Positive_Count_Type is Count_Type range 1 .. Count_Type'Last;

      package M is new SPARK.Containers.Functional.Multiway_Trees
        (Element_Type                   => Element_Type,
         Way_Type                       => Way_Type,
         "="                            => Element_Logic_Equal,
         Equivalent_Elements            => "=",
         Eq_Reflexive                   => Eq_Reflexive,
         Eq_Symmetric                   => Eq_Symmetric,
         Eq_Transitive                  => Eq_Transitive,
         Equivalent_Elements_Reflexive  => Lift_Eq.Eq_Reflexive,
         Equivalent_Elements_Symmetric  => Eq_Checks.Eq_Symmetric,
         Equivalent_Elements_Transitive => Eq_Checks.Eq_Transitive);

      use type M.Path_Type;

      function "="  (Left, Right : M.Tree) return Boolean renames M."=";
      function "<=" (Left, Right : M.Tree) return Boolean renames M."<=";

      function Model (Container : Tree) return M.Tree with
      --  The high-level model of a tree is a tree that references tree nodes
      --  by their paths from the root. Cursors are not represented in this
      --  model.

        Ghost,
        Global => null,
        Post   =>
          Length (Container) = From_Big_Integer (M.Length (Model'Result));

      ---------------------------------
      -- Mapping of Cursors to Paths --
      ---------------------------------

      --  The functional tree does not model cursors but instead references
      --  tree nodes by the path to the node from the root as a sequence of
      --  directions (ways) taken at each node along the path.
      --
      --  To model cursors we define a mapping between cursors and paths.
      --  The mapping is injective, i.e. each valid cursor maps to a distinct
      --  path in the formal model.

      package P is new SPARK.Containers.Functional.Maps
        (Key_Type     => Cursor,
         Element_Type => M.Path_Type,
         "="          => "=");

      function Paths (Container : Tree) return P.Map with
      --  Get the mapping of cursors to paths in the model

        Ghost,
        Global => null,
        Post   =>
          not P.Has_Key (Paths'Result, No_Element)

          --  Every path references a node in the model
          and then
            (for all C of Paths'Result =>
               M.Contains (Model (Container), P.Get (Paths'Result, C)))

          --  Every node in the model has a cursor mapped to it
          and then
            (for all Node of Model (Container) =>
               (for some C of Paths'Result =>
                  P.Get (Paths'Result, C) = Node))

          --  No two cursors map to the same path
          and then
            (for all C1 of Paths'Result =>
               (for all C2 of Paths'Result =>
                  (if P.Get (Paths'Result, C1) = P.Get (Paths'Result, C2) then
                     C1 = C2)))

          --  The parent of all non-root nodes is in the tree
          and then
            (for all C1 of Paths'Result =>
               (if not M.Is_Root (P.Get (Paths'Result, C1)) then
                  (for some C2 of Paths'Result =>
                     P.Get (Paths'Result, C2) =
                       M.Parent (P.Get (Paths'Result, C1)))));

      function M_Path
        (Container : Tree;
         Position  : Cursor) return M.Path_Type
      --  Get the path in the formal model that corresponds to the specified
      --  cursor.

      with
        Ghost,
        Global   => null,
        Pre      => P.Has_Key (Paths (Container), Position),
        Post     => M_Path'Result = P.Get (Paths (Container), Position),
        Annotate => (GNATprove, Inline_For_Proof);

      function Mapping_Preserved (Left, Right : Tree) return Boolean with
      --  Returns True if, for all cursors of Left, the mapping of cursors to
      --  paths is the same in Left and Right.

        Ghost,
        Global => null,
        Post   =>
          Mapping_Preserved'Result =
            --  Right contains all the cursors of Left
            (P.Keys_Included (Paths (Left), Paths (Right))

             --  Mappings from cursors to paths induced by Left and Right are
             --  the same.
             and then
               (for all C of Paths (Left) =>
                  M_Path (Left, C) = M_Path (Right, C)));

      function Elements_Preserved (Left, Right : Tree) return Boolean with
      --  Returns True if, for all cursors of Left, the element at that cursor
      --  is equal to the element at the same cursor in Right.

        Ghost,
        Global => null,
        Post   =>
          Elements_Preserved'Result =
            (for all C of Paths (Left) =>
               P.Has_Key (Paths (Right), C)
               and then M.Get (Model (Left), M_Path (Left, C)) =
                          M.Get (Model (Right), M_Path (Right, C)));

      function Mapping_Preserved_Except_Subtree
        (Left, Right : Tree;
         Position    : Cursor) return Boolean
      --  Returns True if, for all cursors of Left, the mapping of cursors to
      --  paths is the same in Left and Right, except for cursors of Left that
      --  are in the subtree rooted at Position.

      with
        Ghost,
        Global => null,
        Post   =>
          Mapping_Preserved_Except_Subtree'Result =
             --  Right contains all the cursors of Left that are not in the
             --  subtree rooted at Position
            ((for all C of Paths (Left) =>
                (if not M.In_Subtree (M_Path (Left, C),
                                      M_Path (Left, Position))
                 then P.Has_Key (Paths (Right), C)))

             --  Mappings from cursors to paths induced by Left and Right are
             --  the same, except for nodes in the subtree rooted at Position.
             and then
               (for all C of Paths (Left) =>
                  (if not M.In_Subtree (M_Path (Left, C),
                                        M_Path (Left, Position))
                   then M_Path (Left, C) = M_Path (Right, C))));

      function Subtree_Mapping_Shifted_Down
        (Left, Right  : Tree;
         Subtree_Root : M.Path_Type;
         Way          : Way_Type) return Boolean
      --  Returns True if all cursors of Left that are in the subtree rooted at
      --  Subtree_Root are shifted down by one position, specified by Way.
      --
      --  For example, given:
      --   * a binary tree with ways L and R;
      --   * Subtree_Root = [L, L, L]; and
      --   * Way = R.
      --
      --  then the path to the subtree is remapped to [L, L, L, R] so that an
      --  arbitrary cursor in the subtree, e.g. previously mapping to
      --  [L, L, L, L, L], is remapped to [L, L, L, R, L, L].

      with
        Ghost,
        Global => null,
        Post   =>
          Subtree_Mapping_Shifted_Down'Result =
            ((for all C of Paths (Left) =>
                (if M.In_Subtree (M_Path (Left, C), Subtree_Root) then
                   P.Has_Key (Paths (Right), C)))

             and then
               (for all C of Paths (Left) =>
                  (if M.In_Subtree (M_Path (Left, C), Subtree_Root) then
                     M_Path (Right, C) =
                       M.Insert (Path  => M_Path (Left, C),
                                 After => M.Length (Subtree_Root),
                                 Value => Way))));

      function Subtree_Mapping_Shifted_Up
        (Left, Right  : Tree;
         Subtree_Root : M.Path_Type) return Boolean
      --  Returns True if all cursors of Left that are in the subtree rooted at
      --  Subtree_Root are shifted up by one position.
      --
      --  For example, given:
      --   * a binary tree with ways L and R; and
      --   * Subtree_Root = [L, L, L].
      --
      --  then the path to the subtree is remapped to [L, L] so that an
      --  arbitrary cursor in the subtree, e.g. previously mapping to
      --  [L, L, L, R, R], is remapped to [L, L, R, R].

      with
        Ghost,
        Global => null,
        Pre    => M.Length (Subtree_Root) > 0,
        Post   =>
          Subtree_Mapping_Shifted_Up'Result =
            ((for all C of Paths (Left) =>
                (if M.In_Subtree (M_Path (Left, C),
                                  Subtree_Root)
                 then P.Has_Key (Paths (Right), C)))

             and then
               (for all C of Paths (Left) =>
                  (if M.In_Subtree (M_Path (Left, C),
                                    Subtree_Root)
                   then M_Path (Right, C) =
                          M.Remove (Path  => M_Path (Left, C),
                                    Index => M.Length (Subtree_Root)))));

      function Subtree_Remapped
        (Left, Right : Tree;
         Old_Subtree : M.Path_Type;
         New_Subtree : M.Path_Type) return Boolean
      --  Returns True if all cursors of Left that are in the subtree rooted at
      --  Old_Subtree are remapped to the same position with New_Subtree.
      --
      --  For example, given:
      --   * a binary tree with ways L and R;
      --   * Old_Subtree = [L, L]; and
      --   * New_Subtree = [R, R].
      --
      --  Then for an arbitrary cursor in Left which maps to path [L, L, R, L],
      --  that same cursor now maps to [R, R, R, L] in Right.

      with
        Ghost,
        Global => null,
        Post   =>
          Subtree_Remapped'Result =
             --  Right contains all the cursors of Left that are in the
             --  subtree rooted at Old_Subtree
            ((for all C of Paths (Left) =>
                (if M.In_Subtree (M_Path (Left, C), Old_Subtree) then
                   P.Has_Key (Paths (Right), C)))

             --  Cursors in Old_Subtree in Left are now remapped to New_Subtree
             --  in Right.
             and then
               (for all C of Paths (Left) =>
                  (if M.In_Subtree (M_Path (Left, C), Old_Subtree) then
                     M_Path (Right, C) =
                       M.Splice_Path
                         (Old_Subtree, M_Path (Left, C), New_Subtree))));

      function Same_Mapping_Except
        (Left, Right : Tree;
         Position    : Cursor) return Boolean
      --  Returns true if the mapping of cursors to paths is the same in Left
      --  and Right, except for the node at Position.

      with
        Ghost,
        Global => null,
        Post   =>
          Same_Mapping_Except'Result =
             --  Right contains the same cursors of Left, except for the
             --  one node at Position.
            (P.Keys_Included_Except (Paths (Left), Paths (Right), Position)
             and then
               P.Keys_Included_Except (Paths (Right), Paths (Left), Position)

             --  Mappings from cursors to paths induced by Left and Right are
             --  the same.
             and then
               (for all C of Paths (Left) =>
                  (if C /= Position then
                     M_Path (Left, C) = M_Path (Right, C))));

      function Same_Mapping_Except_Subtree
        (Left, Right : Tree;
         Position    : Cursor) return Boolean
      --  Right has the same mapping as Left, except for the subtree rooted at
      --  Position in Left which is removed in Right.

      with
        Ghost,
        Global => null,
        Post   =>
          Same_Mapping_Except_Subtree'Result =
             --  Right contains all the cursors of Left, except for nodes in
             --  the subtree rooted at Position
            ((for all C of Paths (Left) =>
                P.Has_Key (Paths (Right), C) =
                  not M.In_Subtree (M_Path (Left, C), M_Path (Left, Position)))

             and then P.Keys_Included (Paths (Right), Paths (Left))

             --  Mappings from cursors to paths induced by Left and Right are
             --  the same.
             and then
               (for all C of Paths (Left) =>
                  (if not M.In_Subtree
                           (M_Path (Left, C), M_Path (Left, Position))
                   then M_Path (Left, C) = M_Path (Right, C))));

      function Ancestry_Preserved (Left, Right : Tree) return Boolean with
        Ghost,
        Global => null,
        Post   =>
          Ancestry_Preserved'Result =
            (for all C1 of Paths (Left) =>
               P.Has_Key (Paths (Right), C1)
               and then M_Path (Left, C1) <= M_Path (Right, C1)
               and then
                 (for all C2 of Paths (Left) =>
                    (if M_Path (Left, C1) <= M_Path (Left, C2) then
                       P.Has_Key (Paths (Right), C2)
                       and then M_Path (Right, C1) <= M_Path (Right, C2)))),
        Annotate => (GNATprove, Inline_For_Proof);

      ----------------------
      -- Cursor Positions --
      ----------------------

      package S is new SPARK.Containers.Functional.Maps
        (Key_Type                       => Cursor,
         Element_Type                   => Positive_Count_Type,
         Equivalent_Keys                => "=");

      function Positions (Container : Tree) return S.Map with
      --  The Positions map is used to model the order in which nodes are
      --  visited during iteration. It maps all valid cursors to their position
      --  in the iteration sequence.

        Ghost,
        Global => null,
        Post   =>
          --  Every cursor in the tree is in the map
          (for all C of Paths (Container) =>
             S.Has_Key (Positions'Result, C))

          --  Every cursor in the map is in the tree
          and then
            (for all C of Positions'Result =>
               P.Has_Key (Paths (Container), C))

          --  Every position is within the length of the container
          and then
            (for all C of Positions'Result =>
               S.Get (Positions'Result, C) <= Length (Container))

          --  Each cursor has a distinct position, i.e. there are no duplicates
          and then
            (for all C1 of Positions'Result =>
               (for all C2 of Positions'Result =>
                  (if S.Get (Positions'Result, C1) =
                      S.Get (Positions'Result, C2)
                   then C1 = C2)))

          --  Ancestors are always visited before their descendants
          --  (depth-first property).
          and then
            (for all C1 of Positions'Result =>
               (for all C2 of Positions'Result =>
                  (if M.Is_Ancestor (M_Path (Container, C1),
                                     M_Path (Container, C2))
                   then S.Get (Positions'Result, C1) <
                          S.Get (Positions'Result, C2))));

      function Position_Of
        (Container : Tree;
         Position  : Cursor) return Positive_Count_Type
      --  Get the position of a cursor in the iteration sequence

      is
        (S.Get (Positions (Container), Position))
      with
        Ghost,
        Global   => null,
        Pre      => P.Has_Key (Paths (Container), Position),
        Annotate => (GNATprove, Inline_For_Proof);

   end Formal_Model;
   use Formal_Model;
   use type M.Path_Type;

   overriding
   function "=" (Left, Right : Tree) return Boolean with
     Global => null,
     Post   => "="'Result =
                 (M.Same_Nodes (Model (Left), Model (Right))
                  and then Mapping_Preserved (Left, Right)
                  and then Mapping_Preserved (Left => Right, Right => Left));

   function Is_Empty (Container : Tree) return Boolean with
     Global   => null,
     Post     => Is_Empty'Result = M.Is_Empty (Model (Container)),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if the tree contains no elements

   function Has_Element
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global   => null,
     Post     => Has_Element'Result = P.Has_Key (Paths (Container), Position),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if the tree has an element at the specified cursor

   function Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Element'Result = M.Get (Model (Container),
                               M_Path (Container, Position)),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Get the element at the specified position in the tree

   procedure Replace_Element
     (Container : in out Tree;
      Position  :        Cursor;
      New_Item  :        Element_Type)
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   => Model (Container) =
                 M.Set (Model (Container'Old),
                        M_Path (Container, Position),
                        New_Item)

               and then Mapping_Preserved
                          (Left  => Container,
                           Right => Container'Old)
               and then Mapping_Preserved
                          (Left  => Container'Old,
                           Right => Container);

   function Is_Root
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Is_Root'Result =
                 (Has_Element (Container, Position)
                  and then M_Path (Container, Position) = M.Root),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if a cursor references the root of the tree.

   function Is_Leaf
     (Container : Tree;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post   => Is_Leaf'Result =
                 (Has_Element (Container, Position)
                  and then M.Is_Leaf (Model (Container),
                                      M_Path (Container, Position))),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if a cursor references a leaf node in the tree.
   --
   --  A leaf node is any node in the tree that has no children.

   function Root (Container : Tree) return Cursor with
     Global => null,
     Contract_Cases =>
       (Is_Empty (Container) => Root'Result = No_Element,
        others               =>
          Is_Root (Container, Root'Result)
          and then S.Get (Positions (Container), Root'Result) = 1);
   --  Get a cursor to the root of the tree.
   --
   --  No_Element is returned iff the tree is empty, otherwise a cursor to the
   --  root node is returned.

   function First (Container : Tree) return Cursor renames Root;
   --  Get a cursor to the first element in the tree.

   function First_Element (Container : Tree) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   => First_Element'Result = Element (Container, First (Container));
   --  Get the first element in the tree.

   function Last (Container : Tree) return Cursor with
     Global => null,
     Contract_Cases =>
       (Is_Empty (Container) => Last'Result = No_Element,
        others               =>
          Has_Element (Container, Last'Result)
          and then
            S.Get (Positions (Container), Last'Result) = Length (Container));
   --  Get a cursor to the last element in the tree.

   function Last_Element (Container : Tree) return Element_Type is
     (Element (Container, Last (Container)))
   with
     Global => null,
     Pre    => not Is_Empty (Container),
     Post   => Last_Element'Result = Element (Container, Last (Container));
   --  Get the last element in the tree.

   function Next
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Contract_Cases =>
       (not Has_Element (Container, Position)
        or else S.Get (Positions (Container), Position) = Length (Container)
        =>
          Next'Result = No_Element,

        others =>
          Has_Element (Container, Next'Result)
          and then S.Get (Positions (Container), Next'Result) =
                     S.Get (Positions (Container), Position) + 1);
   --  Get the next element in the tree.
   --
   --  The next node is retrieved in depth-first order.

   function First_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global         => null,
     Contract_Cases =>
       (not Has_Element (Container, Position)
        or else Is_Leaf (Container, Position)
        =>
          First_Child'Result = No_Element,

        others =>
          --  A valid cursor is returned
          Has_Element (Container, First_Child'Result)

          --  The returned cursor is a child of the node at Position
          and then M_Path (Container, Position) =
                     M.Parent (M_Path (Container, First_Child'Result))

          --  The returned node is the first child of the node at Position
          and then (for all C in Container =>
                      (if Position = Parent (Container, C) then
                         C = First_Child'Result
                         or else Direction (Container, First_Child'Result) <
                                   Direction (Container, C))));

   function First_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Global => null,
     Pre => Has_Element (Container, Position)
            and then not Is_Leaf (Container, Position),
     Post => First_Child_Element'Result =
               Element (Container, First_Child (Container, Position));

   function Last_Child
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global         => null,
     Contract_Cases =>
       (not Has_Element (Container, Position)
        or else Is_Leaf (Container, Position)
        =>
          Last_Child'Result = No_Element,

        others =>
          --  A valid cursor is returned
          Has_Element (Container, Last_Child'Result)

          --  The returned cursor is a child of the node at Position
          and then M_Path (Container, Position) =
                     M.Parent (M_Path (Container, Last_Child'Result))

          --  The returned node is the last child of the node at Position
          and then (for all C in Container =>
                      (if Position = Parent (Container, C) then
                         C = Last_Child'Result
                         or else Direction (Container, Last_Child'Result) >
                                   Direction (Container, C))));

   function Last_Child_Element
     (Container : Tree;
      Position  : Cursor)
      return Element_Type
   with
     Global => null,
     Pre => Has_Element (Container, Position)
            and then not Is_Leaf (Container, Position),
     Post => Last_Child_Element'Result =
               Element (Container, Last_Child (Container, Position)),
     Annotate => (GNATprove, Inline_For_Proof);

   function Next_Sibling
     (Container : Tree;
      Position  : Cursor)
      return Cursor
   with
     Global         => null,
     Contract_Cases =>
       (not Has_Element (Container, Position)
        or else Is_Root (Container, Position)
        or else (for all W in Way_Type =>
                   (if W > Direction (Container, Position) then
                      not M.Contains
                            (Model (Container),
                             M.Sibling (M_Path (Container, Position), W))))
        =>
          Next_Sibling'Result = No_Element,

        others =>
          --  A valid cursor is returned
          Has_Element (Container, Next_Sibling'Result)

          --  The returned cursor is a sibling of the node at Position
          and then not M.Is_Root (M_Path (Container, Next_Sibling'Result))
          and then M.Parent (M_Path (Container, Position)) =
                     M.Parent (M_Path (Container, Next_Sibling'Result))

          --  The returned node is the next sibling. I.e., there are no
          --  sibling nodes between Position and Next_Sibling'Result
          and then
            (for all Way in Way_Type =>
               (if Way < Direction (Container, Next_Sibling'Result)
                   and then Way > Direction (Container, Position)
                then
                  Sibling (Container, Position, Way) = No_Element)));

   function Root_Element (Container : Tree) return Element_Type with
     Global   => null,
     Pre      => not Is_Empty (Container),
     Post     => Root_Element'Result = Element (Container, Root (Container)),
     Annotate => (GNATprove, Inline_For_Proof);
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
     Post   =>
       ((Parent'Result = No_Element) =
          (not Has_Element (Container, Position)
           or else Is_Root (Container, Position)))

       and then
         (if Parent'Result /= No_Element
          then Has_Element (Container, Parent'Result)
               and then M_Path (Container, Parent'Result) =
                          M.Parent (M_Path (Container, Position)));
   --  Get a cursor to the parent of a node

   function Child
     (Container : Tree;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   with
     Inline,
     Global => null,
     Post   =>
       ((Child'Result = No_Element) =
          (not Has_Element (Container, Position)
           or else
             not M.Contains (Model (Container),
                             M.Child (M_Path (Container, Position), Way))))

       and then
         (if Child'Result /= No_Element then
            Has_Element (Container, Child'Result)
            and then M_Path (Container, Child'Result) =
                       M.Child (M_Path (Container, Position), Way));
   --  Get a cursor to a child element of a node

   function Sibling
     (Container : Tree;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   with
     Global => null,
     Post   =>
       ((Sibling'Result = No_Element) =
          (not Has_Element (Container, Position)
           or else M.Is_Root (M_Path (Container, Position))
           or else
             not M.Contains (Model (Container),
                             M.Sibling (M_Path (Container, Position), Way))))
       and then
         (if Sibling'Result /= No_Element then
            Has_Element (Container, Sibling'Result)
            and then M_Path (Container, Sibling'Result) =
                       M.Sibling (M_Path (Container, Position), Way));

   function Direction
     (Container : Tree;
      Position  : Cursor)
      return Way_Type
   with
     Inline,
     Global => null,
     Pre    => Has_Element (Container, Position)
               and then not Is_Root (Container, Position),
     Post   =>
       Direction'Result = M.Way_From_Parent (M_Path (Container, Position));
   --  Get the direction (way) to the node at the given Position from its
   --  parent node.

   function Is_Ancestor
     (Container : Tree;
      Ancestor  : Cursor;
      Child     : Cursor)
      return Boolean
   with
     Global => null,
     Post   =>
       Is_Ancestor'Result =
         (Has_Element (Container, Ancestor)
          and then Has_Element (Container, Child)
          and then M.Is_Ancestor (M_Path (Container, Ancestor),
                                  M_Path (Container, Child))),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if Ancestor is an ancestor node of Child.

   function In_Subtree
     (Container    : Tree;
      Subtree_Root : Cursor;
      Position     : Cursor)
      return Boolean
   with
     Global => null,
     Post   => In_Subtree'Result =
                 (Has_Element (Container, Subtree_Root)
                  and then Has_Element (Container, Position)
                  and then M.In_Subtree (M_Path (Container, Position),
                                         M_Path (Container, Subtree_Root))),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if the node at Position is in the subtree rooted by Subtree_Root.

   function In_Branch
     (Container    : Tree;
      Ancestor     : Cursor;
      Position     : Cursor;
      Way          : Way_Type)
      return Boolean
   with
     Global => null,
     Post   =>
       In_Branch'Result =
         (Has_Element (Container, Ancestor)
          and then Has_Element (Container, Position)
          and then M.In_Subtree (M_Path (Container, Position),
                                 M.Child (M_Path (Container, Ancestor), Way))),
     Annotate => (GNATprove, Inline_For_Proof);
   --  Query if the node at Position is a descendant of the specified branch
   --  of the node at Ancestor.

   function Depth
     (Container : Tree;
      Position  : Cursor)
      return Count_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       Depth'Result =
         Count_Type_Conversions.From_Big_Integer
           (M.Length (M_Path (Container, Position)));
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
       and then M.Contains (Model (Container), M.Root)
       and then M.Element_Logic_Equal
                  (M.Get (Model (Container), M.Root),
                   M.Copy_Element (New_Item))
       and then M.Is_Leaf (Model (Container), M.Root);

   procedure Insert_Child
     (Container : in out Tree;
      New_Item  :        Element_Type;
      Position  :        Cursor;
      Way       :        Way_Type)
   with
     Global => null,
     Pre    =>
       Has_Element (Container, Position)
       and then Length (Container) < Count_Type'Last
       and then not M.Contains (Model (Container),
                                M.Child (M_Path (Container, Position), Way)),
     Post   =>
       --  One new node has been added to the tree
       Length (Container) = Length (Container'Old) + 1

       --  The element at the new node is equivalent to New_Item
       and then M.Element_Logic_Equal
                  (M.Get (Model (Container),
                          M.Child (M_Path (Container, Position), Way)),
                   M.Copy_Element (New_Item))
       and then
         Element (Container, Child (Container, Position, Way)) = New_Item

       --  All previous elements are unchanged in the formal model
       and then M.Elements_Equal (Model (Container'Old), Model (Container))

       --  Cursors map to the same elements
       and then Elements_Preserved (Container'Old, Container)

       and then
         M.Nodes_Included_Except
           (Left          => Model (Container),
            Right         => Model (Container'Old),
            Excluded_Node => M.Child (M_Path (Container'Old, Position), Way))

       --  The node at Position now has a child at the specified Way
       and then Has_Element (Container, Child (Container, Position, Way))

       --  The new child has no children of its own
       and then Is_Leaf (Container, Child (Container, Position, Way))

       --  The root of the tree is unchanged
       and then Root (Container) = Root (Container'Old)

       --  The mappings of all previous valid cursors to paths in the formal
       --  model is the same as before.
       and then Mapping_Preserved (Container'Old, Container);

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
       --  One new node has been added to the tree
       Length (Container) = Length (Container'Old) + 1

       --  The new node is inserted as the parent of the node at Position
       and then Has_Element (Container, Parent (Container, Position))
       and then M_Path (Container, Parent (Container, Position)) =
                M_Path (Container'Old, Position)
       and then Direction (Container, Position) = Way

       --  The element at the new node is equivalent to New_Item
       and then M.Element_Logic_Equal
                  (M.Copy_Element (New_Item),
                   M.Get (Model (Container), M_Path (Container'Old, Position)))
       and then Element (Container, Parent (Container, Position)) = New_Item

       --  All previous nodes and elements not in the affected subtree are
       --  unchanged.
       and then M.Elements_Equal_Except_Subtree
                  (Model (Container'Old),
                   Model (Container),
                   M_Path (Container'Old, Position))

       --  The position of elements in the affected subtree is shifted by
       --  the insertion of the new parent node.
       and then M.Subtree_Elements_Shifted
                  (Left         => Model (Container'Old),
                   Right        => Model (Container),
                   Subtree_Root => M_Path (Container'Old, Position),
                   Way          => Way)

       --  Cursors map to the same elements
       and then Elements_Preserved (Container'Old, Container)

       --  The root of the tree is unchanged, unless the node at Position was
       --  the root in which case the new parent node becomes the root.
       and then
         (if Root (Container'Old) = Position
          then Root (Container) = Parent (Container, Position)
          else Root (Container) = Root (Container'Old))

       --  All cursors are preserved, except for the insertion of a new cursor
       --  that references the new parent.
       and then P.Keys_Included (Paths (Container'Old), Paths (Container))
       and then P.Keys_Included_Except
                  (Paths (Container),
                   Paths (Container'Old),
                   Parent (Container, Position))

       --  The mapping of cursors to paths in the formal model is preserved
       --  for all nodes that are not in the affected subtree
       and then Mapping_Preserved_Except_Subtree
                  (Left     => Container'Old,
                   Right    => Container,
                   Position => Position)
       and then Mapping_Preserved_Except_Subtree
                  (Left     => Container,
                   Right    => Container'Old,
                   Position => Parent (Container, Position))

       --  Cursors in the affected subtree are remapped to new paths in the
       --  formal model as a result of the insertion of the new parent node.
       and then Subtree_Mapping_Shifted_Down
                  (Left         => Container'Old,
                   Right        => Container,
                   Subtree_Root => M_Path (Container'Old, Position),
                   Way          => Way)
       and then Subtree_Mapping_Shifted_Up
                  (Left         => Container,
                   Right        => Container'Old,
                   Subtree_Root => M_Path (Container, Position))

       --  Nodes that were ancestors of other nodes are still their ancestors
       --  in the updated tree.
       and then Ancestry_Preserved (Container'Old, Container);
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

   procedure Delete
     (Container : in out Tree;
      Position  :        Cursor)
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   =>
       --  The length of the container has decreased
       Length (Container) < Length (Container'Old)

       and then
         Model (Container) =
           M.Remove (Model (Container'Old), M_Path (Container'Old, Position))

       --  The referenced node is now deleted
       and then not Has_Element (Container, Position)

       --  If the root was deleted then the tree is now empty. Otherwise, the
       --  root is preserved.
       and then (if Position = Root (Container'Old)
                 then Is_Empty (Container)
                 else Root (Container) = Root (Container'Old))

       --  The mapping of cursors to paths in the formal model is the same,
       --  except for the deleted subtree.
       and then Mapping_Preserved (Container, Container'Old)
       and then Same_Mapping_Except_Subtree
                  (Left     => Container'Old,
                   Right    => Container,
                   Position => Position)

       --  Cursors map to the same elements
       and then Elements_Preserved (Container, Container'Old);
   --  Delete a node from the tree.
   --
   --  If the node has any child nodes then they are also deleted.

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
     Post   =>
       Element_Logic_Equal
         (Constant_Reference'Result.all,
          M.Get (Model (Container), M_Path (Container, Position)));

   function Reference
     (Container : not null access Tree;
      Position  : Cursor)
      return not null access Element_Type
   with
     Inline,
     Global => null,
     Pre    => Has_Element (Container.all, Position),
     Post   => --  The tree structure is preserved
               M.Same_Nodes (Model (Container.all), Model (Container.all'Old))

               --  The length is preserved
               and then Length (Container.all) = Length (Container.all'Old)

               --  The values of all other elements are preserved
               and then M.Elements_Equal_Except
                          (Model (Container.all),
                           Model (Container.all'Old),
                           M_Path (Container.all, Position))

               --  Mapping from cursors to Model nodes is preserved
               and then Mapping_Preserved
                          (Left  => Container.all,
                           Right => Container.all'Old)
               and then Mapping_Preserved
                          (Left  => Container.all'Old,
                           Right => Container.all)

               --  The value designated by the result of Reference is now
               --  equivalent to the element at the specified Position in the
               --  tree.
               and then
                 Element_Logic_Equal
                   (At_End (Reference'Result).all,
                    Element (At_End (Container).all, Position));

private
   pragma SPARK_Mode (Off);

   package Element_Holder_Types is new Holders (Element_Type);
   package EHT renames Element_Holder_Types;

   subtype Index_Type is Count_Type range 1 .. Count_Type'Last;

   --  The tree nodes are stored in a vector. Each vector contains references
   --  (cursors) to its parent and child nodes which determine the tree
   --  structure.
   --
   --  Cursors are simply indices into the underlying node vector.
   --
   --  When deleting nodes, the node might be in the middle of the vector
   --  which prevents us from simply removing the node from the vector, as this
   --  would shift other nodes in the vector, possibly invalidating cursors.
   --
   --  Instead, when deleting nodes, they are moved to a linked list of free
   --  nodes. When inserting new nodes into the tree, the free list is
   --  checked first and a node is taken from there if one is available.
   --  If the free list is empty, then new nodes are allocated by appending to
   --  the node vector.
   --
   --  This also means that we can't simply use the node vector's length as
   --  the tree length, since some nodes in the vector might be deleted
   --  (i.e., in the free list). Instead, a Length counter is maintained that
   --  is incremented for each node inserted into the tree, and decremented
   --  for each node deleted from the tree.

   type Node_Type is record
      Element  : EHT.Holder_Type;
      --  The element stored in this node.
      --
      --  This is null if and only if Free is True.

      Parent   : Cursor;
      --  Reference to the parent node in the tree
      --
      --  This is No_Element if the node is the root of the tree

      Position : Way_Type;
      --  The position of the node relative to its parent, i.e. which way
      --  (edge) to take from the parent node to this node.

      Ways     : Way_Cursor_Array;
      --  Holds references to child nodes for each way from this node

      Free     : Boolean;
      --  True if this node has been deleted and is in the free list.
      --  False if this node is in use.
   end record;

   package Node_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Index_Type,
      Element_Type => Node_Type,
      "="          => "=");

   type Tree is record
      Nodes     : aliased Node_Vectors.Vector := Node_Vectors.Empty_Vector;
      Root      : Cursor                      := No_Element;
      Free_List : Cursor                      := No_Element;
      Length    : Count_Type                  := 0;
   end record;

end SPARK.Containers.Formal.Unbounded_Multiway_Trees;