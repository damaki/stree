--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Containers;

with SPARK.Big_Integers; use SPARK.Big_Integers;
with SPARK.Containers.Functional.Vectors;

private with SPARK.Containers.Formal.Unbounded_Ordered_Maps;

package SPARK_Multiway_Trees with
  SPARK_Mode => On
is

   use type Ada.Containers.Count_Type;

   type Element_Type is new Integer;
   Order : constant Positive := 2;

   type Way_Type is new Positive range 1 .. Order;

   type Forest is private;

   type Cursor is new Ada.Containers.Count_Type
     range 0 .. Ada.Containers.Count_Type'Last - 1;

   subtype Valid_Cursor_Range is Cursor range 1 .. Cursor'Last;

   No_Element : constant Cursor;

   ------------------
   -- Formal Model --
   ------------------

   package Formal_Model with Ghost is

      function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
      with
        Global => null,
        Annotate => (GNATprove, Logical_Equal);

      ---------------
      -- Accessors --
      ---------------

      function Has_Element (F : Forest; C : Cursor) return Boolean with
        Global => null,
        Post => (if C = No_Element then not Has_Element'Result);

      function Element (F : Forest; C : Cursor) return Element_Type with
        Global => null,
        Pre => Has_Element (F, C);

      function Parent (F : Forest; C : Cursor) return Cursor with
        Global => null,
        Pre    => Has_Element (F, C),
        Post   => (if Parent'Result /= No_Element
                   then Has_Element (F, Parent'Result));

      function Child
        (F : Forest;
         C : Cursor;
         W : Way_Type)
         return Cursor
      with
        Global => null,
        Pre    => Has_Element (F, C),
        Post   => (if Child'Result /= No_Element
                   then Has_Element (F, Child'Result));

      function Is_Root (F : Forest; C : Cursor) return Boolean with
        Global => null,
        Pre    => Has_Element (F, C),
        Post   => (if Is_Root'Result then Parent (F, C) = No_Element);

      function Position (F : Forest; C : Cursor) return Way_Type with
        Ghost,
        Global => null,
        Pre    => Has_Element (F, C)
                  and then Parent (F, C) /= No_Element;

   end Formal_Model;

   function Has_Element
     (Container : Forest;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Post =>
       Has_Element'Result = Formal_Model.Has_Element (Container, Position);

   function Element
     (Container : Forest;
      Position  : Cursor)
      return Element_Type
   with
     Global => null,
     Pre => Has_Element (Container, Position),
     Post => Element'Result = Formal_Model.Element (Container, Position);

   function Is_Root
     (Container : Forest;
      Position  : Cursor)
      return Boolean
   with
     Global => null,
     Pre  => Has_Element (Container, Position),
     Post => Is_Root'Result = Formal_Model.Is_Root (Container, Position);

   function Parent
     (Container : Forest;
      Position  : Cursor)
      return Cursor
   with
     Global => null,
     Pre => Has_Element (Container, Position),
     Post => Parent'Result = Formal_Model.Parent (Container, Position);

   function Child
     (Container : Forest;
      Position  : Cursor;
      Way       : Way_Type)
      return Cursor
   with
     Global => null,
     Pre => Has_Element (Container, Position),
     Post => Child'Result = Formal_Model.Child (Container, Position, Way);

   function Constant_Reference
     (Container : Forest;
      Position  : Cursor) return not null access constant Element_Type
   with
     Global => null,
     Pre    => Has_Element (Container, Position),
     Post   => Formal_Model.Element_Logic_Equal
                 (Constant_Reference'Result.all,
                  Formal_Model.Element (Container, Position));

private

   type Way_Array is array (Way_Type) of Cursor;

   subtype Position_Type is Way_Type'Base
     range Way_Type'First - 1 .. Way_Type'Last;

   Top : constant Position_Type := Position_Type'First;

   No_Element : constant Cursor := Cursor'First;

   type Node_Type is record
      Element  : aliased Element_Type;
      Parent   : Cursor;
      Position : Position_Type;
      Ways     : Way_Array;
   end record;

   package Node_Maps is new SPARK.Containers.Formal.Unbounded_Ordered_Maps
     (Key_Type     => Valid_Cursor_Range,
      Element_Type => Node_Type,
      "<"          => "<",
      "="          => "=");
   use Node_Maps;

   type Forest is record
      Nodes : aliased Node_Maps.Map;
      Root  : Cursor := No_Element;
   end record with
     Type_Invariant =>
       Forest_Model.Tree_Structure (Nodes)
       and then Forest_Model.Forest_Root (Nodes, Root);

   package Forest_Model with Ghost is

      function Tree_Structure (F : Node_Maps.Map) return Boolean with
        Global => null;

      function Forest_Root
        (Nodes : Node_Maps.Map;
         Root  : Cursor)
         return Boolean
      is
        (if Is_Empty (Nodes)
         then Root = No_Element
         else Root /= No_Element
              and then Contains (Nodes, Root)
              and then Element (Nodes, Root).Position = Top)
      with
        Global => null;

      -----------
      -- Paths --
      -----------

      Max_Size : constant :=
        (Valid_Cursor_Range'Last - Valid_Cursor_Range'First) + 1;

      subtype Positive_Count_Type is Ada.Containers.Count_Type
        range 1 .. Max_Size;

      package Way_Seqs is new SPARK.Containers.Functional.Vectors
        (Positive_Count_Type, Way_Type);
      use Way_Seqs;

      type Path_Type is record
         Path    : Way_Seqs.Sequence;
         In_Tree : Boolean := False;
      end record;

      function Is_Concat (Q, V, P : Sequence) return Boolean is
        (Length (P) - Length (V) = Length (Q)
         and then (for all I in 1 .. Last (Q) => Get (P, I) = Get (Q, I))
         and then (for all I in 1 .. Last (V) =>
                    Get (P, I + Last (Q)) = Get (V, I))
         and then (for all I in Last (Q) + 1 .. Last (P) =>
                    Get (V, I - Last (Q)) = Get (P, I)))
      with Pre => Length (Q) <= Big_Integer'(Max_Size);
      --  Returns True if P is the concatenation of Q & V.

      function Is_Add
        (S1 : Sequence;
         W  : Way_Type;
         S2 : Sequence)
         return Boolean
      is
        (Length (S2) - 1 = Length (S1) and then S1 < S2
         and then Get (S2, Last (S2)) = W);
      --  Returns True if S2 is equal to S1 with W appended to it

      -----------
      -- Model --
      -----------

      type Model_Type is array (Valid_Cursor_Range) of Path_Type with
        Predicate =>
          (for all P of Model_Type =>
             Way_Seqs.Length (P.Path) < Big_Integer'(Max_Size));

      function Model (F : Forest; Root : Cursor) return Model_Type with
        Global => null,
        Pre    => Tree_Structure (F.Nodes)
                  and then Forest_Root (F.Nodes, F.Root)
                  and then Root /= No_Element
                  and then Has_Element (F, Root)
                  and then Is_Root (F, Root),
        Post   =>
          --  The root is part of the tree
          Model'Result (Root).In_Tree

          --  The path from the root to itself is empty
          and then Last (Model'Result (Root).Path) = 0

          --  Non-root nodes are in the tree iff their parent is in the tree
          and then
            (for all I in Valid_Cursor_Range =>
               (if I /= Root and then Has_Element (F, I) then
                  (if Parent (F, I) /= No_Element
                      and then Model'Result (Parent (F, I)).In_Tree
                   then Model'Result (I).In_Tree
                   else not Model'Result (I).In_Tree)))

          --  If a node is in the tree, then its children are also in the tree
          and then
            (for all I in Valid_Cursor_Range =>
               (if Model'Result (I).In_Tree then
                  (for all W in Way_Type =>
                     (if Child (F, I, W) /= No_Element then
                        Model'Result (Child (F, I, W))
                          .In_Tree))))

          --  The path from the root to non-root tree nodes is equal to the
          --  path to their parent extended by the last direction to get to the
          --  node. For other nodes, the path is empty.
          and then
            (for all I in Valid_Cursor_Range =>
               (if Model'Result (I).In_Tree and then I /= Root
                then Is_Add (Model'Result (Parent (F, I)).Path,
                             Element (F.Nodes, I).Position,
                             Model'Result (I).Path)
                else Last (Model'Result (I).Path) = 0))

          --  Nodes in the tree all have different associated paths
          and then
            (for all I in Valid_Cursor_Range =>
               (if Model'Result (I).In_Tree then
                  (for all J in Valid_Cursor_Range =>
                     (if Model'Result (J).In_Tree
                         and then Model'Result (J).Path = Model'Result (I).Path
                      then J = I))))

          --  All nodes in the tree map to a valid element
          and then
            (for all I in Valid_Cursor_Range =>
               (if Model'Result (I).In_Tree then Has_Element (F, I)));

      function In_Tree
        (F : Forest; R : Cursor; C : Cursor)
         return Boolean
      with
        Global => null,
        Pre    => Tree_Structure (F.Nodes)
                  and then Forest_Root (F.Nodes, F.Root)
                  and then H1as_Element (F, R)
                  and then Has_Element (F, C)
                  and then Is_Root (F, R);

      function Depth
        (F : Forest; R : Cursor; C : Cursor)
         return Ada.Containers.Count_Type
      with
        Global => null,
        Pre    => Tree_Structure (F.Nodes)
                  and then Forest_Root (F.Nodes, F.Root)
                  and then Has_Element (F, R)
                  and then Has_Element (F, C)
                  and then Is_Root (F, R)
                  and then In_Tree (F, R, C);

   end Forest_Model;

end SPARK_Multiway_Trees;
