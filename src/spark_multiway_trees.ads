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

   type Index_Type is new Ada.Containers.Count_Type
     range 1 .. Ada.Containers.Count_Type'Last - 1;

   subtype Extended_Index_Type is Index_Type'Base
     range Index_Type'First - 1 .. Index_Type'Last;

   type Cursor is record
      Node : Extended_Index_Type := Extended_Index_Type'First;
   end record;

   No_Element : constant Cursor;

   ------------------
   -- Formal Model --
   ------------------

   package Formal_Model with Ghost is

      function Element_Logic_Equal (Left, Right : Element_Type) return Boolean
      with
        Global => null,
        Annotate => (GNATprove, Logical_Equal);

      -----------
      -- Paths --
      -----------

      Max_Size : constant := (Index_Type'Last - Index_Type'First) + 1;

      type Positive_Count_Type is range 1 .. Max_Size + 1;
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

      ---------------
      -- Accessors --
      ---------------

      function Has_Element (F : Forest; C : Cursor) return Boolean with
        Global => null,
        Post => (if C = No_Element then not Has_Element'Result);

      function Has_Element (F : Forest; I : Index_Type) return Boolean is
        (Has_Element (F, Cursor'(Node => I)))
      with
        Global => null;

      function Element (F : Forest; C : Cursor) return Element_Type with
        Global => null,
        Pre => Has_Element (F, C);

      function Element (F : Forest; I : Index_Type) return Element_Type is
        (Element (F, Cursor'(Node => I)))
      with
        Global => null,
        Pre => Has_Element (F, I);

      function Parent (F : Forest; C : Cursor) return Cursor with
        Global => null,
        Pre    => Has_Element (F, C),
        Post   => (if Parent'Result /= No_Element
                   then Has_Element (F, Parent'Result));

      function Parent (F : Forest; I : Index_Type) return Cursor is
        (Parent (F, Cursor'(Node => I)))
      with
        Global => null,
        Pre    => Has_Element (F, I);

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

      function Position (F : Forest; I : Index_Type) return Way_Type with
        Ghost,
        Global => null,
        Pre    => Has_Element (F, I)
                  and then Parent (F, I) /= No_Element;

      -----------
      -- Model --
      -----------

      type Model_Type is array (Index_Type) of Path_Type with
        Predicate =>
          (for all P of Model_Type =>
             Way_Seqs.Length (P.Path) < Big_Integer'(Max_Size));

      function Model (F : Forest; Root : Cursor) return Model_Type with
        Global => null,
        Pre    => Root /= No_Element
                  and then Has_Element (F, Root.Node)
                  and then Is_Root (F, Root),
        Post   =>
          --  The root is part of the tree
          Model'Result (Root.Node).In_Tree

          --  The path from the root to itself is empty
          and then Last (Model'Result (Root.Node).Path) = 0

          --  Non-root nodes are in the tree iff their parent is in the tree
          and then
            (for all I in Index_Type =>
               (if I /= Root.Node and then Has_Element (F, I) then
                  (if Parent (F, I) /= No_Element
                      and then Model'Result (Parent (F, I).Node).In_Tree
                   then Model'Result (I).In_Tree
                   else not Model'Result (I).In_Tree)))

          --  The path from the root to non-root tree nodes is equal to the
          --  path to their parent extended by the last direction to get to the
          --  node. For other nodes, the path is empty.
          and then
            (for all I in Index_Type =>
               (if Model'Result (I).In_Tree and then I /= Root.Node
                then Is_Add (Model'Result (Parent (F, I).Node).Path,
                             Position (F, I),
                             Model'Result (I).Path)
                else Last (Model'Result (I).Path) = 0))

          --  Nodes in the tree all have different associated paths
          and then
            (for all I in Index_Type =>
               (if Model'Result (I).In_Tree then
                  (for all J in Index_Type =>
                     (if Model'Result (J).In_Tree
                         and then Model'Result (J).Path = Model'Result (I).Path
                      then J = I))))

          --  All nodes in the tree map to a valid element
          and then
            (for all I in Index_Type =>
               (if Model'Result (I).In_Tree then Has_Element (F, I)));

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

   Empty : constant := Extended_Index_Type'First;

   type Way_Array is array (Way_Type) of Extended_Index_Type;

   subtype Position_Type is Way_Type'Base
     range Way_Type'First - 1 .. Way_Type'Last;

   Top : constant Position_Type := Position_Type'First;

   No_Element : constant Cursor := Cursor'(Node => Empty);

   type Node_Type is record
      Element  : aliased Element_Type;
      Parent   : Extended_Index_Type;
      Position : Position_Type;
      Ways     : Way_Array;
   end record;

   package Node_Maps is new SPARK.Containers.Formal.Unbounded_Ordered_Maps
     (Key_Type     => Index_Type,
      Element_Type => Node_Type,
      "<"          => "<",
      "="          => "=");
   use Node_Maps;

   type Forest is record
      Nodes : aliased Node_Maps.Map;
   end record with
     Type_Invariant => Tree_Structure (Nodes);

   function Tree_Structure (F : Node_Maps.Map) return Boolean with
     Ghost,
     Global => null;

end SPARK_Multiway_Trees;
