--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
private with SPARK.Containers.Formal.Unbounded_Ordered_Maps;

package SPARK_Multiway_Trees with
  SPARK_Mode => On
is

   type Element_Type is new Integer;
   type Index_Type is new Positive;
   Order : constant Positive := 5;

   type Way_Type is new Positive range 1 .. Order;

   type Forest is private;

private

   subtype Extended_Index_Type is Index_Type'Base
     range Index_Type'First - 1 .. Index_Type'Last;

   Empty : constant := Index_Type'First - 1;

   type Way_Array is array (Way_Type) of Extended_Index_Type;

   subtype Position_Type is Way_Type'Base
     range Way_Type'First - 1 .. Way_Type'Last;

   Top : constant Position_Type := Position_Type'First;

   type Node_Type is record
      Element  : Element_Type;
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
      Nodes : Node_Maps.Map;
   end record;

   function Tree_Structure (F : Node_Maps.Map) return Boolean with
     Ghost,
     Global => null;

end SPARK_Multiway_Trees;
