with SPARK.Containers.Functional.Multiway_Trees;

package P with
  SPARK_Mode => On
is

   type Way_Type is (Left, Middle, Right);

   package P_Trees is new SPARK.Containers.Functional.Multiway_Trees
     (Element_Type => Integer,
      Way_Type     => Way_Type,
      "="          => "=");
   use P_Trees;

   function Contains_Value (T : Tree; V : Integer) return Boolean with
     Post => Contains_Value'Result = (for some P of T => Get (T, P) = V);

end P;