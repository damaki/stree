with Stree.Unbounded_Multiway_Trees;

package P with
  SPARK_Mode => On
is

   type Way_Type is (Left, Middle, Right);

   package P_Trees is new Stree.Unbounded_Multiway_Trees
     (Element_Type => Integer,
      Way_Type     => Way_Type);
   use P_Trees;

   procedure Create (T : out Tree);

end P;