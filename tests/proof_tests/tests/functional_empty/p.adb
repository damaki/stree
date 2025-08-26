package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
   begin
      T := Empty_Tree;

      pragma Assert (not Contains (T, Root));
   end Create;

end P;