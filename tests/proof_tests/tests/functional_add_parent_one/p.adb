package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
      use type Path_Type;

      M : constant Path_Type := [Middle];

   begin
      T := Empty_Tree;
      T := Add (T, 1, Root_Node);
      T := Add_Parent (T, 2, Root_Node, Middle);

      pragma Assert (Contains (T, Root_Node));
      pragma Assert (Contains (T, M));
      pragma Assert (Get (T, Root_Node) = 2);
      pragma Assert (Get (T, M) = 1);
   end Create;

end P;