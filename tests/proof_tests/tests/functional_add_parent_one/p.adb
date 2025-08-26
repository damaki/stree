package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
      M : constant Path_Type := Child (Root, Middle);

   begin
      T := Empty_Tree;
      T := Add (T, 1, Root);
      T := Add_Parent (T, 2, Root, Middle);

      pragma Assert (Contains (T, Root));
      pragma Assert (Contains (T, M));
      pragma Assert (Get (T, Root) = 2);
      pragma Assert (Get (T, M) = 1);
   end Create;

end P;