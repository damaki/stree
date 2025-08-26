package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
   begin
      T := Empty_Tree;
      T := Add (T, 1, Root);

      pragma Assert (Contains (T, Root));
      pragma Assert (Get (T, Root) = 1);
      pragma Assert (not Contains (T, Child (Root, Left)));
      pragma Assert (not Contains (T, Child (Root, Middle)));
      pragma Assert (not Contains (T, Child (Root, Right)));
      pragma Assert (for all N of T => N = Root);
   end Create;

end P;