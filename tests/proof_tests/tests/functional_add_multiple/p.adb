package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
      L : constant Path_Type := Child (Root, Left);
      R : constant Path_Type := Child (Root, Right);

   begin

      T := Empty_Tree;
      T := Add (T, 1, Root);
      T := Add (T, 2, L);
      T := Add (T, 3, R);

      pragma Assert (Contains (T, Root));
      pragma Assert (Contains (T, L));
      pragma Assert (Contains (T, R));

      pragma Assert (Get (T, Root) = 1);
      pragma Assert (Get (T, L)    = 2);
      pragma Assert (Get (T, R)    = 3);

      pragma Assert (for all N of T => Get (T, N) in 1 .. 3);
   end Create;

end P;