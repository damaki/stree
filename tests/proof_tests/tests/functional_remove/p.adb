package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
      L  : constant Path_Type := Child (Root, Left);
      R  : constant Path_Type := Child (Root, Right);
      RR : constant Path_Type := Child (R, Right);
   begin
      T := Empty_Tree;
      T := Add (T, 1, Root);
      T := Add (T, 2, L);
      T := Add (T, 4, R);
      T := Add (T, 5, RR);

      pragma Assert (Get (T, Root) = 1);
      pragma Assert (Get (T, L)    = 2);
      pragma Assert (Get (T, R)    = 4);
      pragma Assert (Get (T, RR)   = 5);

      T := Remove (T, R);

      pragma Assert (Contains (T, Root));
      pragma Assert (Contains (T, L));
      pragma Assert (not Contains (T, R));
      pragma Assert (not Contains (T, RR));
      pragma Assert (Get (T, Root) = 1);
      pragma Assert (Get (T, L)    = 2);
   end Create;

end P;