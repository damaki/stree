package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
      use type Path_Type;

   begin
      T := Empty_Tree;
      T := Add (T, 1, Root_Node);

      pragma Assert (Contains (T, Root_Node));
      pragma Assert (Get (T, Root_Node) = 1);
      pragma Assert (not Contains (T, [Left]));
      pragma Assert (not Contains (T, [Middle]));
      pragma Assert (not Contains (T, [Right]));
      pragma Assert (for all N of T => N = Root_Node);
   end Create;

end P;