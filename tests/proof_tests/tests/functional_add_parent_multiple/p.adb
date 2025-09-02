package body P with
  SPARK_Mode => On
is

   procedure Create (T : out Tree) is
      M   : constant Path_Type := Child (Root, Middle);
      MM  : constant Path_Type := Child (M, Middle);

   begin
      --  Create a tree with one root node
      T := Add (Empty_Tree, 1, Root);

      pragma Assert (Contains (T, Root));
      pragma Assert (Get (T, Root) = 1);

      --  Change tree structure to: 2 -> 1
      T := Add_Parent (T, 2, Root, Middle);
      pragma Assert (Contains (T, Root));
      pragma Assert (Contains (T, M));
      pragma Assert (Get (T, Root) = 2);
      pragma Assert (Get (T, M) = 1);

      --  Change tree structure to: 2 -> 3 -> 1
      T := Add_Parent (T, 3, M, Middle);
      pragma Assert (Contains (T, Root));
      pragma Assert (Contains (T, M));
      pragma Assert (MM = Insert (M, Length (M), Middle));
      pragma Assert (Contains (T, MM));
      pragma Assert (Get (T, Root) = 2);
      pragma Assert (Get (T, M) = 3);
      pragma Assert (Get (T, MM) = 1);
   end Create;

end P;