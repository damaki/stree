with Ada.Containers;

package body P with
  SPARK_Mode => On
is
   use type Ada.Containers.Count_Type;

   procedure Create (T : out Tree) is
      Node_1 : Cursor;
      Node_2 : Cursor;
      Node_3 : Cursor;
   begin
      T := Empty_Tree;
      pragma Assert (Length (T) = 0);

      Insert_Root (T, 1);
      Node_1 := Root (T);
      pragma Assert (Length (T) = 1);
      pragma Assert (Root_Element (T) = 1);
      pragma Assert (Child (T, Node_1, Left) = No_Element);
      pragma Assert (Child (T, Node_1, Middle) = No_Element);
      pragma Assert (Child (T, Node_1, Right) = No_Element);

      --  Change tree structure to: 2 -> 1
      Insert_Parent (T, 2, Node_1, Left, Node_2);
      pragma Assert (Length (T) = 2);
      pragma Assert (Root_Element (T) = 2);
      pragma Assert (Element (T, Node_1) = 1);
      pragma Assert (Element (T, Node_2) = 2);
      pragma Assert (Root (T) = Node_2);
      pragma Assert (Parent (T, Node_1) = Node_2);
      pragma Assert (Is_Ancestor (T, Node_2, Node_1));

      --  Change tree structure to: 2 -> 3 -> 1
      Insert_Parent (T, 3, Node_1, Middle, Node_3);
      pragma Assert (Length (T) = 3);
      pragma Assert (Element (T, Node_1) = 1);
      pragma Assert (Element (T, Node_2) = 2);
      pragma Assert (Element (T, Node_3) = 3);
      pragma Assert (Root (T) = Node_2);
      pragma Assert (Parent (T, Node_1) = Node_3);
      pragma Assert (Parent (T, Node_3) = Node_2);
      pragma Assert (Is_Ancestor (T, Node_2, Node_3));
      pragma Assert (Is_Ancestor (T, Node_2, Node_1));
   end Create;

end P;