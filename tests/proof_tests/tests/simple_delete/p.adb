with Ada.Containers;

package body P with
  SPARK_Mode => On
is
   use type Ada.Containers.Count_Type;

   procedure Create (T : out Tree) is
      N4 : Cursor;
      N5 : Cursor;
   begin
      T := Empty_Tree;
      pragma Assert (Length (T) = 0);

      Insert_Root (T, 1);
      Insert_Child (T, 2, Root (T), Left);
      Insert_Child (T, 3, Root (T), Middle);
      Insert_Child (T, 4, Root (T), Right);

      N4 := Child (T, Root (T), Right);
      Insert_Child (T, 5, N4, Right);
      N5 := Child (T, N4, Right);

      pragma Assert (N4 = Child (T, Root (T), Right));

      Delete (T, N4);

      pragma Assert (not Has_Element (T, N4));
      pragma Assert (not Has_Element (T, N5));
      pragma Assert (Child (T, Root (T), Right) = No_Element);
      pragma Assert (Element (T, Root (T)) = 1);
      pragma Assert (Element (T, Child (T, Root (T), Left)) = 2);
      pragma Assert (Element (T, Child (T, Root (T), Middle)) = 3);
   end Create;

end P;