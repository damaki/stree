with Ada.Containers;

package body P with
  SPARK_Mode => On
is
   use type Ada.Containers.Count_Type;

   procedure Create (T : out Tree) is
   begin
      T := Empty_Tree;
      pragma Assert (Length (T) = 0);

      Insert_Root (T, 1);
      pragma Assert (Length (T) = 1);
      pragma Assert (Root_Element (T) = 1);
      pragma Assert (Child (T, Root (T), Left) = No_Element);
      pragma Assert (Child (T, Root (T), Middle) = No_Element);
      pragma Assert (Child (T, Root (T), Right) = No_Element);

      Insert_Child (T, 2, Root (T), Left);
      pragma Assert (Length (T) = 2);
      pragma Assert (Root_Element (T) = 1);
      pragma Assert (Element (T, Child (T, Root (T), Left)) = 2);

      Insert_Child (T, 3, Root (T), Middle);
      pragma Assert (Length (T) = 3);
      pragma Assert (Root_Element (T) = 1);
      pragma Assert (Element (T, Child (T, Root (T), Left)) = 2);
      pragma Assert (Element (T, Child (T, Root (T), Middle)) = 3);

      Insert_Child (T, 4, Root (T), Right);
      pragma Assert (Length (T) = 4);
      pragma Assert (Root_Element (T) = 1);
      pragma Assert (Element (T, Child (T, Root (T), Left)) = 2);
      pragma Assert (Element (T, Child (T, Root (T), Middle)) = 3);
      pragma Assert (Element (T, Child (T, Root (T), Middle)) = 4);
   end Create;

end P;