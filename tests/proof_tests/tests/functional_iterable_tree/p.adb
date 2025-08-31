package body P with
  SPARK_Mode => On
is

   function Contains_Value (T : Tree; V : Integer) return Boolean is
      Found : Boolean := False;

   begin
      for C in Iterate (T) loop
         pragma Loop_Variant (Decreases => Length (C));
         pragma Loop_Invariant
           (for all P of T =>
              (if not Contains (C, P) then
                 Get (T, P) /= V));

         if Get (T, Choose (C)) = V then
            Found := True;
            exit;
         end if;
      end loop;

      return Found;
   end Contains_Value;

end P;