--
--  Copyright 2023 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Tree_Tests.Delete_Tests;
with Tree_Tests.Element_Tests;
with Tree_Tests.Empty_Tree_Tests;
with Tree_Tests.First_Tests;
with Tree_Tests.Has_Element_Tests;
with Tree_Tests.Is_Empty_Tests;
with Tree_Tests.Is_Root_Tests;
with Tree_Tests.Iterable_Tests;
with Tree_Tests.Last_Tests;
with Tree_Tests.Length_Tests;
with Tree_Tests.Next_Tests;
with Tree_Tests.Parent_Tests;
with Tree_Tests.Root_Element_Tests;
with Tree_Tests.Root_Tests;

with Functional_Tree_Tests.Add_Tests;
with Functional_Tree_Tests.Add_Parent_Tests;
with Functional_Tree_Tests.Contains_Tests;
with Functional_Tree_Tests.Is_Empty_Tests;
with Functional_Tree_Tests.Length_Tests;

package body Test_Suites
is

   function Suite return Access_Test_Suite
   is
      S : constant Access_Test_Suite := new Test_Suite;
   begin
      Tree_Tests.Delete_Tests.Add_To_Suite (S.all);
      Tree_Tests.Element_Tests.Add_To_Suite (S.all);
      Tree_Tests.Empty_Tree_Tests.Add_To_Suite (S.all);
      Tree_Tests.First_Tests.Add_To_Suite (S.all);
      Tree_Tests.Has_Element_Tests.Add_To_Suite (S.all);
      Tree_Tests.Is_Empty_Tests.Add_To_Suite (S.all);
      Tree_Tests.Is_Root_Tests.Add_To_Suite (S.all);
      Tree_Tests.Iterable_Tests.Add_To_Suite (S.all);
      Tree_Tests.Last_Tests.Add_To_Suite (S.all);
      Tree_Tests.Length_Tests.Add_To_Suite (S.all);
      Tree_Tests.Next_Tests.Add_To_Suite (S.all);
      Tree_Tests.Parent_Tests.Add_To_Suite (S.all);
      Tree_Tests.Root_Element_Tests.Add_To_Suite (S.all);
      Tree_Tests.Root_Tests.Add_To_Suite (S.all);

      Functional_Tree_Tests.Add_Tests.Add_To_Suite (S.all);
      Functional_Tree_Tests.Add_Parent_Tests.Add_To_Suite (S.all);
      Functional_Tree_Tests.Contains_Tests.Add_To_Suite (S.all);
      Functional_Tree_Tests.Is_Empty_Tests.Add_To_Suite (S.all);
      Functional_Tree_Tests.Length_Tests.Add_To_Suite (S.all);

      return S;
   end Suite;

end Test_Suites;