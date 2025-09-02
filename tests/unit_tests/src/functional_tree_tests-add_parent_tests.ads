--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;   use AUnit.Test_Suites;
with AUnit.Test_Caller;

--  This package tests the Delete function.

package Functional_Tree_Tests.Add_Parent_Tests is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Add_Parent_Root (T : in out Test_Fixture);
   procedure Test_Add_Parent_Non_Root (T : in out Test_Fixture);

   procedure Add_To_Suite (S : in out Test_Suite'Class);

private

   package Caller is new AUnit.Test_Caller (Test_Fixture);

end Functional_Tree_Tests.Add_Parent_Tests;