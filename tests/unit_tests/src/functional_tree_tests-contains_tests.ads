--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;   use AUnit.Test_Suites;
with AUnit.Test_Caller;

--  This package tests the Contains function.

package Functional_Tree_Tests.Contains_Tests is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Contains_Empty (T : in out Test_Fixture);
   procedure Test_Contains_Valid (T : in out Test_Fixture);
   procedure Test_Contains_Not_Valid (T : in out Test_Fixture);

   procedure Add_To_Suite (S : in out Test_Suite'Class);

private

   package Caller is new AUnit.Test_Caller (Test_Fixture);

end Functional_Tree_Tests.Contains_Tests;