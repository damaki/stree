--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;   use AUnit.Test_Suites;
with AUnit.Test_Caller;

--  This package tests the Length function.

package Functional_Tree_Tests.Length_Tests is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Length_0 (T : in out Test_Fixture);
   procedure Test_Length_1 (T : in out Test_Fixture);
   procedure Test_Length_2 (T : in out Test_Fixture);
   procedure Test_Length_3 (T : in out Test_Fixture);
   procedure Test_Length_4 (T : in out Test_Fixture);

   procedure Add_To_Suite (S : in out Test_Suite'Class);

private

   package Caller is new AUnit.Test_Caller (Test_Fixture);

end Functional_Tree_Tests.Length_Tests;