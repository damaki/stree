--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;   use AUnit.Test_Suites;
with AUnit.Test_Caller;

--  This package tests the Last function.

package Tree_Tests.Last_Tests is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Last_Empty (T : in out Test_Fixture);
   procedure Test_Last_Not_Empty (T : in out Test_Fixture);

   procedure Add_To_Suite (S : in out Test_Suite'Class);

private

   package Caller is new AUnit.Test_Caller (Test_Fixture);

end Tree_Tests.Last_Tests;