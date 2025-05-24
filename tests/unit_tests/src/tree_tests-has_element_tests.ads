--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;   use AUnit.Test_Suites;
with AUnit.Test_Caller;

--  This package tests the Has_Element function.

package Tree_Tests.Has_Element_Tests is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Has_Element_No_Element (T : in out Test_Fixture);
   procedure Test_Has_Element_Empty (T : in out Test_Fixture);
   procedure Test_Has_Element_Invalid_Cursor (T : in out Test_Fixture);
   procedure Test_Has_Element_Valid_Cursor (T : in out Test_Fixture);

   procedure Add_To_Suite (S : in out Test_Suite'Class);

private

   package Caller is new AUnit.Test_Caller (Test_Fixture);

end Tree_Tests.Has_Element_Tests;