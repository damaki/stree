--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Test_Fixtures;
with AUnit.Test_Suites;   use AUnit.Test_Suites;
with AUnit.Test_Caller;

--  This package tests the Splice_Subtree function.

package Tree_Tests.Splice_Subtree_Tests is

   type Test_Fixture is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Splice_Subtree_Single_Node (T : in out Test_Fixture);
   procedure Test_Splice_Subtree_Multiple_Nodes (T : in out Test_Fixture);
   procedure Test_Splice_Subtree_Bad_Subtree_Root (T : in out Test_Fixture);
   procedure Test_Splice_Subtree_Bad_New_Parent (T : in out Test_Fixture);
   procedure Test_Splice_Subtree_Cyclic_Tree (T : in out Test_Fixture);

   procedure Add_To_Suite (S : in out Test_Suite'Class);

private

   package Caller is new AUnit.Test_Caller (Test_Fixture);

end Tree_Tests.Splice_Subtree_Tests;