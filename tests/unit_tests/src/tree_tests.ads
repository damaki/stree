--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Stree.Unbounded_Multiway_Trees;

package Tree_Tests is

   --  Ghost code is not executable since some parts of the SPARK library
   --  containers are not executable (they are for analysis only).

   pragma Assertion_Policy (Ghost => Ignore);

   --  The tests are performed on a tree instantiation with Integer elements,
   --  with order 3 (i.e. a 3-way tree) with ways: left, middle, and right.

   type Direction is (Left, Middle, Right);

   package Integer_3Way_Trees is new Stree.Unbounded_Multiway_Trees
     (Element_Type => Integer,
      Way_Type     => Direction);

end Tree_Tests;