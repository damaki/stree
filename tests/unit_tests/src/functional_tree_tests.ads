--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Stree.Functional_Multiway_Trees;

package Functional_Tree_Tests is

   --  The tests are performed on a tree instantiation with Integer elements,
   --  with order 3 (i.e. a 3-way tree) with ways: left, middle, and right.

   type Direction is (Left, Middle, Right);

   package Integer_3Way_Trees is new Stree.Functional_Multiway_Trees
     (Element_Type => Integer,
      Way_Type     => Direction);

end Functional_Tree_Tests;