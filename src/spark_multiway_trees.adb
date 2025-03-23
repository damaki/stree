--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
package body SPARK_Multiway_Trees with
  SPARK_Mode => On
is

   --------------------
   -- Tree_Structure --
   --------------------

   function Tree_Structure (F : Node_Maps.Map) return Boolean is
     (
      --  The parent of a node is either Empty or references another, valid
      --  node.
      (for all I of F =>
         Element (F, I).Parent = Empty
         or else Contains (F, Element (F, I).Parent))

      --  Each way of a node is either Empty or references another, valid node.
      and then
        (for all I of F =>
           (for all W of Element (F, I).Ways =>
              W = Empty or else Contains (F, W)))

      --  If a node has position Top then it has no parent, otherwise it
      --  has a valid parent
      and then
        (for all I of F =>
           (if Element (F, I).Position = Top
            then Element (F, I).Parent = Empty
            else Element (F, I).Parent /= Empty
                 and then Contains (F, Element (F, I).Parent)))

      --  If a node is a child (has a position), then it is the child of its
      --  parent.
      and then
        (for all I of F =>
           (if Element (F, I).Position /= Top then
              Element (F, Element (F, I).Parent).Ways (Element (F, I).Position)
                = I))
     );

end SPARK_Multiway_Trees;