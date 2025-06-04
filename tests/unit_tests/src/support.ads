--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Stree.Unbounded_Multiway_Trees;

generic
   with package Trees is new Stree.Unbounded_Multiway_Trees
     (Element_Type => Integer, Way_Type => <>);
package Support is

   type Cursor_Map is array (Integer range <>) of Trees.Cursor;

   type Cursor_Way_Map is
     array (Integer range <>, Trees.Way_Type range <>)
     of Trees.Cursor;

   procedure Insert_Child
     (Container : in out Trees.Tree;
      New_Item  :        Integer;
      Parent    :        Integer;
      Way       :        Trees.Way_Type;
      Nodes     : in out Cursor_Map)
   with
     Pre => New_Item in Nodes'Range
            and then Parent in Nodes'Range;
   --  Call Trees.Insert_Child and store a cursor to the new node in Nodes.
   --
   --  @param Container The tree to modify.
   --  @param New_Item  The element to insert into the tree. This is used as
   --                   the index into Nodes to store the cursor to the new
   --                   node.
   --  @param Parent The index into Nodes of the parent node. The new node
   --                will be inserted as the child of that parent node.
   --  @param Way The way for the new child.
   --  @param Nodes Array of cursors to update with the new node.

   procedure Check_Parents
     (Container        : Trees.Tree;
      Nodes            : Cursor_Map;
      Expected_Parents : Cursor_Map)
   with
     Pre => Nodes'First = Expected_Parents'First
            and then Nodes'Last = Expected_Parents'Last;
   --  Check that the parent of each node in Nodes is equal to the associated
   --  value in Expected_Parents at the same index.

   procedure Check_Children
     (Container         : Trees.Tree;
      Nodes             : Cursor_Map;
      Expected_Children : Cursor_Way_Map)
   with
     Pre => Nodes'First = Expected_Children'First (1)
            and then Nodes'Last = Expected_Children'Last (1);
   --  Check that each child of each node in Nodes is equal to the associated
   --  value in Expected_Children at the same index.

end Support;