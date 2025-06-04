--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with AUnit.Assertions; use AUnit.Assertions;

package body Support is

   ------------------
   -- Insert_Child --
   ------------------

   procedure Insert_Child
     (Container : in out Trees.Tree;
      New_Item  :        Integer;
      Parent    :        Integer;
      Way       :        Trees.Way_Type;
      Nodes     : in out Cursor_Map)
   is
   begin
      Trees.Insert_Child
        (Container => Container,
         New_Item  => New_Item,
         Position  => Nodes (Parent),
         Way       => Way);

      Nodes (New_Item) := Trees.Child (Container, Nodes (Parent), Way);
   end Insert_Child;

   -------------------
   -- Check_Parents --
   -------------------

   procedure Check_Parents
     (Container        : Trees.Tree;
      Nodes            : Cursor_Map;
      Expected_Parents : Cursor_Map)
   is
      use type Trees.Cursor;

      P : Trees.Cursor;

   begin
      for I in Nodes'Range loop
         P := Trees.Parent (Container, Nodes (I));
         Assert (P = Expected_Parents (I),
                 "Node" & I'Image & " has wrong parent. (expected cursor:" &
                   Expected_Parents (I).Node'Image & ", got cursor:" &
                   P.Node'Image & ")");
      end loop;
   end Check_Parents;

   --------------------
   -- Check_Children --
   --------------------

   procedure Check_Children
     (Container         : Trees.Tree;
      Nodes             : Cursor_Map;
      Expected_Children : Cursor_Way_Map)
   is
      use type Trees.Cursor;

      P : Trees.Cursor;
      C : Trees.Cursor;
   begin
      for I in Nodes'Range loop
         for W in Trees.Way_Type loop
            P := Nodes (I);
            C := Trees.Child (Container, P, W);

            Assert (C = Expected_Children (I, W),
                    "Wrong child in way " & W'Image & " of cursor" &
                      P.Node'Image & " (expected cursor:" &
                      Expected_Children (I, W).Node'Image & ", got cursor:" &
                      C.Node'Image & ")");
         end loop;
      end loop;
   end Check_Children;

end Support;