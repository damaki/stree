--
--  Copyright 2025 (C) Daniel King
--
--  SPDX-License-Identifier: Apache-2.0
--
with Ada.Unchecked_Deallocation;

with SPARK.Containers;

package body Stree.Functional_Multiway_Trees with
  SPARK_Mode => Off
is

   procedure Unchecked_Free_Element is new
     Ada.Unchecked_Deallocation
       (Object => Element_Type,
        Name   => Element_Access);

   procedure Unchecked_Free_Element_Ref is new
     Ada.Unchecked_Deallocation
       (Object => Refcounted_Element,
        Name   => Refcounted_Element_Access);

   procedure Unchecked_Free_Node is new
     Ada.Unchecked_Deallocation
       (Object => Node_Type,
        Name   => Node_Access);

   procedure Unchecked_Free_Tree is new
     Ada.Unchecked_Deallocation
       (Object => Refcounted_Tree,
        Name   => Refcounted_Tree_Access);

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Tree) return Boolean is
      use type Node_Maps.Cursor;

      L, R     : Node_Access;
      L_Parent : Node_Access;
      Way      : Way_Type;
      Pos      : Node_Maps.Cursor;

   begin
      if Left.Ref.Ref = null then
         return True;
      elsif Right.Ref.Ref = null then
         return False;
      end if;

      --  Go depth-first down the first key in each child until a difference
      --  is found, or a leaf is found.

      L := Left.Ref.Ref.all.Root_Node;
      R := Right.Ref.Ref.all.Root_Node;

      while not Node_Maps.Is_Empty (L.all.Children) loop
         Way  := Node_Maps.First_Key (L.all.Children);

         if not Node_Maps.Contains (R.all.Children, Way) then
            return False;
         end if;

         L := Node_Maps.Element (L.all.Children, Way);
         R := Node_Maps.Element (R.all.Children, Way);
      end loop;

      --  Iterate node-by-node in L, ensuring that the same path exists in R.

      while L /= null loop

         --  Compare elements at this first node

         if L.all.Element.Ref.all.Element.all /=
            R.all.Element.Ref.all.Element.all
         then
            return False;
         end if;

         L_Parent := L.all.Parent;

         if L_Parent = null then
            return True;
         end if;

         --  Find the next sibling node in L

         Way := L.all.Way_From_Parent;
         Pos := Node_Maps.Find (L_Parent.all.Children, Way);
         Pos := Node_Maps.Next (L_Parent.all.Children, Pos);

         if Pos = Node_Maps.No_Element then
            --  No more siblings. Go up to the parent.

            L := L.all.Parent;
            R := R.all.Parent;
         else
            --  Sibling exists in Left. Check if it exists in Right too.

            Way := Node_Maps.Key (L_Parent.all.Children, Pos);

            if not Node_Maps.Contains (R.all.Parent.all.Children, Way) then
               return False;
            end if;

            --  Both trees have the same sibling node. Go depth-first down that
            --  sibling to find the first leaf node in Left, checking along the
            --  way that the same path exists in Right.

            L := Node_Maps.Element (L.all.Parent.all.Children, Way);
            R := Node_Maps.Element (R.all.Parent.all.Children, Way);

            while not Node_Maps.Is_Empty (L.all.Children) loop
               Way  := Node_Maps.First_Key (L.all.Children);

               if not Node_Maps.Contains (R.all.Children, Way) then
                  return False;
               end if;

               L := Node_Maps.Element (L.all.Children, Way);
               R := Node_Maps.Element (R.all.Children, Way);
            end loop;
         end if;
      end loop;

      return True;
   end "<=";

   ---------
   -- "=" --
   ---------

   overriding
   function "=" (Left, Right : Tree) return Boolean is
      L, R : Node_Access;
   begin
      if not Same_Nodes (Left, Right) then
         return False;
      end if;

      if Left.Ref.Ref = null then
         return True;
      end if;

      L := First_Node (Left.Ref.Ref.all.Root_Node);
      R := First_Node (Right.Ref.Ref.all.Root_Node);

      while L /= null loop
         pragma Loop_Invariant (R /= null);

         if L.all.Element.Ref.all.Element.all /=
            R.all.Element.Ref.all.Element.all
         then
            return False;
         end if;

         L := Next_Node (L);
         R := Next_Node (R);
      end loop;

      return True;
   end "=";

   ---------
   -- Add --
   ---------

   function Add
     (Container : Tree;
      New_Item  : Element_Type;
      New_Node  : Path_Type) return Tree
   is
      use Way_Sequences;

      New_Element : Controlled_Element_Access;
      New_Tree : Tree;
      Node     : Node_Access;
      Way      : Way_Type;

   begin
      if Container.Ref.Ref = null then
         return Create_Tree (New_Item);
      end if;

      New_Tree := Tree'(Ref => Copy_Tree_Except_Subtree
                                 (Root    => Container.Ref.Ref.all.Root_Node,
                                  Exclude => null));

      pragma Assert (Elements_Equal (Container, New_Tree));
      pragma Assert (Elements_Equal (New_Tree, Container));

      Node := Find_Node (New_Tree, Parent (New_Node));
      Way  := Get (New_Node, Length (New_Node));

      --  Guard against precondition violations

      if Node = null then
         raise Constraint_Error;
      end if;

      if Node_Maps.Contains (Node.all.Children, Way) then
         raise Constraint_Error;
      end if;

      New_Element := (Ada.Finalization.Controlled with
                      Ref => new Refcounted_Element'
                               (Refcount => 1,
                                Element  => new Element_Type'(New_Item)));

      pragma Assert (Elements_Equal (Container, New_Tree));

      Node_Maps.Insert
        (Container => Node.all.Children,
         Key       => Way,
         New_Item  => new Node_Type'(Parent          => Node,
                                     Way_From_Parent => Way,
                                     Children        => Node_Maps.Empty_Map,
                                     Element         => New_Element));

      pragma Assert (Elements_Equal (Container, New_Tree));

      return New_Tree;
   end Add;

   ----------------
   -- Add_Parent --
   ----------------

   function Add_Parent
     (Container : Tree;
      New_Item  : Element_Type;
      Node      : Path_Type;
      Way       : Way_Type) return Tree
   is
      Element : constant Controlled_Element_Access :=
                  (Ada.Finalization.Controlled with
                   Ref => new Refcounted_Element'
                            (Refcount => 1,
                             Element  => new Element_Type'(New_Item)));
      New_Tree     : Tree;
      Node_Acc     : Node_Access;
      New_Node     : Node_Access;
      New_Children : Node_Maps.Map := Node_Maps.Empty_Map;

   begin
      if Container.Ref.Ref = null then
         return Create_Tree (New_Item);
      end if;

      New_Tree := Tree'(Ref => Copy_Tree_Except_Subtree
                                 (Root    => Container.Ref.Ref.all.Root_Node,
                                  Exclude => null));

      Node_Acc := Find_Node (New_Tree, Node);

      --  Guard against precondition violations

      if Node_Acc = null then
         raise Constraint_Error;
      end if;

      --  Create the new node and link it with its parent and new child

      Node_Maps.Insert (New_Children, Way, Node_Acc);

      New_Node := new Node_Type'
                        (Parent          => Node_Acc.all.Parent,
                         Way_From_Parent => Node_Acc.all.Way_From_Parent,
                         Children        => New_Children,
                         Element         => Element);

      if Node_Acc = New_Tree.Ref.Ref.all.Root_Node then
         New_Tree.Ref.Ref.all.Root_Node := New_Node;

      else
         --  Update the parent to replace its child with New_Node

         Node_Maps.Replace_Element
           (Container => New_Node.all.Parent.all.Children,
            Position  => Node_Maps.Find (New_Node.all.Parent.all.Children,
                                       New_Node.all.Way_From_Parent),
            New_Item  => New_Node);
      end if;

      --  Link old Node with its new parent

      Node_Acc.all.Parent          := New_Node;
      Node_Acc.all.Way_From_Parent := Way;

      return New_Tree;
   end Add_Parent;

   ------------
   -- Adjust --
   ------------

   overriding
   procedure Adjust (C_E : in out Controlled_Element_Access) is
   begin
      if C_E.Ref /= null then
         C_E.Ref.all.Refcount := @ + 1;
      end if;
   end Adjust;

   overriding
   procedure Adjust (C_T : in out Controlled_Tree_Access) is
   begin
      if C_T.Ref /= null then
         C_T.Ref.all.Refcount := @ + 1;
      end if;
   end Adjust;

   ------------
   -- Choose --
   ------------

   function Choose (Container : Tree) return Path_Type is
   begin
      if Container.Ref.Ref = null then
         raise Constraint_Error;
      end if;

      return Path_To_Node (First_Node (Container.Ref.Ref.all.Root_Node));
   end Choose;

   ------------
   -- Concat --
   ------------

   function Concat (Left, Right : Path_Type) return Path_Type is
      Result : Path_Type := Left;
   begin
      for Value of Right loop
         Result := Way_Sequences.Add (Result, Value);
      end loop;

      return Result;
   end Concat;

   --------------
   -- Contains --
   --------------

   function Contains
     (Container : Tree;
      Path      : Path_Type) return Boolean
   is
     (Find_Node (Container, Path) /= null);

   ------------------------------
   -- Copy_Tree_Except_Subtree --
   ------------------------------

   function Copy_Tree_Except_Subtree
     (Root    : Not_Null_Node_Access;
      Exclude : Node_Access) return Controlled_Tree_Access
   is
      use type Node_Maps.Cursor;

      Old_Node   : Node_Access;
      New_Node   : Node_Access;
      Old_Parent : Node_Access;
      Next       : Node_Access;
      Way        : Way_Type;
      Pos        : Node_Maps.Cursor;

   begin
      if Exclude = Root then
         return Controlled_Tree_Access'
                  (Ada.Finalization.Controlled with
                   Ref => null);
      end if;

      return New_Tree : constant Controlled_Tree_Access :=
                          (Ada.Finalization.Controlled with
                           Ref => new Refcounted_Tree'(Refcount  => 1,
                                                       Root_Node => null))
      do
         New_Tree.Ref.all.Root_Node :=
           new Node_Type'
             (Parent          => null,
              Way_From_Parent => Way_Type'First,
              Children        => Node_Maps.Empty_Map,
              Element         => Root.all.Element);

         Old_Node := Root;
         New_Node := New_Tree.Ref.all.Root_Node;

         while Old_Node /= null loop

            --  Iterate down the first branch of each node until we reach a
            --  leaf node, copying the nodes we visit to the new tree as we go.

            while not Node_Maps.Is_Empty (Old_Node.all.Children) loop
               Way     := Node_Maps.First_Key (Old_Node.all.Children);
               Next    := Node_Maps.Element (Old_Node.all.Children, Way);

               exit when Next = Exclude;

               Node_Maps.Insert
                 (Container => New_Node.all.Children,
                  Key       => Way,
                  New_Item  => new Node_Type'
                                    (Parent          => New_Node,
                                     Way_From_Parent => Way,
                                     Children        => Node_Maps.Empty_Map,
                                     Element         => Next.all.Element));

               Old_Node := Next;
               New_Node := Node_Maps.Element (New_Node.all.Children, Way);
            end loop;

            --  Find the next sibling and continue. If this node has no
            --  sibling, then move up the tree until one is found.

            Old_Parent := Old_Node.all.Parent;

            while Old_Parent /= null loop

               --  Get the next sibling of Old_Node, skipping over the excluded
               --  node if found.

               Pos := Node_Maps.Find (Old_Parent.all.Children,
                                      Old_Node.all.Way_From_Parent);

               loop
                  Pos  := Node_Maps.Next (Old_Parent.all.Children, Pos);
                  exit when Pos = Node_Maps.No_Element;

                  Next := Node_Maps.Element (Old_Parent.all.Children, Pos);
                  exit when Next /= Exclude;
               end loop;

               if Pos = Node_Maps.No_Element then
                  --  No sibling. Move up the tree and try again.

                  Old_Node   := Old_Node.all.Parent;
                  New_Node   := New_Node.all.Parent;
                  Old_Parent := Old_Parent.all.Parent;

               else
                  Way  := Node_Maps.Key (Old_Parent.all.Children, Pos);
                  Next := Node_Maps.Element (Old_Parent.all.Children, Way);

                  Node_Maps.Insert
                    (Container => New_Node.all.Parent.all.Children,
                     Key       => Way,
                     New_Item  => new Node_Type'
                                       (Parent          => New_Node.all.Parent,
                                        Way_From_Parent => Way,
                                        Children        => Node_Maps.Empty_Map,
                                        Element         => Next.all.Element));

                  Old_Node   := Next;
                  New_Node   := Node_Maps.Element
                                  (New_Node.all.Parent.all.Children, Way);
                  Old_Parent := Old_Node.Parent;
                  exit;
               end if;
            end loop;

            exit when Old_Parent = null;
         end loop;
      end return;
   end Copy_Tree_Except_Subtree;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree (New_Item : Element_Type) return Tree is
     (Tree'
        (Ref =>
           (Ada.Finalization.Controlled with
            Ref => new Refcounted_Tree'
                     (Refcount  => 1,
                      Root_Node =>
                        new Node_Type'
                          (Parent          => null,
                           Way_From_Parent => Way_Type'First,
                           Children        => Node_Maps.Empty_Map,
                           Element         =>
                             (Ada.Finalization.Controlled with
                              Ref => new Refcounted_Element'
                                       (Refcount => 1,
                                        Element  =>
                                          new Element_Type'(New_Item))))))));

   -------------------------
   -- Element_Logic_Equal --
   -------------------------

   function Element_Logic_Equal (Left, Right : Element_Type) return Boolean is
   begin
      SPARK.Containers.Check_Or_Fail;
      return Left = Right;
   end Element_Logic_Equal;

   --------------------
   -- Elements_Equal --
   --------------------

   function Elements_Equal (Left, Right : Tree) return Boolean is
      L, R    : Node_Access;
      Success : Boolean;

   begin
      SPARK.Containers.Check_Or_Fail;

      if Left.Ref.Ref = null then
         return True;
      elsif Right.Ref.Ref = null then
         return False;
      end if;

      L := Left.Ref.Ref.all.Root_Node;
      R := Right.Ref.Ref.all.Root_Node;

      First_Node_In_Both (L, R, Success);

      while Success and then L /= null loop
         if not Element_Logic_Equal
              (L.all.Element.Ref.all.Element.all,
               R.all.Element.Ref.all.Element.all)
         then
            return False;
         end if;

         Next_Node_In_Both (L, R, Success);
      end loop;

      return Success;
   end Elements_Equal;

   ---------------------------
   -- Elements_Equal_Except --
   ---------------------------

   function Elements_Equal_Except
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   is
      Excluded_Node_Acc : constant Node_Access :=
                            Find_Node (Left, Excluded_Node);

      L, R    : Node_Access;
      Success : Boolean;
   begin
      SPARK.Containers.Check_Or_Fail;

      --  If the root node is excluded, then the entire Tree of Left is
      --  excluded, so we are effectively comparing an empty tree.

      if Is_Root (Excluded_Node) then
         return True;
      end if;

      --  If Excluded_Node does not exist in Left, then we can simply compare
      --  the trees directly. This also covers the case when Left is empty.

      if Excluded_Node_Acc = null then
         return Elements_Equal (Left, Right);
      end if;

      pragma Assert (Left.Ref.Ref /= null);

      if Right.Ref.Ref = null then
         return False;
      end if;

      L := Left.Ref.Ref.all.Root_Node;
      R := Right.Ref.Ref.all.Root_Node;

      First_Node_In_Both (L, R, Success);

      while Success and then L /= null loop
         if L /= Excluded_Node_Acc
            and then not Element_Logic_Equal
                           (L.all.Element.Ref.all.Element.all,
                            R.all.Element.Ref.all.Element.all)
         then
            return False;
         end if;

         Next_Node_In_Both (L, R, Success);
      end loop;

      return Success;
   end Elements_Equal_Except;

   -----------------------------------
   -- Elements_Equal_Except_Subtree --
   -----------------------------------

   function Elements_Equal_Except_Subtree
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   is
      Excluded_Node_Acc : constant Node_Access :=
                            Find_Node (Left, Excluded_Node);

      L, R    : Node_Access;
      Success : Boolean;
   begin
      SPARK.Containers.Check_Or_Fail;

      if Left.Ref.Ref = null then
         return True;
      end if;

      --  If the root node is excluded, then the entire Tree of Left is
      --  excluded, so we are effectively comparing an empty tree.

      if Is_Root (Excluded_Node) then
         return True;
      end if;

      L := Left.Ref.Ref.all.Root_Node;
      R := Right.Ref.Ref.all.Root_Node;

      First_Node_In_Both_Except (L, R, Excluded_Node_Acc, Success);

      while Success and then L /= null loop
         if not Element_Logic_Equal
              (L.all.Element.Ref.all.Element.all,
               R.all.Element.Ref.all.Element.all)
         then
            return False;
         end if;

         Next_Node_In_Both_Except (L, R, Excluded_Node_Acc, Success);
      end loop;

      return Success;
   end Elements_Equal_Except_Subtree;

   --------------------------------
   -- Elements_Equal_In_Subtrees --
   --------------------------------

   function Elements_Equal_In_Subtrees
     (Left, Right   : Tree;
      Left_Subtree  : Path_Type;
      Right_Subtree : Path_Type) return Boolean
   is
      LS      : Node_Access;
      L, R    : Node_Access;
      Success : Boolean;

   begin
      SPARK.Containers.Check_Or_Fail;

      if Left.Ref.Ref = null then
         return True;
      elsif Right.Ref.Ref = null then
         return False;
      end if;

      L := Find_Node (Left, Left_Subtree);
      R := Find_Node (Right, Right_Subtree);

      LS := L;

      if R = null then
         return False;
      end if;

      First_Node_In_Both (L, R, Success);

      while Success and then L /= null loop
         if not Element_Logic_Equal
              (L.all.Element.Ref.all.Element.all,
               R.all.Element.Ref.all.Element.all)
         then
            return False;
         end if;

         --  Don't go beyond the subtree

         exit when L = LS;

         Next_Node_In_Both (L, R, Success);
      end loop;

      return Success;
   end Elements_Equal_In_Subtrees;

   ----------------
   -- Empty_Tree --
   ----------------

   function Empty_Tree return Tree is
     (Tree'(Ref => (Ada.Finalization.Controlled with Ref => null)));

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (C_E : in out Controlled_Element_Access) is
   begin
      if C_E.Ref /= null then
         C_E.Ref.all.Refcount := @ - 1;

         if C_E.Ref.all.Refcount = 0 then
            Unchecked_Free_Element (C_E.Ref.all.Element);
            Unchecked_Free_Element_Ref (C_E.Ref);
         end if;

         C_E.Ref := null;
      end if;
   end Finalize;

   overriding
   procedure Finalize (C_T : in out Controlled_Tree_Access) is
      Node      : Node_Access;
      Next      : Node_Access;

   begin
      if C_T.Ref /= null then
         C_T.Ref.all.Refcount := @ - 1;

         if C_T.Ref.all.Refcount = 0 then

            --  Iterate through the tree and free each node. Note that the
            --  iteration is performed depth-first, so child nodes will always
            --  be freed before their parent.

            Node := First_Node (C_T.Ref.all.Root_Node);

            while Node /= null loop
               Next := Next_Node (Node);
               Unchecked_Free_Node (Node);
               Node := Next;
            end loop;

            --  All nodes are freed. Now free the reference.

            Unchecked_Free_Tree (C_T.Ref);
         end if;

         C_T.Ref := null;
      end if;
   end Finalize;

   ---------------
   -- Find_Node --
   ---------------

   function Find_Node
     (Container : Tree;
      Path      : Path_Type) return Node_Access
   is
      Node : Node_Access;

   begin
      if Container.Ref.Ref = null then
         return null;
      end if;

      Node := Container.Ref.Ref.Root_Node;

      --  Follow the path down the tree from the root

      for Way of Path loop
         exit when Node = null;

         if Node_Maps.Contains (Node.all.Children, Way) then
            Node := Node_Maps.Element (Node.all.Children, Way);
         else
            Node := null;
         end if;
      end loop;

      return Node;
   end Find_Node;

   ----------------
   -- First_Node --
   ----------------

   function First_Node
     (Node : Not_Null_Node_Access) return Not_Null_Node_Access
   is
      Current_Node : Not_Null_Node_Access := Node;
   begin
      while not Node_Maps.Is_Empty (Current_Node.all.Children) loop
         Current_Node := Node_Maps.First_Element (Current_Node.all.Children);
      end loop;

      return Current_Node;
   end First_Node;

   ------------------------
   -- First_Node_In_Both --
   ------------------------

   procedure First_Node_In_Both
     (Left  : in out Node_Access;
      Right : in out Node_Access;
      Success :    out Boolean)
   is
   begin
      First_Node_In_Both_Except (Left, Right, null, Success);
   end First_Node_In_Both;

   -------------------------------
   -- First_Node_In_Both_Except --
   -------------------------------

   procedure First_Node_In_Both_Except
     (Left          : in out Node_Access;
      Right         : in out Node_Access;
      Excluded_Node :        Node_Access;
      Success       :    out Boolean)
   is
      use type Node_Maps.Cursor;

      L, R : Node_Access;
      Next : Node_Access;
      Way  : Way_Type;
      Pos  : Node_Maps.Cursor;

   begin
      L := Left;
      R := Right;

      Pos := Node_Maps.First (L.all.Children);

      while Pos /= Node_Maps.No_Element loop
         Next := Node_Maps.Element (L.all.Children, Pos);

         if Next = Excluded_Node then
            Pos := Node_Maps.Next (L.all.Children, Pos);

         else
            Way := Node_Maps.Key (L.all.Children, Pos);

            if not Node_Maps.Contains (R.all.Children, Way) then
               Success := False;
               return;
            end if;

            L   := Next;
            R   := Node_Maps.Element (R.all.Children, Way);
            Pos := Node_Maps.First (L.all.Children);
         end if;
      end loop;

      Left    := L;
      Right   := R;
      Success := True;
   end First_Node_In_Both_Except;

   ---------
   -- Get --
   ---------

   function Get (Container : Tree; Path : Path_Type) return Element_Type is
      Node : constant Node_Access := Find_Node (Container, Path);
   begin
      if Node = null then
         raise Constraint_Error;
      end if;

      return Node.all.Element.Ref.Element.all;
   end Get;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Tree) return Boolean is
     (Container.Ref.Ref = null or else Container.Ref.Ref.Root_Node = null);

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf
     (Container : Tree;
      Path      : Path_Type) return Boolean
   is
      Node : constant Node_Access := Find_Node (Container, Path);
   begin
      if Node = null then
         raise Constraint_Error;
      end if;

      return Node_Maps.Is_Empty (Node.all.Children);
   end Is_Leaf;

   ------------------
   -- Iter_Element --
   ------------------

   function Iter_Element
     (Container : Tree;
      Path : Private_Path) return Path_Type
   is
      pragma Unreferenced (Container);
   begin
      return Path.Path;
   end Iter_Element;

   ----------------------
   -- Iter_Has_Element --
   ----------------------

   function Iter_Has_Element
     (Container : Tree;
      Path      : Private_Path) return Boolean
   is
   begin
      return Path.Valid and then Contains (Container, Path.Path);
   end Iter_Has_Element;

   ----------------
   -- Iter_First --
   ----------------

   function Iter_First (Container : Tree) return Private_Path is
   begin
      return
        (if Container.Ref.Ref = null
         then No_Element
         else Private_Path'
                (Path  => Path_To_Node
                            (First_Node (Container.Ref.Ref.all.Root_Node)),
                 Valid => True));
   end Iter_First;

   ---------------
   -- Iter_Next --
   ---------------

   function Iter_Next
     (Container : Tree;
      Path      : Private_Path) return Private_Path
   is
      use Way_Sequences;

      Node : Node_Access;

   begin
      if not Path.Valid then
         raise Constraint_Error;
      end if;

      --  If the Path references the root, then end the iteration.

      if Is_Empty (Container) or else Length (Path.Path) = 0 then
         return No_Element;
      end if;

      Node := Find_Node (Container, Path.Path);

      if Node = null then
         raise Constraint_Error;
      end if;

      Node := Next_Node (Node);

      if Node = null then
         return No_Element;
      else
         return Private_Path'
                  (Path  => Path_To_Node (Node),
                   Valid => True);
      end if;
   end Iter_Next;

   -------------------------------
   -- Lemma_Ancestor_Transitive --
   -------------------------------

   procedure Lemma_Ancestor_Transitive (P1, P2, P3 : Path_Type) is null;

   -----------------------------
   -- Lemma_Contains_Ancestor --
   -----------------------------

   procedure Lemma_Contains_Ancestor
     (Container : Tree;
      P1, P2    : Path_Type)
   is null;

   -------------------------------
   -- Lemma_Contains_Transitive --
   -------------------------------

   procedure Lemma_Contains_Transitive
     (Container : Tree;
      P1, P2    : Path_Type)
   is null;

   -------------------------
   -- Lemma_Contains_Root --
   -------------------------

   procedure Lemma_Contains_Root (Container : Tree) is null;

   --------------------------
   -- Lemma_Get_Transitive --
   --------------------------

   procedure Lemma_Get_Transitive
     (Container : Tree;
      P1, P2    : Path_Type)
   is null;

   ------------------------------------
   -- Lemma_Not_Contains_Descendants --
   ------------------------------------

   procedure Lemma_Not_Contains_Descendants
     (Container : Tree;
      P1, P2    : Path_Type)
   is null;

   -----------------------------------------
   -- Lemma_Not_Contains_Leaf_Descendants --
   -----------------------------------------

   procedure Lemma_Not_Contains_Leaf_Descendants
     (Container : Tree;
      P1, P2    : Path_Type)
   is null;

   -----------------------------------------------
   -- Lemma_Path_Length_Within_Container_Length --
   -----------------------------------------------

   procedure Lemma_Path_Length_Within_Container_Length
     (Container : Tree;
      Node      : Path_Type)
   is null;

   ------------
   -- Length --
   ------------

   function Length (Container : Tree) return SPARK.Big_Integers.Big_Natural is
      Node   : Node_Access;
      Result : SPARK.Big_Integers.Big_Natural := 0;
   begin
      if Container.Ref.Ref = null then
         return 0;
      end if;

      Node := First_Node (Container.Ref.Ref.all.Root_Node);

      while Node /= null loop
         Result := Result + 1;
         Node   := Next_Node (Node);
      end loop;

      return Result;
   end Length;

   ---------------
   -- Next_Node --
   ---------------

   function Next_Node (Node : Not_Null_Node_Access) return Node_Access is
      use type Node_Maps.Cursor;

      Parent : constant Node_Access := Node.all.Parent;
      Way    : Way_Type;
      Pos    : Node_Maps.Cursor;

   begin

      --  The root is the last node in the iteration

      if Parent = null then
         return null;
      end if;

      --  Find the next sibling node

      Way := Node.all.Way_From_Parent;
      Pos := Node_Maps.Find (Parent.all.Children, Way);

      pragma Assert (Node_Maps.Has_Element (Parent.all.Children, Pos));

      Pos := Node_Maps.Next (Parent.all.Children, Pos);

      --  If a sibling was found, then return the first leaf node in that
      --  subtree. Otherwise, if no sibling was found, then the Parent is the
      --  next node in the iteration.

      if Pos /= Node_Maps.No_Element then
         return First_Node (Node_Maps.Element (Parent.all.Children, Pos));
      else
         return Parent;
      end if;
   end Next_Node;

   -----------------------
   -- Next_Node_In_Both --
   -----------------------

   procedure Next_Node_In_Both
     (Left  : in out Node_Access;
      Right : in out Node_Access;
      Success :    out Boolean)
   is
   begin
      Next_Node_In_Both_Except (Left, Right, null, Success);
   end Next_Node_In_Both;

   ------------------------------
   -- Next_Node_In_Both_Except --
   ------------------------------

   procedure Next_Node_In_Both_Except
     (Left          : in out Node_Access;
      Right         : in out Node_Access;
      Excluded_Node :        Node_Access;
      Success       :    out Boolean)
   is
      use type Node_Maps.Cursor;

      L, R     : Node_Access;
      Way      : Way_Type;
      Pos      : Node_Maps.Cursor;

   begin
      L := Left;
      R := Right;

      if L.all.Parent = null then
         Left    := null;
         Right   := null;
         Success := True;
         return;
      end if;

      --  Find the next sibling node in L, skipping over the Excluded_Node
      --  if encountered.

      Way := L.all.Way_From_Parent;
      Pos := Node_Maps.Find (L.all.Parent.all.Children, Way);

      loop
         Pos := Node_Maps.Next (L.all.Parent.all.Children, Pos);
         exit when Pos = Node_Maps.No_Element;
         exit when Node_Maps.Element (L.all.Parent.all.Children, Pos) /=
                     Excluded_Node;
      end loop;

      if Pos = Node_Maps.No_Element then
         --  No more siblings. Go up to the parent.

         L       := L.all.Parent;
         R       := R.all.Parent;
         Success := True;
      else
         --  Sibling exists in Left. Check if it exists in Right too.

         Way := Node_Maps.Key (L.all.Parent.all.Children, Pos);

         if not Node_Maps.Contains (R.all.Parent.all.Children, Way) then
            Success := False;
            return;
         end if;

         --  Move to sibling node

         L := Node_Maps.Element (L.all.Parent.all.Children, Pos);
         R := Node_Maps.Element (R.all.Parent.all.Children, Pos);

         pragma Assert (L /= Excluded_Node);

         First_Node_In_Both_Except (L, R, Excluded_Node, Success);
      end if;

      Left    := L;
      Right   := R;
   end Next_Node_In_Both_Except;

   --------------------
   -- Nodes_Included --
   --------------------

   function Nodes_Included (Left, Right : Tree) return Boolean is
     (Nodes_Included_In_Subtrees (Left, Right, Root_Node, Root_Node));

   -----------------------------------
   -- Nodes_Included_Except_Subtree --
   -----------------------------------

   function Nodes_Included_Except_Subtree
     (Left          : Tree;
      Right         : Tree;
      Excluded_Node : Path_Type) return Boolean
   is
      Excluded_Node_Acc : constant Node_Access :=
                            Find_Node (Left, Excluded_Node);

      L, R    : Node_Access;
      Success : Boolean;
   begin
      --  If the root node is excluded, then the entire Tree of Left is
      --  excluded, so we are effectively comparing an empty tree.

      if Is_Root (Excluded_Node) or else Left.Ref.Ref = null then
         return True;
      end if;

      if Right.Ref.Ref = null then
         return False;
      end if;

      L := Left.Ref.Ref.all.Root_Node;
      R := Right.Ref.Ref.all.Root_Node;

      First_Node_In_Both_Except (L, R, Excluded_Node_Acc, Success);

      while Success and then L /= null loop
         Next_Node_In_Both_Except (L, R, Excluded_Node_Acc, Success);
      end loop;

      return Success;
   end Nodes_Included_Except_Subtree;

   --------------------------------
   -- Nodes_Included_In_Subtrees --
   --------------------------------

   function Nodes_Included_In_Subtrees
     (Left, Right   : Tree;
      Left_Subtree  : Path_Type;
      Right_Subtree : Path_Type) return Boolean
   is
      L, R    : Node_Access;
      Success : Boolean;

   begin
      if Is_Empty (Left) then
         return True;
      elsif Is_Empty (Right) then
         return False;
      end if;

      L := Find_Node (Left, Left_Subtree);

      if L = null then
         return True; --  subtree does not exist in Left
      end if;

      R := Find_Node (Right, Right_Subtree);

      if R = null then
         return False; --  subtree exists in Left, but not Right
      end if;

      First_Node_In_Both (L, R, Success);

      while Success and then L /= null loop
         Next_Node_In_Both (L, R, Success);
      end loop;

      return Success;
   end Nodes_Included_In_Subtrees;

   ------------------
   -- Path_To_Node --
   ------------------

   function Path_To_Node (Node : Not_Null_Node_Access) return Path_Type is
      Path : Path_Type   := Way_Sequences.Empty_Sequence;
      N    : Node_Access := Node;

   begin
      --  Walk up the tree towards the root, building the path as we go

      while N /= null loop
         Path := Way_Sequences.Add (Path, 1, N.all.Way_From_Parent);
         N    := N.all.Parent;
      end loop;

      return Path;
   end Path_To_Node;

   ------------
   -- Remove --
   ------------

   function Remove (Container : Tree; Node : Path_Type) return Tree is
      Excluded_Node : constant Node_Access := Find_Node (Container, Node);
   begin
      if Excluded_Node = null then
         return Container;
      else
         return Tree'(Ref => Copy_Tree_Except_Subtree
                               (Root    => Container.Ref.Ref.all.Root_Node,
                                Exclude => Excluded_Node));
      end if;
   end Remove;

   ------------------
   -- Remove_Front --
   ------------------

   function Remove_Front
     (Path  : Path_Type;
      Count : SPARK.Big_Integers.Big_Natural) return Path_Type
   is
      use Way_Sequences;

      Result : Path_Type                      := Path;
      I      : SPARK.Big_Integers.Big_Natural := 0;

   begin
      while I < Count loop
         Result := Remove (Result, 1);
         I      := I + 1;
      end loop;

      return Result;
   end Remove_Front;

   ----------------
   -- Same_Nodes --
   ----------------

   function Same_Nodes (Left, Right : Tree) return Boolean is
     (Same_Nodes_In_Subtrees (Left, Right, Root_Node, Root_Node));

   ----------------------------
   -- Same_Nodes_In_Subtrees --
   ----------------------------

   function Same_Nodes_In_Subtrees
     (Left, Right   : Tree;
      Left_Subtree  : Path_Type;
      Right_Subtree : Path_Type) return Boolean
   is
      use type Node_Maps.Cursor;

      L, R : Node_Access;

      L_Pos, R_Pos : Node_Maps.Cursor;

      LS_Root : Node_Access;
      RS_Root : Node_Access;

   begin
      if Is_Empty (Left) /= Is_Empty (Right) then
         return False;
      end if;

      if Is_Empty (Left) then
         return True;
      end if;

      LS_Root := Find_Node (Left, Left_Subtree);
      RS_Root := Find_Node (Right, Right_Subtree);

      if LS_Root = null then
         --  Subtree does not exist in Left. Check that is also doens't exist
         --  in the Right tree.
         return RS_Root = null;

      elsif RS_Root = null then
         --  Subtree exists in Right but not Left
         return False;

      end if;

      --  Go node-by-node and compare structures

      L := First_Node (LS_Root);
      R := First_Node (RS_Root);

      loop

         --  Check that both nodes have the same branches

         L_Pos := Node_Maps.First (L.all.Children);
         R_Pos := Node_Maps.First (R.all.Children);

         while L_Pos /= Node_Maps.No_Element loop
            if Node_Maps.Key (L.all.Children, L_Pos) /=
               Node_Maps.Key (R.all.Children, R_Pos)
            then
               return False;
            end if;

            L_Pos := Node_Maps.Next (L.all.Children, L_Pos);
            R_Pos := Node_Maps.Next (R.all.Children, R_Pos);
         end loop;

         if L_Pos /= Node_Maps.No_Element then
            return False;
         end if;

         --  Stop when we reach the subtree root to avoid comparing nodes
         --  outside of the subtree.

         exit when L = LS_Root or else R = RS_Root;

         --  Move to the next node in the subtree

         L := Next_Node (L);
         R := Next_Node (R);
      end loop;

      return L = LS_Root and then R = RS_Root;
   end Same_Nodes_In_Subtrees;

   ---------
   -- Set --
   ---------

   function Set
     (Container : Tree;
      Node      : Path_Type;
      New_Item  : Element_Type) return Tree
   is
      New_Tree : Tree;
      Node_Acc : Node_Access;

   begin
      if Is_Empty (Container) then
         raise Constraint_Error;
      end if;

      New_Tree := (Ref => Copy_Tree_Except_Subtree
                            (Root    => Container.Ref.Ref.all.Root_Node,
                             Exclude => null));

      Node_Acc := Find_Node (New_Tree, Node);

      if Node_Acc = null then
         raise Constraint_Error;
      end if;

      Node_Acc.Element :=
        (Ada.Finalization.Controlled with
         Ref => new Refcounted_Element'
                      (Refcount => 1,
                       Element  => new Element_Type'(New_Item)));

      return New_Tree;
   end Set;

   -----------------
   -- Splice_Path --
   -----------------

   function Splice_Path
     (Left_Subtree  : Path_Type;
      Left               : Path_Type;
      Right_Subtree : Path_Type) return Path_Type
   is
      use Way_Sequences;
   begin
      return Concat (Right_Subtree,
                     Remove_Front (Left, Length (Left_Subtree)));
   end Splice_Path;

end Stree.Functional_Multiway_Trees;
