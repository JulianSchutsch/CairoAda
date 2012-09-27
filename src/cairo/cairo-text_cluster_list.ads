------------------------------------------------------------------------------
--                                                                          --
--                  CairoAda - Ada95 binding for Cairo                      --
--                                                                          --
-- Copyright (C) 2006-2009, Damien Carbonne                                 --
--                                                                          --
-- This library is free software; you can redistribute it and/or modify it  --
-- under the terms of the GNU General Public License as published by the    --
-- Free Software Foundation; either version 2 of the License, or (at your   --
-- option) any later version.                                               --
--                                                                          --
-- This library is distributed in the hope that it will be useful,          --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of           --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.                                         --
--                                                                          --
-- You should have received a copy of the GNU General Public License along  --
-- with this library; if not, write to the Free Software Foundation,        --
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License. This exception does not       --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
------------------------------------------------------------------------------

--  This package defines Cairo_Text_Cluster_List, used to store a dynamically
--  allocated array of Cairo_Text_Clusters.
--  The Ada wrapper handles memory deallocation automatically.

with Ada.Finalization;

package Cairo.Text_Cluster_List is

   pragma Elaborate_Body;

   type Cairo_Text_Cluster_List is limited private;
   --  A dynamically allocated array of Cairo_text_Clusters.
   --  Memory is automatically freed.

   procedure Allocate
     (Text_Cluster_List : in out Cairo_Text_Cluster_List;
      Length : in Natural);
   --  Allocate an array of Cairo_text_Clusters.
   --  Previousy allocated array, if any, is freed.

   procedure Free
     (Text_Cluster_List : in out Cairo_Text_Cluster_List);
   --  Explicit request to free allocate mem reaches end of its scope.

   procedure Set_Text_Cluster
     (Text_Cluster_List : in out Cairo_Text_Cluster_List;
      Index : Natural;
      Text_Cluster : Cairo_Text_Cluster);
   --  Set the Index-th Text_Cluster.

   function Get_Length
     (Text_Cluster_List : Cairo_Text_Cluster_List)
      return Natural;
   --  Return the length of the allocated array or 0.

   function Get_Text_Cluster
     (Text_Cluster_List : Cairo_Text_Cluster_List;
      Index : Natural)
      return Cairo_Text_Cluster;
   --  Return the Index-th Text_Cluster.

   function To_Array
     (Text_Cluster_List : Cairo_Text_Cluster_List)
      return Cairo_Text_Cluster_Array;
   --  Return an array of Text_Clusters as an Ada array.

   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   procedure Set_Ptr
     (Text_Cluster_List : in out Cairo_Text_Cluster_List;
      Ptr : Text_Cluster_Ptr;
      Length : Natural);
   --  Set the C allocated array (cairo_text_cluster_t*) and its length
   --  Do NOT Free previously allocated array, if any.

   function Ptr
     (Text_Cluster_List : Cairo_Text_Cluster_List)
      return Text_Cluster_Ptr;
   --  Return the pointer to the C allocated array (cairo_text_cluster_t*).

private

   type Cairo_Text_Cluster_List is new Ada.Finalization.Limited_Controlled with record
      Ptr : Text_Cluster_Ptr;
      --  Pointer to the C allocated array (cairo_test_cluster_t*).
      Length : Natural := 0;
      -- Length of the allocated array.
   end record;
   procedure Finalize (O : in out Cairo_Text_Cluster_List);

end Cairo.Text_Cluster_List;
