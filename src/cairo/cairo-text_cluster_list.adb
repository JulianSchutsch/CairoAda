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

with Cairo.Support; use Cairo.Support;
with Interfaces.C.Pointers;

package body Cairo.Text_Cluster_List is

   Default_Text_Cluster : constant Cairo_Text_Cluster := (0, 0);

   package Text_Cluster_Pointers is new Interfaces.C.Pointers (
      Natural,
      Cairo_Text_Cluster,
      Cairo_Text_Cluster_Array,
      Default_Text_Cluster);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Text_Cluster_List : in out Cairo_Text_Cluster_List;
      Length            : in Natural)
   is
   begin
      Free (Text_Cluster_List);
      Text_Cluster_List.Ptr := cairo_text_cluster_allocate (int (Length));
      if Text_Cluster_List.Ptr /= null then
         Text_Cluster_List.Length := Length;
      end if;
   end Allocate;

   ----------
   -- Free --
   ----------

   procedure Free (Text_Cluster_List : in out Cairo_Text_Cluster_List) is
   begin
      if Text_Cluster_List.Ptr /= null then
         cairo_text_cluster_free (Text_Cluster_List.Ptr);
         Text_Cluster_List.Ptr    := null;
         Text_Cluster_List.Length := 0;
      end if;
   end Free;

   ----------------------
   -- Set_Text_Cluster --
   ----------------------

   procedure Set_Text_Cluster
     (Text_Cluster_List : in out Cairo_Text_Cluster_List;
      Index             : Natural;
      Text_Cluster      : Cairo_Text_Cluster)
   is
   begin
      if Index < Text_Cluster_List.Length then
         declare
            P : Text_Cluster_Pointers.Pointer :=
               Text_Cluster_Pointers.Pointer (Text_Cluster_List.Ptr);
            use Text_Cluster_Pointers;
         begin
            P     := P + Interfaces.C.ptrdiff_t (Index);
            P.all := Text_Cluster;
         end;
      else
         raise Constraint_Error;
      end if;
   end Set_Text_Cluster;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
     (Text_Cluster_List : Cairo_Text_Cluster_List)
      return Natural
   is
   begin
      return Text_Cluster_List.Length;
   end Get_Length;

   ----------------------
   -- Get_Text_Cluster --
   ----------------------

   function Get_Text_Cluster
     (Text_Cluster_List : Cairo_Text_Cluster_List;
      Index             : Natural)
      return Cairo_Text_Cluster
   is
   begin
      if Index < Text_Cluster_List.Length then
         declare
            P : Text_Cluster_Pointers.Pointer :=
               Text_Cluster_Pointers.Pointer (Text_Cluster_List.Ptr);
            use Text_Cluster_Pointers;
         begin
            P := P + Interfaces.C.ptrdiff_t (Index);
            return P.all;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Text_Cluster;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Text_Cluster_List : Cairo_Text_Cluster_List)
      return Cairo_Text_Cluster_Array
   is
   begin
      if Text_Cluster_List.Ptr /= null then
         return Text_Cluster_Pointers.Value
                  (Text_Cluster_Pointers.Pointer (Text_Cluster_List.Ptr),
                   Length =>
                     Interfaces.C.ptrdiff_t (Text_Cluster_List.Length));
      else
         raise Constraint_Error;
      end if;
   end To_Array;

   -------------
   -- Set_Ptr --
   -------------

   procedure Set_Ptr
     (Text_Cluster_List : in out Cairo_Text_Cluster_List;
      Ptr               : Text_Cluster_Ptr;
      Length            : Natural)
   is
   begin
      Text_Cluster_List.Ptr := Ptr;
      Text_Cluster_List.Length := Length;
   end Set_Ptr;

   ---------
   -- Ptr --
   ---------

   function Ptr
     (Text_Cluster_List : Cairo_Text_Cluster_List)
      return Text_Cluster_Ptr
   is
   begin
      return Text_Cluster_List.Ptr;
   end Ptr;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Text_Cluster_List) is
   begin
      Free (O);
   end Finalize;

end Cairo.Text_Cluster_List;
