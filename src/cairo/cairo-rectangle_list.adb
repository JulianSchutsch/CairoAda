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

with Ada.Unchecked_Conversion;
with Interfaces.C.Pointers;
with Cairo.Support;            use Cairo.Support;

package body Cairo.Rectangle_List is

   type C_Rectangle_List is record
      Status         : Cairo_Status;
      Rectangles     : Void_Ptr;
      Num_Rectangles : int;
   end record;
   pragma Convention (C, C_Rectangle_List);
   type C_Rectangle_List_Ptr is access all C_Rectangle_List;
   pragma Convention (C, C_Rectangle_List_Ptr);

   function To_C_Rectangle_List_Ptr is new Ada.Unchecked_Conversion (
      Rectangle_List_Ptr,
      C_Rectangle_List_Ptr);

   Default_Rectangle : constant Cairo_Rectangle := (others => 0.0);
   package Rectangle_Pointers is new Interfaces.C.Pointers (
      Natural,
      Cairo_Rectangle,
      Cairo_Rectangle_Array,
      Default_Rectangle);
   function To_Rectangle_Ptr is new Ada.Unchecked_Conversion (
      Void_Ptr,
      Rectangle_Pointers.Pointer);

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Rectangle_List : Cairo_Rectangle_List)
      return Cairo_Status
   is
   begin
      if Rectangle_List.Ptr /= null then
         return To_C_Rectangle_List_Ptr (Rectangle_List.Ptr).Status;
      else
         return CAIRO_STATUS_NO_MEMORY;
      end if;
   end Get_Status;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length
     (Rectangle_List : Cairo_Rectangle_List)
      return Natural
   is
   begin
      if Rectangle_List.Ptr /= null then
         return Natural (
           To_C_Rectangle_List_Ptr (Rectangle_List.Ptr).Num_Rectangles);
      else
         return 0;
      end if;
   end Get_Length;

   -------------------
   -- Get_Rectangle --
   -------------------

   function Get_Rectangle
     (Rectangle_List : Cairo_Rectangle_List;
      Index          : Natural)
      return Cairo_Rectangle
   is
   begin
      return To_Array (Rectangle_List) (Index);
   end Get_Rectangle;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Rectangle_List : Cairo_Rectangle_List)
      return Cairo_Rectangle_Array
   is
   begin
      if Rectangle_List.Ptr /= null then
         declare
            C_Ptr : constant C_Rectangle_List_Ptr :=
               To_C_Rectangle_List_Ptr (Rectangle_List.Ptr);
         begin
            return Rectangle_Pointers.Value
                     (To_Rectangle_Ptr (C_Ptr.Rectangles),
                      Length => Interfaces.C.ptrdiff_t (C_Ptr.Num_Rectangles));
         end;
      else
         declare
            Tmp : constant Cairo_Rectangle_Array (0 .. -1) :=
              (others => (others => 0.0));
         begin
            return Tmp;
         end;
      end if;
   end To_Array;

   -------------
   -- Set_Ptr --
   -------------

   procedure Set_Ptr
     (Rectangle_List : in out Cairo_Rectangle_List;
      Ptr            : Rectangle_List_Ptr)
   is
   begin
      pragma Assert (Rectangle_List.Ptr = null);
      Rectangle_List.Ptr := Ptr;
   end Set_Ptr;

   ---------
   -- Ptr --
   ---------

   function Ptr
     (Rectangle_List : Cairo_Rectangle_List)
      return Rectangle_List_Ptr
   is
   begin
      return Rectangle_List.Ptr;
   end Ptr;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Rectangle_List) is
   begin
      if O.Ptr /= null then
         cairo_rectangle_list_destroy (O.Ptr);
         O.Ptr := null;
      end if;
   end Finalize;

end Cairo.Rectangle_List;
