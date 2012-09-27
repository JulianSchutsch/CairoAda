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

--  This package defines Rectangle_List, which can store a dynamically
--  allocated array of rectangles.
--  No means is provided to directly create a Rectangle_List.
--  One needs to retrieve a Rectangle_List from Cairo itself.
--  See Copy_Clip_Rectangle_List.
--  One can however query an existing Rectangle_List.

with Ada.Finalization;

package Cairo.Rectangle_List is

   pragma Elaborate_Body;

   type Cairo_Rectangle_List is limited private;
   --  A dynamically allocated array of rectangles.
   --  Memory allocation/deallocation is automatically handled.

   function Get_Status
     (Rectangle_List : Cairo_Rectangle_List)
      return Cairo_Status;
   --  Status of the Rectangle_ListO

   function Get_Length
     (Rectangle_List : Cairo_Rectangle_List)
      return Natural;
   --  Return the number of stored rectangles

   function Get_Rectangle
     (Rectangle_List : Cairo_Rectangle_List;
      Index : Natural)
      return Cairo_Rectangle;
   --  Return the Index-th rectangle.
   --  Index must be in range 0 .. Get_Length - 1

   function To_Array
     (Rectangle_List : Cairo_Rectangle_List)
      return Cairo_Rectangle_Array;
   --  Return all rectangles as an array

   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   procedure Set_Ptr
     (Rectangle_List : in out Cairo_Rectangle_List;
      Ptr : Rectangle_List_Ptr);
   --  Set the pointer to the C allocated structure (cairo_rectangle_list_t*)
   --  Current Ptr MUST be null before setting it.

   function Ptr
     (Rectangle_List : Cairo_Rectangle_List)
      return Rectangle_List_Ptr;
   --  Return the C allocated structure (cairo_rectangle_list_t*)

private

   type Cairo_Rectangle_List is new Ada.Finalization.Limited_Controlled with record
      Ptr : Rectangle_List_Ptr;
      --  Pointer to the C allocated structure (cairo_rectangle_list_t*)
   end record;
   procedure Finalize (O : in out Cairo_Rectangle_List);

end Cairo.Rectangle_List;
