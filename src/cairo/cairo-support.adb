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

package body Cairo.Support is

   type Stream_Ptr is access all Ada.Streams.Root_Stream_Type'Class;
   function To_Stream_Ptr is new Ada.Unchecked_Conversion (Cairo_Closure, Stream_Ptr);

   ------------------
   -- Stream_Write --
   ------------------

   function Stream_Write (Closure : Cairo_Closure; Data : Void_Ptr; Length : unsigned) return Cairo.Cairo_Status is
      use Ada.Streams;
      Stream : constant Stream_Ptr := To_Stream_Ptr (Closure);
      Item : Stream_Element_Array (0 .. Stream_Element_Offset (Length) - 1);
      for Item'Address use To_Address (Data);
   begin
      Write (Stream.all, Item);
      return CAIRO_STATUS_SUCCESS;
   exception
      when others =>
         return CAIRO_STATUS_WRITE_ERROR;
   end Stream_Write;

   -----------------
   -- Stream_Read --
   -----------------

   function Stream_Read (Closure : Cairo_Closure; Data : Void_Ptr; Length : unsigned) return Cairo.Cairo_Status is
      use Ada.Streams;
      Stream : constant Stream_Ptr := To_Stream_Ptr (Closure);
      Item : Stream_Element_Array (0 .. Stream_Element_Offset (Length) - 1);
      for Item'Address use To_Address (Data);
      Last : Stream_Element_Offset;
   begin
      Read (Stream.all, Item, Last);
      if Last = Item'Last then
         return CAIRO_STATUS_SUCCESS;
      else
         return CAIRO_STATUS_READ_ERROR;
      end if;
   exception
      when others =>
         return CAIRO_STATUS_READ_ERROR;
   end Stream_Read;

end Cairo.Support;
