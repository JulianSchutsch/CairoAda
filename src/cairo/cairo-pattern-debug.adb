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

with System.Address_Image;
with Ada.Unchecked_Conversion;
with Cairo.Support;

package body Cairo.Pattern.Debug is

   function To_Address is new Ada.Unchecked_Conversion
     (Pattern_Ptr, System.Address);
   function To_Address is new Ada.Unchecked_Conversion
     (Cairo_Pattern_Ref, System.Address);

   ---------
   -- Img --
   ---------

   function Img (X : Pattern_Ptr) return String is
   begin
      if X = null then
         return System.Address_Image (To_Address (X));
      else
         return System.Address_Image (To_Address (X))
           & " Count:" & Cairo.Support.cairo_pattern_get_reference_count (X)'Img
           & " Type: " & Cairo.Support.cairo_pattern_get_type (X)'Img;
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (X : Cairo_Pattern_Ref) return String is
   begin
      if X = null then
         return "null";
      else
         return System.Address_Image (To_Address (X))
           & "[Ptr: " & Img (X.Ptr) & "]";
      end if;
   end Img;

   ---------
   -- Img --
   ---------

   function Img (X : Cairo_Pattern_Handle) return String is
   begin
      return System.Address_Image (X'Address)
        & "[Ref: " & Img (X.Ref) & "]";
   end Img;

end Cairo.Pattern.Debug;
