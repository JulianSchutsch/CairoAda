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

with Interfaces.C.Strings;
with Cairo.Support; use Cairo.Support;
with Cairo.Font_Face; -- For pragma
pragma Elaborate_All (Cairo.Font_Face);

package body Cairo.Font_Face.Toy is

   function Allocate_Toy_Font_Face return Cairo_Font_Face_Ref;

   -----------------------
   -- New_Toy_Font_Face --
   -----------------------

   function New_Toy_Font_Face
     (Family : String;
      Slant  : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight)
      return Cairo_Font_Face_Handle
   is
--        C_Family : aliased constant Interfaces.C.char_array :=
--           Interfaces.C.To_C (Family, Append_Nul => True);
   begin
      return To_Handle
               (cairo_toy_font_face_create
                   (To_Chars_Ptr (Family'Address),
                    Slant,
                    Weight),
                Is_Referenced => True);
   end New_Toy_Font_Face;

   ----------------
   -- Get_Family --
   ----------------

   function Get_Family
     (Font_Face : Cairo_Toy_Font_Face'Class)
      return String
   is
      C_Result : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      C_Result := cairo_toy_font_face_get_family (Font_Face.Ptr);
      if C_Result /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (C_Result);
      else
         return "";
      end if;
   end Get_Family;

   ---------------
   -- Get_Slant --
   ---------------

   function Get_Slant
     (Font_Face : Cairo_Toy_Font_Face'Class)
      return Cairo_Font_Slant
   is
   begin
      return cairo_toy_font_face_get_slant (Font_Face.Ptr);
   end Get_Slant;

   ----------------
   -- Get_Weight --
   ----------------

   function Get_Weight
     (Font_Face : Cairo_Toy_Font_Face'Class)
      return Cairo_Font_Weight
   is
   begin
      return cairo_toy_font_face_get_weight (Font_Face.Ptr);
   end Get_Weight;

   ----------------------------
   -- Allocate_Toy_Font_Face --
   ----------------------------

   function Allocate_Toy_Font_Face return Cairo_Font_Face_Ref is
   begin
      return new Cairo_Toy_Font_Face;
   end Allocate_Toy_Font_Face;

begin
   Register (CAIRO_FONT_TYPE_TOY, Allocate_Toy_Font_Face'Access);
end Cairo.Font_Face.Toy;
