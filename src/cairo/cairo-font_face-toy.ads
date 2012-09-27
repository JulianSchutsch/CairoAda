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

package Cairo.Font_Face.Toy is

   type Cairo_Toy_Font_Face (<>) is new Cairo_Font_Face with private;
   type Cairo_Toy_Font_Face_Ref is access all Cairo_Toy_Font_Face'Class;


   ------------------
   -- Construction --
   ------------------

   function New_Toy_Font_Face
     (Family : String;
      Slant : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight)
      return Cairo_Font_Face_Handle;
   --  <parameter name="family">a font family name, encoded in UTF-8</parameter>
   --  <parameter name="slant">the slant for the font</parameter>
   --  <parameter name="weight">the weight for the font</parameter>
   --
   --  Creates a font face from a triplet of family, slant, and weight.
   --  These font faces are used in implementation of the the Cairo_Context
   --  "toy" font API.
   --
   --  If family is the zero-length string "", the platform-specific default
   --  family is assumed.  The default family then can be queried using
   --  Get_Family.
   --
   --  The Select_Font_Face function uses this to create font faces.
   --  See that function for limitations of toy font faces.
   --
   --  Since: 1.8

   -------------
   -- Getters --
   -------------

   function Get_Family
     (Font_Face : Cairo_Toy_Font_Face'Class)
      return String;
   --  <parameter name="font_face">A toy font face</parameter>
   --
   --  Gets the familly name of a toy font.
   --
   --  Return value: The family name.
   --
   --  Since: 1.8

   function Get_Slant
     (Font_Face : Cairo_Toy_Font_Face'Class)
      return Cairo_Font_Slant;
   --  <parameter name="font_face">A toy font face</parameter>
   --
   --  Gets the slant a toy font.
   --
   --  Return value: The slant value
   --
   --  Since: 1.8

   function Get_Weight
     (Font_Face : Cairo_Toy_Font_Face'Class)
      return Cairo_Font_Weight;
   --  <parameter name="font_face">A toy font face</parameter>
   --
   --  Gets the weight a toy font.
   --
   --  Return value: The weight value
   --
   --  Since: 1.8

private
   type Cairo_Toy_Font_Face is new Cairo_Font_Face with null record;
end Cairo.Font_Face.Toy;
