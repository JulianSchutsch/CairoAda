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

--  This package defines Glyph_List, used to store a variable length array
--  of glyphs.

with Ada.Finalization;

package Cairo.Glyph_List is

   pragma Elaborate_Body;

   type Cairo_Glyph_List is limited private;
   --  Variable length array of Glyphs.
   --  Memory reclamation is automatic.
   --  Initial length of a Cairo_Glyph_List is 0.

   procedure Allocate
     (Glyph_List : in out Cairo_Glyph_List;
      Length : in Natural);
   --  Change the length of the allocated array.
   --  Previously allocated glyphs are freed.

   procedure Free
     (Glyph_List : in out Cairo_Glyph_List);
   --  Require an immediate memory reclamation.
   --  This is automatically called when Glyph_List scope ends.

   procedure Set_Glyph
     (Glyph_List : in out Cairo_Glyph_List;
      Index : Natural;
      Glyph : Cairo_Glyph);
   --  Set the Index-th glyph.
   --  Index must be in range 0 .. Get_Length - 1

   function Get_Length
     (Glyph_List : Cairo_Glyph_List)
      return Natural;
   --  Return the length of teh allocated glyph array

   function Get_Glyph
     (Glyph_List : Cairo_Glyph_List;
      Index : Natural)
      return Cairo_Glyph;
   --  Return the Index-th glyph.
   --  Index must be in range 0 .. Get_Length - 1

   function To_Array
     (Glyph_List : Cairo_Glyph_List)
      return Cairo_Glyph_Array;
   --  Return all stored glyphs as an array


   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   procedure Set_Ptr
     (Glyph_List : in out Cairo_Glyph_List;
      Ptr : Glyph_Ptr;
      Length : Natural);
   --  Set the C allocated array (cairo_glyph_t*) and its length.
   --  Do NOT Free previously allocated array, if any.

   function Ptr
     (Glyph_List : Cairo_Glyph_List)
      return Glyph_Ptr;
   --  Return the C allocated array (cairo_glyph_t*)

private

   type Cairo_Glyph_List is new Ada.Finalization.Limited_Controlled with record
      Ptr : Glyph_Ptr;
      --  Pointer to the C allocated glyph array (cairo_glyph_t*)
      Length : Natural := 0;
      --  Length of the allocated array
   end record;
   procedure Finalize (O : in out Cairo_Glyph_List);

end Cairo.Glyph_List;
