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
with Cairo.Pattern;
pragma Elaborate_All (Cairo.Pattern);

package body Cairo.Pattern.Surface is

   function Allocate_Surface_Pattern return Cairo_Pattern_Ref;

   -------------------------
   -- New_Surface_Pattern --
   -------------------------

   function New_Surface_Pattern
     (Surface : access Cairo_Surface'Class)
      return Cairo_Pattern_Handle
   is
   begin
      return To_Handle
               (cairo_pattern_create_for_surface (Ptr (Surface.all)),
                Is_Referenced => True);
   end New_Surface_Pattern;

   ----------------
   -- Set_Extend --
   ----------------

   procedure Set_Extend
     (Pattern : in out Cairo_Surface_Pattern'Class;
      Extend  : Cairo_Extend)
   is
   begin
      cairo_pattern_set_extend (Pattern.Ptr, Extend);
   end Set_Extend;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Pattern : in out Cairo_Surface_Pattern'Class;
      Filter  : Cairo_Filter)
   is
   begin
      cairo_pattern_set_filter (Pattern.Ptr, Filter);
   end Set_Filter;

   ----------------
   -- Get_Extend --
   ----------------

   function Get_Extend
     (Pattern : Cairo_Surface_Pattern'Class)
      return Cairo_Extend
   is
   begin
      return cairo_pattern_get_extend (Pattern.Ptr);
   end Get_Extend;

   ----------------
   -- Get_Filter --
   ----------------

   function Get_Filter
     (Pattern : Cairo_Surface_Pattern'Class)
      return Cairo_Filter
   is
   begin
      return cairo_pattern_get_filter (Pattern.Ptr);
   end Get_Filter;

   -----------------
   -- Get_Surface --
   -----------------

   procedure Get_Surface
     (Pattern : Cairo_Surface_Pattern'Class;
      Surface : out Cairo_Surface_Handle;
      Success : out Cairo_Status)
   is
      Tmp : aliased Surface_Ptr;
   begin
      Success := cairo_pattern_get_surface (Ptr (Pattern), Tmp'Access);
      Surface := To_Handle (Tmp, False);
   end Get_Surface;

   ------------------------------
   -- Allocate_Surface_Pattern --
   ------------------------------

   function Allocate_Surface_Pattern return Cairo_Pattern_Ref is
   begin
      return new Cairo_Surface_Pattern;
   end Allocate_Surface_Pattern;

begin
   Register (CAIRO_PATTERN_TYPE_SURFACE, Allocate_Surface_Pattern'Access);
end Cairo.Pattern.Surface;
