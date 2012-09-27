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

package body Cairo.Pattern.Solid is

   function Allocate_Solid_Pattern return Cairo_Pattern_Ref;

   ---------------------
   -- New_RGB_Pattern --
   ---------------------

   function New_RGB_Pattern
     (Red, Green, Blue : double)
      return Cairo_Pattern_Handle
   is
   begin
      return To_Handle
               (cairo_pattern_create_rgb (Red, Green, Blue),
                Is_Referenced => True);
   end New_RGB_Pattern;

   ----------------------
   -- New_RGBA_Pattern --
   ----------------------

   function New_RGBA_Pattern
     (Red, Green, Blue, Alpha : double)
      return Cairo_Pattern_Handle
   is
   begin
      return To_Handle
               (cairo_pattern_create_rgba (Red, Green, Blue, Alpha),
                Is_Referenced => True);
   end New_RGBA_Pattern;

   --------------
   -- Get_RGBA --
   --------------

   procedure Get_RGBA
     (Pattern                 : Cairo_Solid_Pattern'Class;
      Red, Green, Blue, Alpha : out double;
      Status                  : out Cairo_Status)
   is
      -- TODO : avoid copies here
      R, G, B, A : aliased double;
   begin
      Status :=
         cairo_pattern_get_rgba
           (Ptr (Pattern),
            R'Access,
            G'Access,
            B'Access,
            A'Access);
      Red := R;
      Green := G;
      Blue := B;
      Alpha := A;
   end Get_RGBA;

   ----------------------------
   -- Allocate_Solid_Pattern --
   ----------------------------

   function Allocate_Solid_Pattern return Cairo_Pattern_Ref is
   begin
      return new Cairo_Solid_Pattern;
   end Allocate_Solid_Pattern;

begin
   Register (CAIRO_PATTERN_TYPE_SOLID, Allocate_Solid_Pattern'Access);
end Cairo.Pattern.Solid;
