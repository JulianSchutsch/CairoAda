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

package body Cairo.Pattern.Gradient.Radial is

   function Allocate_Radial_Gradient return Cairo_Pattern_Ref;

   -------------------------
   -- New_Radial_Gradient --
   -------------------------

   function New_Radial_Gradient
     (Center0 : Cairo_Tuple;
      Radius0 : double;
      Center1 : Cairo_Tuple;
      Radius1 : double)
      return Cairo_Pattern_Handle
   is
   begin
      return To_Handle
               (cairo_pattern_create_radial
                   (Center0.X,
                    Center0.Y,
                    Radius0,
                    Center1.X,
                    Center1.Y,
                    Radius1),
                Is_Referenced => True);
   end New_Radial_Gradient;

   -----------------
   -- Get_Circles --
   -----------------

   procedure Get_Circles
     (Pattern : Cairo_Radial_Gradient'Class;
      Center0 : out Cairo_Tuple;
      Radius0 : out double;
      Center1 : out Cairo_Tuple;
      Radius1 : out double;
      Status  : out Cairo_Status)
   is
      X0, Y0, R0 : aliased double;
      X1, Y1, R1 : aliased double;
   begin
      Status :=
         cairo_pattern_get_radial_circles
           (Ptr (Pattern),
            X0'Access,
            Y0'Access,
            R0'Access,
            X1'Access,
            Y1'Access,
            R1'Access);
      Center0.X := X0;
      Center0.Y := Y0;
      Radius0 := R0;
      Center1.X := X1;
      Center1.Y := Y1;
      Radius1 := R1;
   end Get_Circles;

   ------------------------------
   -- Allocate_Radial_Gradient --
   ------------------------------

   function Allocate_Radial_Gradient return Cairo_Pattern_Ref is
   begin
      return new Cairo_Radial_Gradient;
   end Allocate_Radial_Gradient;

begin
   Register (CAIRO_PATTERN_TYPE_RADIAL, Allocate_Radial_Gradient'Access);
end Cairo.Pattern.Gradient.Radial;
