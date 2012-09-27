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

package body Cairo.Pattern.Gradient.Linear is

   function Allocate_Linear_Gradient return Cairo_Pattern_Ref;

   -------------------------
   -- New_Linear_Gradient --
   -------------------------

   function New_Linear_Gradient
     (Point0, Point1 : Cairo_Tuple)
      return Cairo_Pattern_Handle
   is
   begin
      return To_Handle
               (cairo_pattern_create_linear
                   (Point0.X,
                    Point0.Y,
                    Point1.X,
                    Point1.Y),
                Is_Referenced => True);
   end New_Linear_Gradient;

   ----------------
   -- Get_Points --
   ----------------

   procedure Get_Points
     (Pattern        : Cairo_Linear_Gradient'Class;
      Point0, Point1 : out Cairo_Tuple;
      Status         : out Cairo_Status)
   is
      X0, Y0, X1, Y1 : aliased double;
   begin
      Status   :=
         cairo_pattern_get_linear_points
           (Ptr (Pattern),
            X0'Access,
            Y0'Access,
            X1'Access,
            Y1'Access);
      Point0.X := X0;
      Point0.Y := Y0;
      Point1.X := X1;
      Point1.Y := Y1;
   end Get_Points;

   ------------------------------
   -- Allocate_Linear_Gradient --
   ------------------------------

   function Allocate_Linear_Gradient return Cairo_Pattern_Ref is
   begin
      return new Cairo_Linear_Gradient;
   end Allocate_Linear_Gradient;

begin
   Register (CAIRO_PATTERN_TYPE_LINEAR, Allocate_Linear_Gradient'Access);
end Cairo.Pattern.Gradient.Linear;
