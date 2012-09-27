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

package Cairo.Pattern.Gradient is

   pragma Elaborate_Body;

   type Cairo_Gradient (<>) is new Cairo_Pattern with private;
   type Cairo_Gradient_Ref is access all Cairo_Gradient'Class;


   ------------------
   -- Modification --
   ------------------

   procedure Add_Color_Stop_RGB
     (Pattern : in out Cairo_Gradient'Class;
      Offset : double;
      Red, Green, Blue : double);
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="offset">an offset in the range [0.0 .. 1.0]</parameter>
   --  <parameter name="red">red component of color</parameter>
   --  <parameter name="green">green component of color</parameter>
   --  <parameter name="blue">blue component of color</parameter>
   --
   --  Adds an opaque color stop to a gradient pattern. The offset
   --  specifies the location along the gradient's control vector. For
   --  example, a linear gradient's control vector is from (x0,y0) to
   --  (x1,y1) while a radial gradient's control vector is from any point
   --  on the start circle to the corresponding point on the end circle.
   --
   --  The color is specified in the same way as in Set_Source_Rgb.
   --
   --  If two (or more) stops are specified with identical offset values,
   --  they will be sorted according to the order in which the stops are
   --  added, (stops added earlier will compare less than stops added
   --  later). This can be useful for reliably making sharp color
   --  transitions instead of the typical blend.
   --
   --
   --  Note: If the pattern is not a gradient pattern, (eg. a linear or
   --  radial pattern), then the pattern will be put into an error status
   --  with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH.

   procedure Add_Color_Stop_RGBA
     (Pattern : in out Cairo_Gradient'Class;
      Offset : double;
      Red, Green, Blue, Alpha : double);
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="offset">an offset in the range [0.0 .. 1.0]</parameter>
   --  <parameter name="red">red component of color</parameter>
   --  <parameter name="green">green component of color</parameter>
   --  <parameter name="blue">blue component of color</parameter>
   --  <parameter name="alpha">alpha component of color</parameter>
   --
   --  Adds a translucent color stop to a gradient pattern. The offset
   --  specifies the location along the gradient's control vector. For
   --  example, a linear gradient's control vector is from (x0,y0) to
   --  (x1,y1) while a radial gradient's control vector is from any point
   --  on the start circle to the corresponding point on the end circle.
   --
   --  The color is specified in the same way as in Set_Source_Rgba.
   --
   --  If two (or more) stops are specified with identical offset values,
   --  they will be sorted according to the order in which the stops are
   --  added, (stops added earlier will compare less than stops added
   --  later). This can be useful for reliably making sharp color
   --  transitions instead of the typical blend.
   --
   --  Note: If the pattern is not a gradient pattern, (eg. a linear or
   --  radial pattern), then the pattern will be put into an error status
   --  with a status of CAIRO_STATUS_PATTERN_TYPE_MISMATCH.


   -------------
   -- Getters --
   -------------

   procedure Get_Color_Stop_RGBA
     (Pattern : Cairo_Gradient'Class;
      Index : int;
      Offset : out double;
      Red, Green, Blue, Alpha : out double;
      Status : out Cairo_Status);

   procedure Get_Color_Stop_Count
     (Pattern : Cairo_Gradient'Class;
      Count : out int;
      Status : out Cairo_Status);

private

   type Cairo_Gradient is new Cairo_Pattern with null record;

end Cairo.Pattern.Gradient;
