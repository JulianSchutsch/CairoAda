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

package Cairo.Pattern.Gradient.Linear is

   pragma Elaborate_Body;

   type Cairo_Linear_Gradient (<>) is new Cairo_Gradient with private;
   type Cairo_Linear_Gradient_Ref is access all Cairo_Linear_Gradient'Class;

   ------------------
   -- Construction --
   ------------------

   function New_Linear_Gradient
     (Point0, Point1 : Cairo_Tuple)
      return Cairo_Pattern_Handle;
   --  <parameter name="x0">x coordinate of the start point</parameter>
   --  <parameter name="y0">y coordinate of the start point</parameter>
   --  <parameter name="x1">x coordinate of the end point</parameter>
   --  <parameter name="y1">y coordinate of the end point</parameter>
   --
   --  Create a new linear gradient Cairo_Pattern along the line defined
   --  by (x0, y0) and (x1, y1).  Before using the gradient pattern, a
   --  number of color stops should be defined using
   --  Pattern_Add_Color_Stop_Rgb or
   --  Pattern_Add_Color_Stop_Rgba.
   --
   --  Note: The coordinates here are in pattern space. For a new pattern,
   --  pattern space is identical to user space, but the relationship
   --  between the spaces can be changed with Pattern_Set_Matrix.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Pattern_Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Pattern_Status.

   -------------
   -- Getters --
   -------------

   procedure Get_Points
     (Pattern : Cairo_Linear_Gradient'Class;
      Point0, Point1 : out Cairo_Tuple;
      Status : out Cairo_Status);

private

   type Cairo_Linear_Gradient is new Cairo_Gradient with null record;

end Cairo.Pattern.Gradient.Linear;
