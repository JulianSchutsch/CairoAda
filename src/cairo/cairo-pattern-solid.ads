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

package Cairo.Pattern.Solid is

   pragma Elaborate_Body;

   type Cairo_Solid_Pattern (<>) is new Cairo_Pattern with private;
   type Cairo_Solid_Pattern_Ref is access all Cairo_Solid_Pattern'Class;

   ------------------
   -- Construction --
   ------------------

   function New_RGB_Pattern
     (Red, Green, Blue : double)
      return Cairo_Pattern_Handle;
   --  <parameter name="red">red component of the color</parameter>
   --  <parameter name="green">green component of the color</parameter>
   --  <parameter name="blue">blue component of the color</parameter>
   --
   --  Creates a new Cairo_Pattern corresponding to an opaque color.  The
   --  color components are floating point numbers in the range 0 to 1.
   --  If the values passed in are outside that range, they will be
   --  clamped.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Pattern_Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Pattern_Status.

   function New_RGBA_Pattern
     (Red, Green, Blue, Alpha : double)
      return Cairo_Pattern_Handle;
   --  <parameter name="red">red component of the color</parameter>
   --  <parameter name="green">green component of the color</parameter>
   --  <parameter name="blue">blue component of the color</parameter>
   --  <parameter name="alpha">alpha component of the color</parameter>
   --
   --  Creates a new Cairo_Pattern corresponding to a translucent color.
   --  The color components are floating point numbers in the range 0 to
   --  1.  If the values passed in are outside that range, they will be
   --  clamped.
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

   procedure Get_RGBA
     (Pattern : Cairo_Solid_Pattern'Class;
      Red, Green, Blue, Alpha : out double;
      Status : out Cairo_Status);

private

   type Cairo_Solid_Pattern is new Cairo_Pattern with null record;

end Cairo.Pattern.Solid;
