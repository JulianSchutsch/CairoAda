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

with Ada.Streams;

package Cairo.Surface.SVG is

   type Cairo_SVG_Surface (<>) is new Cairo_Surface with private;
   type Cairo_SVG_Surface_Ref is access all Cairo_SVG_Surface'Class;

   type SVG_Version is
     (SVG_VERSION_1_1,
      SVG_VERSION_1_2);
   pragma Convention (C, SVG_Version);
   for SVG_Version'Size use 32;

   type SVG_Version_Array is array (Natural range <>) of SVG_Version;
   pragma Convention (C, SVG_Version_Array);

   function New_SVG_Surface
     (Filename : String;
      Width_In_Points : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle;
   --  <parameter name="filename">a filename for the SVG output (must be writable)</parameter>
   --  <parameter name="width_in_points">width of the surface, in points (1 point == 1/72.0 inch)</parameter>
   --  <parameter name="height_in_points">height of the surface, in points (1 point == 1/72.0 inch)</parameter>
   --
   --  Creates a SVG surface of the specified size in points to be written
   --  to Filename.
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Surface_Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if an error such as out of memory
   --  occurs. You can use Surface_Status to check for this.
   --
   --  Since: 1.2

   function New_SVG_Surface_For_Stream
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Width_In_Points : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle;
   --  <parameter name="write_func">a Cairo_Write_Func to accept the output data</parameter>
   --  <parameter name="closure">the closure argument for Write_Func</parameter>
   --  <parameter name="width_in_points">width of the surface, in points (1 point == 1/72.0 inch)</parameter>
   --  <parameter name="height_in_points">height of the surface, in points (1 point == 1/72.0 inch)</parameter>
   --
   --  Creates a SVG surface of the specified size in points to be written
   --  incrementally to the stream represented by Write_Func and Closure.
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Surface_Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if an error such as out of memory
   --  occurs. You can use Surface_Status to check for this.
   --
   --  Since: 1.2


   procedure Restrict_To_Version (Surface : in out Cairo_SVG_Surface'Class; Version : SVG_Version);
   --  <parameter name="surface">a SVG Cairo_Surface</parameter>
   --  <parameter name="version">SVG version</parameter>
   --
   --  Restricts the generated SVG file to Version. See Svg_Get_Versions
   --  for a list of available version values that can be used here.
   --
   --  This function should only be called before any drawing operations
   --  have been performed on the given surface. The simplest way to do
   --  this is to call this function immediately after creating the
   --  surface.
   --
   --  Since: 1.2

   function Get_SVG_Versions return SVG_Version_Array;
   -- Is_Supported_Version return Boolean; ???

   function To_String (Version : SVG_Version) return String;
   --  <parameter name="version">a version id</parameter>
   --
   --  Get the string representation of the given Version id. This function
   --  will return NULL if Version isn't valid. See Svg_Get_Versions
   --  for a way to get the list of valid version ids.
   --
   --  Return value: the string associated to given version.
   --
   --  Since: 1.2

private

   type Cairo_SVG_Surface is new Cairo_Surface with null record;

end Cairo.Surface.SVG;
