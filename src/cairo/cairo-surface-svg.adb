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
with Cairo.Support.SVG; use Cairo.Support.SVG;
with Interfaces.C.Strings;
with Cairo.Surface;
pragma Elaborate_All (Cairo.Surface);

package body Cairo.Surface.SVG is

   function Allocate_SVG_Surface return Cairo_Surface_Ref;

   ---------------------
   -- New_SVG_Surface --
   ---------------------

   function New_SVG_Surface
     (Filename         : String;
      Width_In_Points  : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle
   is
      C_Filename : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      return To_Handle
               (cairo_svg_surface_create
                   (To_Chars_Ptr (C_Filename'Address),
                    Width_In_Points,
                    Height_In_Points),
                Is_Referenced => True);
   end New_SVG_Surface;

   --------------------------------
   -- New_SVG_Surface_For_Stream --
   --------------------------------

   function New_SVG_Surface_For_Stream
     (Stream           : access Ada.Streams.Root_Stream_Type'Class;
      Width_In_Points  : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
               (cairo_svg_surface_create_for_stream
                   (Stream_Write'Access,
                    To_Closure (Stream_Access (Stream)),
                    Width_In_Points,
                    Height_In_Points),
                Is_Referenced => True);
   end New_SVG_Surface_For_Stream;

   -------------------------
   -- Restrict_To_Version --
   -------------------------

   procedure Restrict_To_Version
     (Surface : in out Cairo_SVG_Surface'Class;
      Version : SVG_Version)
   is
   begin
      cairo_svg_surface_restrict_to_version (Surface.Ptr, Version);
   end Restrict_To_Version;

   ----------------------
   -- Get_SVG_Versions --
   ----------------------

   function Get_SVG_Versions return SVG_Version_Array is
      Version_Ptr  : SVG_Version_Ptr;
      Num_Versions : int;
   begin
      cairo_svg_get_versions (Version_Ptr, Num_Versions);
      declare
         Versions : SVG_Version_Array (0 .. Integer (Num_Versions) - 1);
         for Versions'Address use To_Address (Version_Ptr);
      begin
         return Versions;
      end;
   end Get_SVG_Versions;

   ---------------
   -- To_String --
   ---------------

   function To_String (Version : SVG_Version) return String is
      C_Result : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      C_Result := cairo_svg_version_to_string (Version);
      if C_Result /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (C_Result);
      else
         return "";
      end if;
   end To_String;

   --------------------------
   -- Allocate_SVG_Surface --
   --------------------------

   function Allocate_SVG_Surface return Cairo_Surface_Ref is
   begin
      return new Cairo_SVG_Surface;
   end Allocate_SVG_Surface;

begin
   Register (CAIRO_SURFACE_TYPE_SVG, Allocate_SVG_Surface'Access);
end Cairo.Surface.SVG;
