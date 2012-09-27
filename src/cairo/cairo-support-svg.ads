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

with Cairo.Surface.SVG; use Cairo.Surface.SVG;
with System;

package Cairo.Support.SVG is

   type SVG_Version_Ptr is access all SVG_Version;
   pragma Convention (C, SVG_Version_Ptr);
   function To_SVG_Version_Ptr is new Ada.Unchecked_Conversion (System.Address, SVG_Version_Ptr);
   function To_Address is new Ada.Unchecked_Conversion (SVG_Version_Ptr, System.Address);

   function cairo_svg_surface_create (Filename : Interfaces.C.Strings.chars_ptr; Width_In_Points : double; Height_In_Points : double) return Surface_Ptr;
   function cairo_svg_surface_create_for_stream (Write_Func : Cairo_Write_Func; Closure : Cairo_Closure; Width_In_Points : double; Height_In_Points : double) return Surface_Ptr;
   procedure cairo_svg_surface_restrict_to_version (Surface : Surface_Ptr; Version : SVG_Version);
   procedure cairo_svg_get_versions (Versions : out SVG_Version_Ptr; Num_Versions : out int);
   function cairo_svg_version_to_string (Version : SVG_Version) return Interfaces.C.Strings.chars_ptr;

   pragma Import (C, cairo_svg_surface_create, "cairo_svg_surface_create");
   pragma Import (C, cairo_svg_surface_create_for_stream, "cairo_svg_surface_create_for_stream");
   pragma Import (C, cairo_svg_surface_restrict_to_version, "cairo_svg_surface_restrict_to_version");
   pragma Import (C, cairo_svg_get_versions, "cairo_svg_get_versions");
   pragma Import (C, cairo_svg_version_to_string, "cairo_svg_version_to_string");

end Cairo.Support.SVG;
