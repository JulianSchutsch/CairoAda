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
with Cairo.Surface.PS; use Cairo.Surface.PS;
with System;

package Cairo.Support.PS is

   type PS_Level_Ptr is access all PS_Level;
   pragma Convention (C, PS_Level_Ptr);
   function To_SVG_Version_Ptr is new Ada.Unchecked_Conversion (System.Address, PS_Level_Ptr);
   function To_Address is new Ada.Unchecked_Conversion (PS_Level_Ptr, System.Address);

   function cairo_ps_surface_create (Filename : Interfaces.C.Strings.chars_ptr; Width_In_Points : double; Height_In_Points : double) return Surface_Ptr;
   function cairo_ps_surface_create_for_stream (Write_Func : Cairo_Write_Func; Closure : Cairo_Closure; Width_In_Points : double; Height_In_Points : double) return Surface_Ptr;
   procedure cairo_ps_surface_restrict_to_level (Surface : Surface_Ptr; Level : PS_Level);
   procedure cairo_ps_get_levels (Levels : out PS_Level_Ptr; Num_Levels : out int);
   function cairo_ps_level_to_string (Level : PS_Level) return Interfaces.C.Strings.chars_ptr;
   procedure cairo_ps_surface_set_eps (Surface : Surface_Ptr; Eps : Cairo_Bool);
   function cairo_ps_surface_get_eps (Surface : Surface_Ptr) return Cairo_Bool;
   procedure cairo_ps_surface_set_size (Surface : Surface_Ptr; Width_In_Points : double; Height_In_Points : double);
   procedure cairo_ps_surface_dsc_comment (Surface : Surface_Ptr; Comment : Interfaces.C.Strings.chars_ptr);
   procedure cairo_ps_surface_dsc_begin_setup (Surface : Surface_Ptr);
   procedure cairo_ps_surface_dsc_begin_page_setup (Surface : Surface_Ptr);

   pragma Import (C, cairo_ps_surface_create, "cairo_ps_surface_create");
   pragma Import (C, cairo_ps_surface_create_for_stream, "cairo_ps_surface_create_for_stream");
   pragma Import (C, cairo_ps_surface_restrict_to_level, "cairo_ps_surface_restrict_to_level");
   pragma Import (C, cairo_ps_get_levels, "cairo_ps_get_levels");
   pragma Import (C, cairo_ps_level_to_string, "cairo_ps_level_to_string");
   pragma Import (C, cairo_ps_surface_set_eps, "cairo_ps_surface_set_eps");
   pragma Import (C, cairo_ps_surface_get_eps, "cairo_ps_surface_get_eps");
   pragma Import (C, cairo_ps_surface_set_size, "cairo_ps_surface_set_size");
   pragma Import (C, cairo_ps_surface_dsc_comment, "cairo_ps_surface_dsc_comment");
   pragma Import (C, cairo_ps_surface_dsc_begin_setup, "cairo_ps_surface_dsc_begin_setup");
   pragma Import (C, cairo_ps_surface_dsc_begin_page_setup, "cairo_ps_surface_dsc_begin_page_setup");

end Cairo.Support.PS;
