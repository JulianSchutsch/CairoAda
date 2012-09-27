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

package Cairo.Support.PDF is
   function cairo_pdf_surface_create (Filename : Interfaces.C.Strings.chars_ptr; Width_In_Points : double; Height_In_Points : double) return Surface_Ptr;
   function cairo_pdf_surface_create_for_stream (Write_Func : Cairo_Write_Func; Closure : Cairo_Closure; Width_In_Points : double; Height_In_Points : double) return Surface_Ptr;
   procedure cairo_pdf_surface_set_size (Surface : Surface_Ptr; Width_In_Points : double; Height_In_Points : double);

   pragma Import (C, cairo_pdf_surface_create, "cairo_pdf_surface_create");
   pragma Import (C, cairo_pdf_surface_create_for_stream, "cairo_pdf_surface_create_for_stream");
   pragma Import (C, cairo_pdf_surface_set_size, "cairo_pdf_surface_set_size");
end Cairo.Support.PDF;
