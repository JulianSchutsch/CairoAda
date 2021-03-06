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

with Cairo.Support.PDF; use Cairo.Support.PDF; use Cairo.Support;
with Cairo.Surface; -- for pragma !!!
pragma Elaborate_All (Cairo.Surface);

package body Cairo.Surface.PDF is

   function Allocate_PDF_Surface return Cairo_Surface_Ref;

   ---------------------
   -- New_PDF_Surface --
   ---------------------

   function New_PDF_Surface
     (Filename         : String;
      Width_In_Points  : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle
   is
      C_Filename : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      return To_Handle
               (cairo_pdf_surface_create
                   (To_Chars_Ptr (C_Filename'Address),
                    Width_In_Points,
                    Height_In_Points),
                Is_Referenced => True);
   end New_PDF_Surface;

   --------------------------------
   -- New_PDF_Surface_For_Stream --
   --------------------------------

   function New_PDF_Surface_For_Stream
     (Stream           : access Ada.Streams.Root_Stream_Type'Class;
      Width_In_Points  : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
               (cairo_pdf_surface_create_for_stream
                   (Stream_Write'Access,
                    To_Closure (Stream_Access (Stream)),
                    Width_In_Points,
                    Height_In_Points),
                Is_Referenced => True);
   end New_PDF_Surface_For_Stream;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Surface          : in out Cairo_PDF_Surface'Class;
      Width_In_Points  : double;
      Height_In_Points : double)
   is
   begin
      cairo_pdf_surface_set_size
        (Surface.Ptr,
         Width_In_Points,
         Height_In_Points);
   end Set_Size;

   --------------------------
   -- Allocate_PDF_Surface --
   --------------------------

   function Allocate_PDF_Surface return Cairo_Surface_Ref is
   begin
      return new Cairo_PDF_Surface;
   end Allocate_PDF_Surface;

begin
   Register (CAIRO_SURFACE_TYPE_PDF, Allocate_PDF_Surface'Access);
end Cairo.Surface.PDF;
