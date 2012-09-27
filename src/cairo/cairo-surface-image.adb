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
with Cairo.Surface;
pragma Elaborate_All (Cairo.Surface);

package body Cairo.Surface.Image is

   function Allocate_Image_Surface return Cairo_Surface_Ref;

   ----------------------
   -- Stride_For_Width --
   ----------------------

   function Stride_For_Width
     (Format : Cairo_Format;
      Width  : int)
      return int
   is
   begin
      return cairo_format_stride_for_width (Format, Width);
   end Stride_For_Width;

   -----------------------
   -- New_Image_Surface --
   -----------------------

   function New_Image_Surface
     (Format        : Cairo_Format;
      Width, Height : int)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
               (cairo_image_surface_create (Format, Width, Height),
                Is_Referenced => True);
   end New_Image_Surface;

   --------------------------------
   -- New_Image_Surface_For_Data --
   --------------------------------

   function New_Image_Surface_For_Data
     (Data          : System.Address;
      Format        : Cairo_Format;
      Width, Height : int;
      Stride        : int)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
               (cairo_image_surface_create_for_data
                   (Data,
                    Format,
                    Width,
                    Height,
                    Stride),
                Is_Referenced => True);
   end New_Image_Surface_For_Data;

   --------------------------------
   -- New_Image_Surface_From_PNG --
   --------------------------------

   function New_Image_Surface_From_PNG
     (Filename : String)
      return Cairo_Surface_Handle
   is
      C_Filename : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      return To_Handle
               (cairo_image_surface_create_from_png
                   (To_Chars_Ptr (C_Filename'Address)),
                Is_Referenced => True);
   end New_Image_Surface_From_PNG;

   ---------------------------------------
   -- New_Image_Surface_From_PNG_Stream --
   ---------------------------------------

   function New_Image_Surface_From_PNG_Stream
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
               (cairo_image_surface_create_from_png_stream
                   (Stream_Read'Access,
                    To_Closure (Stream_Access (Stream))),
                Is_Referenced => True);
   end New_Image_Surface_From_PNG_Stream;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Surface : Cairo_Image_Surface'Class)
      return System.Address
   is
   begin
      return cairo_image_surface_get_data (Surface.Ptr);
   end Get_Data;

   ----------------
   -- Get_Format --
   ----------------

   function Get_Format
     (Surface : Cairo_Image_Surface'Class)
      return Cairo_Format
   is
   begin
      return cairo_image_surface_get_format (Surface.Ptr);
   end Get_Format;

   ---------------
   -- Get_Width --
   ---------------

   function Get_Width (Surface : Cairo_Image_Surface'Class) return int is
   begin
      return cairo_image_surface_get_width (Surface.Ptr);
   end Get_Width;

   ----------------
   -- Get_Height --
   ----------------

   function Get_Height (Surface : Cairo_Image_Surface'Class) return int is
   begin
      return cairo_image_surface_get_height (Surface.Ptr);
   end Get_Height;

   ----------------
   -- Get_Stride --
   ----------------

   function Get_Stride (Surface : Cairo_Image_Surface'Class) return int is
   begin
      return cairo_image_surface_get_stride (Surface.Ptr);
   end Get_Stride;

   ----------------------------
   -- Allocate_Image_Surface --
   ----------------------------

   function Allocate_Image_Surface return Cairo_Surface_Ref is
   begin
      return new Cairo_Image_Surface;
   end Allocate_Image_Surface;

begin
   Register (CAIRO_SURFACE_TYPE_IMAGE, Allocate_Image_Surface'Access);
end Cairo.Surface.Image;
