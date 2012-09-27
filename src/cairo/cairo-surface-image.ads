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

with System;
with Ada.Streams;

package Cairo.Surface.Image is

   pragma Elaborate_Body;

   type Cairo_Image_Surface (<>) is new Cairo_Surface with private;
   type Cairo_Image_Surface_Ref is access all Cairo_Image_Surface'Class;

   function Stride_For_Width (Format : Cairo_Format; Width : int) return int;


   ------------------
   -- Construction --
   ------------------

   function New_Image_Surface
     (Format : Cairo_Format;
      Width, Height : int)
      return Cairo_Surface_Handle;
   --  <parameter name="format">format of pixels in the surface to create</parameter>
   --  <parameter name="width">width of the surface, in pixels</parameter>
   --  <parameter name="height">height of the surface, in pixels</parameter>
   --
   --  Creates an image surface of the specified format and
   --  dimensions. Initially the surface contents are all
   --  0. (Specifically, within each pixel, each color or alpha channel
   --  belonging to format will be 0. The contents of bits within a pixel,
   --  but not belonging to the given format are undefined).
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Surface_Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if an error such as out of memory
   --  occurs. You can use Surface_Status to check for this.

   function New_Image_Surface_For_Data
     (Data : System.Address;
      Format : Cairo_Format;
      Width, Height : int;
      Stride : int)
      return Cairo_Surface_Handle;
   --  <parameter name="data">a pointer to a buffer supplied by the application in which
   --      to write contents. This pointer must be suitably aligned for any
   --      kind of variable, (for example, a pointer returned by malloc).</parameter>
   --  <parameter name="format">the format of pixels in the buffer</parameter>
   --  <parameter name="width">the width of the image to be stored in the buffer</parameter>
   --  <parameter name="height">the height of the image to be stored in the buffer</parameter>
   --  <parameter name="stride">the number of bytes between the start of rows in the
   --      buffer as allocated. This value should always be computed by
   --      Format_Stride_For_Width before allocating the data
   --      buffer.</parameter>
   --
   --  Creates an image surface for the provided pixel data. The output
   --  buffer must be kept around until the Cairo_Surface is destroyed
   --  or Surface_Finish is called on the surface.  The initial
   --  contents of Buffer will be used as the initial image contents; you
   --  must explicitly clear the buffer, using, for example,
   --  Rectangle and Fill if you want it cleared.
   --
   --  Note that the stride may be larger than
   --  width*bytes_per_pixel to provide proper alignment for each pixel
   --  and row. This alignment is required to allow high-performance rendering
   --  within cairo. The correct way to obtain a legal stride value is to
   --  call Format_Stride_For_Width with the desired format and
   --  maximum image width value, and the use the resulting stride value
   --  to allocate the data and to create the image surface. See
   --  Format_Stride_For_Width for example code.
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Surface_Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface in the case of an error such as out of
   --  memory or an invalid stride value. In case of invalid stride value
   --  the error status of the returned surface will be
   --  CAIRO_STATUS_INVALID_STRIDE.  You can use
   --  Surface_Status to check for this.
   --
   --  See Surface_Set_User_Data for a means of attaching a
   --  destroy-notification fallback to the surface if necessary.

   function New_Image_Surface_From_PNG
     (Filename : String)
      return Cairo_Surface_Handle;
   --  <parameter name="filename">name of PNG file to load</parameter>
   --
   --  Creates a new image surface and initializes the contents to the
   --  given PNG file.
   --
   --  Return value: a new Cairo_Surface initialized with the contents
   --  of the PNG file, or a "nil" surface if any error occurred. A nil
   --  surface can be checked for with cairo_surface_status(surface) which
   --  may return one of the following values:
   --
   --  CAIRO_STATUS_NO_MEMORY
   --  CAIRO_STATUS_FILE_NOT_FOUND
   --  CAIRO_STATUS_READ_ERROR

   function New_Image_Surface_From_PNG_Stream
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return Cairo_Surface_Handle;
   --  <parameter name="read_func">function called to read the data of the file</parameter>
   --  <parameter name="closure">data to pass to Read_Func.</parameter>
   --
   --  Creates a new image surface from PNG data read incrementally
   --  via the Read_Func function.
   --
   --  Return value: a new Cairo_Surface initialized with the contents
   --  of the PNG file or NULL if the data read is not a valid PNG image or
   --  memory could not be allocated for the operation.
   --
   --  PNG functions don't seem to be present on all targets supported by cairo

   function Get_Data
     (Surface : Cairo_Image_Surface'Class)
      return System.Address;
   --  <parameter name="surface">a Cairo_Image_Surface</parameter>
   --
   --  Get a pointer to the data of the image surface, for direct
   --  inspection or modification.
   --
   --  Return value: a pointer to the image data of this surface or NULL
   --  if Surface is not an image surface.
   --
   --  Since: 1.2

   function Get_Format
     (Surface : Cairo_Image_Surface'Class)
      return Cairo_Format;
   --  <parameter name="surface">a Cairo_Image_Surface</parameter>
   --
   --  Get the format of the surface.
   --
   --  Return value: the format of the surface
   --
   --  Since: 1.2

   function Get_Width
     (Surface : Cairo_Image_Surface'Class)
      return int;
   --  <parameter name="surface">a Cairo_Image_Surface</parameter>
   --
   --  Get the width of the image surface in pixels.
   --
   --  Return value: the width of the surface in pixels.

   function Get_Height
     (Surface : Cairo_Image_Surface'Class)
      return int;
   --  <parameter name="surface">a Cairo_Image_Surface</parameter>
   --
   --  Get the height of the image surface in pixels.
   --
   --  Return value: the height of the surface in pixels.

   function Get_Stride
     (Surface : Cairo_Image_Surface'Class)
      return int;
   --  <parameter name="surface">a Cairo_Image_Surface</parameter>
   --
   --  Get the stride of the image surface in bytes
   --
   --  Return value: the stride of the image surface in bytes (or 0 if
   --  Surface is not an image surface). The stride is the distance in
   --  bytes from the beginning of one row of the image data to the
   --  beginning of the next row.
   --
   --  Since: 1.2

private

   type Cairo_Image_Surface is new Cairo_Surface with null record;

end Cairo.Surface.Image;
