------------------------------------------------------------------------------
--                                                                          --
--                  RsvgAda - Ada95 binding for Rsvg                        --
--                                                                          --
-- Copyright (C) 2006-2008, Damien Carbonne                                 --
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
with Glib; use Glib;
with Glib.Object;
with Glib.Error; use Glib.Error;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Cairo.Context; use Cairo.Context;

package RSVG is

   -- Librsvg Ada binding using GtkAda conventions.
   -- Deallocation must be handled by user using Ref and Unref.

   type RSVG_Error is (RSVG_ERROR_FAILED);
   pragma Convention (C, RSVG_Error);

   type RSVG_Dimension_Data is record
      Width  : Gint; -- Width in pixels
      Height : Gint; -- Height in pixels
      EM     : Gdouble;
      EX     : Gdouble;
   end record;
   pragma Convention (C, RSVG_Dimension_Data);

   type RSVG_Position_Data is record
      X : Gint;
      Y : Gint;
   end record;
   pragma Convention (C, RSVG_Position_Data);


   function Error_Quark return GQuark;
   procedure Init;
   --  Initialize librsvg

   procedure Term;
   --  De-initialize librsvg

   procedure Set_Default_DPI (DPI : Gdouble);
   --  Sets the DPI for the all future outgoing pixbufs.
   --  Common values are 75, 90, and 300 DPI.
   --  Passing a number <= 0 to dpi will reset the DPI to whatever the default
   --  value happens to be.
   --  dpi : Dots Per Inch (aka Pixels Per Inch)
   --  since 2.8

   procedure Set_Default_DPI_X_Y (DPI_X : Gdouble; DPI_Y : Gdouble);
   --  Sets the DPI for the all future outgoing pixbufs.
   --  Common values are 75, 90, and 300 DPI.
   --  Passing a number <= 0 to dpi will reset the DPI to whatever the default
   --  value happens to be.
   --  dpi_x : Dots Per Inch (aka Pixels Per Inch)
   --  dpi_y : Dots Per Inch (aka Pixels Per Inch)
   --  since 2.8

   ------------
   -- Handle --
   ------------

   type RSVG_Handle_Record is new Glib.Object.GObject_Record with private;
   type RSVG_Handle is access all RSVG_Handle_Record'Class;

   procedure RSVG_New (Handle : out RSVG_Handle);

   procedure RSVG_New_From_Data
     (Handle : out RSVG_Handle;
      Data   : Ada.Streams.Stream_Element_Array;
      Error  : out GError);

   procedure RSVG_New_From_File
     (Handle   : out RSVG_Handle;
      Filename : String;
      Error    : out GError);

   procedure Set_DPI
     (Handle : access RSVG_Handle_Record'Class;
      DPI : Gdouble);

   procedure Set_DPI_X_Y
     (Handle : access RSVG_Handle_Record'Class;
      DPI_X : Gdouble;
      DPI_Y : Gdouble);

   procedure Write
     (Handle  : access RSVG_Handle_Record'Class;
      Buffer : Ada.Streams.Stream_Element_Array;
      Error  : out GError;
      Success : out Boolean);

   procedure Close
     (Handle  : access RSVG_Handle_Record'Class;
      Error   : out GError;
      Success : out Boolean);

   function Get_Base_URI
     (Handle : access RSVG_Handle_Record'Class)
      return String;

   procedure Set_Base_URI
     (Handle : access RSVG_Handle_Record'Class;
      Base_URI : String);

   function Get_Dimensions
     (Handle : access RSVG_Handle_Record'Class)
      return RSVG_Dimension_Data;

   procedure Get_Dimensions_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id : String;
      Dimension_Data : out RSVG_Dimension_Data;
      Success : out Boolean);

   procedure Get_Position_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id : String;
      Position_Data : out RSVG_Position_Data;
      Success : out Boolean);

   function Has_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id : String)
      return Boolean;

   function Get_Title
     (Handle : access RSVG_Handle_Record'Class)
      return String;

   function Get_Desc
     (Handle : access RSVG_Handle_Record'Class)
      return String;

   function Get_Metadata
     (Handle : access RSVG_Handle_Record'Class)
      return String;

   ---------------------
   -- Cairo rendering --
   ---------------------

   procedure Render
     (Handle  : access RSVG_Handle_Record'Class;
      Context : in out Cairo_Context'Class;
      Success : out Boolean);
   --  Draws a SVG to a Cairo surface

   procedure Render_Sub
     (Handle  : access RSVG_Handle_Record'Class;
      Context : in out Cairo_Context'Class;
      Id      : String;
      Success : out Boolean);
   --  Draws a subset of a SVG to a Cairo surface
   --  Id : An element's id within the SVG, or "" to render the whole SVG.
   --  For example, if you have a layer called "layer1" that you wish
   --  to render, pass "#layer1" as the id.

   ------------
   -- Pixbuf --
   ------------

   function Get_Pixbuf
     (Handle : access RSVG_Handle_Record'Class)
      return Gdk_Pixbuf;

   function Get_Pixbuf_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id     : String)
      return Gdk_Pixbuf;

private
   pragma Import (C, Error_Quark, "rsvg_error_quark");
   pragma Import (C, Init, "rsvg_init");
   pragma Import (C, Term, "rsvg_term");
   pragma Import (C, Set_Default_DPI, "rsvg_set_default_dpi");
   pragma Import (C, Set_Default_DPI_X_Y, "rsvg_set_default_dpi_x_y");
   type RSVG_Handle_Record is new Glib.Object.GObject_Record with null record;

   -- Those functions are deprecated and not bound
   -- rsvg_handle_free
   -- rsvg_handle_set_size_callback
   -- rsvg_pixbuf_from_file
   -- rsvg_pixbuf_from_file_at_zoom
   -- rsvg_pixbuf_from_file_at_size
   -- rsvg_pixbuf_from_file_at_max_size
   -- rsvg_pixbuf_from_file_at_zoom_with_max

end RSVG;
