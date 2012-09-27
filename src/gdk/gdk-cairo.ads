------------------------------------------------------------------------------
--                                                                          --
--                  GdkCairoAda - Ada95 binding for Gdk.Cairo               --
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

with Glib; use Glib;
with Gdk.Color;
with Gdk.Drawable;
with Gdk.Pixbuf;
with Gdk.Rectangle;
with Gdk.Region;
with Cairo.Context;

package Gdk.Cairo is

   function Create
     (Drawable : Gdk.Drawable.Gdk_Drawable)
      return Standard.Cairo.Context.Cairo_Context_Handle;

   procedure Set_Source_Color
     (Context : in out Standard.Cairo.Context.Cairo_Context'Class;
      Color : Gdk.Color.Gdk_Color);

   procedure Set_Source_Pixbuf
     (Context : in out Standard.Cairo.Context.Cairo_Context'Class;
      Pixbuf : Gdk.Pixbuf.Gdk_Pixbuf;
      Pixbuf_X : Gdouble;
      Pixbuf_Y : Gdouble);

   procedure Rectangle
     (Context : in out Standard.Cairo.Context.Cairo_Context'Class;
      Rectangle : Gdk.Rectangle.Gdk_Rectangle);

   procedure Region
     (Context : in out Standard.Cairo.Context.Cairo_Context'Class;
      Region : Gdk.Region.Gdk_Region);

end Gdk.Cairo;
