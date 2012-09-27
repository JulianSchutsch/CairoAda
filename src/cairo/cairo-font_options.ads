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

with Ada.Finalization;

package Cairo.Font_Options is

   pragma Elaborate_Body;

   ------------------
   -- Font_Options --
   ------------------

   type Cairo_Font_Options is tagged private;
   --  An opaque structure holding all options that are used when
   --  rendering fonts.
   --
   --  New features may be added to a Cairo_Font_Options in the
   --  future.
   --  Note: Font options is not subclassed in Cairo.

   -------------
   -- Setters --
   -------------

   procedure Set_Antialias
     (Font_Options : in out Cairo_Font_Options'Class;
      Antialias : Cairo_Antialias);
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --  <parameter name="antialias">the new antialiasing mode</parameter>
   --
   --  Sets the antialiasing mode for the font options object. This
   --  specifies the type of antialiasing to do when rendering text.

   procedure Set_Subpixel_Order
     (Font_Options : in out Cairo_Font_Options'Class;
      Subpixel_Order : Cairo_Subpixel_Order);
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --  <parameter name="subpixel_order">the new subpixel order</parameter>
   --
   --  Sets the subpixel order for the font options object. The subpixel
   --  order specifies the order of color elements within each pixel on
   --  the display device when rendering with an antialiasing mode of
   --  CAIRO_ANTIALIAS_SUBPIXEL. See the documentation for
   --  Cairo_Subpixel_Order for full details.

   procedure Set_Hint_Style
     (Font_Options : in out Cairo_Font_Options'Class;
      Hint_Style : Cairo_Hint_Style);
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --  <parameter name="hint_style">the new hint style</parameter>
   --
   --  Sets the hint style for font outlines for the font options object.
   --  This controls whether to fit font outlines to the pixel grid,
   --  and if so, whether to optimize for fidelity or contrast.
   --  See the documentation for Cairo_Hint_Style for full details.

   procedure Set_Hint_Metrics
     (Font_Options : in out Cairo_Font_Options'Class;
      Hint_Metrics : Cairo_Hint_Metrics);
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --  <parameter name="hint_metrics">the new metrics hinting mode</parameter>
   --
   --  Sets the metrics hinting mode for the font options object. This
   --  controls whether metrics are quantized to integer values in
   --  device units.
   --  See the documentation for Cairo_Hint_Metrics for full details.


   -------------
   -- Getters --
   -------------

   function Get_Status
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Status;
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --
   --  Checks whether an error has previously occurred for this
   --  font options object
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY

   function Hash
     (Font_Options : Cairo_Font_Options'Class)
      return unsigned_long;
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --
   --  Compute a hash for the font options object; this value will
   --  be useful when storing an object containing a Cairo_Font_Options
   --  in a hash table.
   --
   --  Return value: the hash value for the font options object.
   --    The return value can be cast to a 32-bit type if a
   --    32-bit hash value is needed.

   function Get_Antialias
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Antialias;
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --
   --  Gets the antialiasing mode for the font options object.
   --
   --  Return value: the antialiasing mode

   function Get_Subpixel_Order
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Subpixel_Order;
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --
   --  Gets the subpixel order for the font options object.
   --  See the documentation for Cairo_Subpixel_Order for full details.
   --
   --  Return value: the subpixel order for the font options object

   function Get_Hint_Style
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Hint_Style;
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --
   --  Gets the hint style for font outlines for the font options object.
   --  See the documentation for Cairo_Hint_Style for full details.
   --
   --  Return value: the hint style for the font options object

   function Get_Hint_Metrics
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Hint_Metrics;
   --  <parameter name="options">a Cairo_Font_Options</parameter>
   --
   --  Gets the metrics hinting mode for the font options object.
   --  See the documentation for Cairo_Hint_Metrics for full details.
   --
   --  Return value: the metrics hinting mode for the font options object



   function "=" (Left, Right : Cairo_Font_Options'Class) return Boolean;

   -- TODO : merge


   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function To_Font_Options
     (Ptr : Font_Options_Ptr)
      return Cairo_Font_Options;
   --  Create a Font_Options from a C allocated structure (cairo_font_options_t*).

   function Ptr
     (Font_Options : Cairo_Font_Options'Class)
      return Font_Options_Ptr;
   --  Return the pointer to the C allocated structure (cairo_font_options_t*).

private

   type Cairo_Font_Options is new Ada.Finalization.Controlled with record
      Ptr : Font_Options_Ptr;
   end record;
   procedure Initialize (O : in out Cairo_Font_Options);
   procedure Adjust (O : in out Cairo_Font_Options);
   procedure Finalize (O : in out Cairo_Font_Options);

end Cairo.Font_Options;
