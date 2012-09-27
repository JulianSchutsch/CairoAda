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

package body Cairo.Font_Options is

   -------------------
   -- Set_Antialias --
   -------------------

   procedure Set_Antialias
     (Font_Options : in out Cairo_Font_Options'Class;
      Antialias    : Cairo_Antialias)
   is
   begin
      cairo_font_options_set_antialias (Font_Options.Ptr, Antialias);
   end Set_Antialias;

   ------------------------
   -- Set_Subpixel_Order --
   ------------------------

   procedure Set_Subpixel_Order
     (Font_Options   : in out Cairo_Font_Options'Class;
      Subpixel_Order : Cairo_Subpixel_Order)
   is
   begin
      cairo_font_options_set_subpixel_order
        (Font_Options.Ptr,
         Subpixel_Order);
   end Set_Subpixel_Order;

   --------------------
   -- Set_Hint_Style --
   --------------------

   procedure Set_Hint_Style
     (Font_Options : in out Cairo_Font_Options'Class;
      Hint_Style   : Cairo_Hint_Style)
   is
   begin
      cairo_font_options_set_hint_style (Font_Options.Ptr, Hint_Style);
   end Set_Hint_Style;

   ----------------------
   -- Set_Hint_Metrics --
   ----------------------

   procedure Set_Hint_Metrics
     (Font_Options : in out Cairo_Font_Options'Class;
      Hint_Metrics : Cairo_Hint_Metrics)
   is
   begin
      cairo_font_options_set_hint_metrics (Font_Options.Ptr, Hint_Metrics);
   end Set_Hint_Metrics;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Status
   is
   begin
      return cairo_font_options_status (Font_Options.Ptr);
   end Get_Status;

   ----------
   -- Hash --
   ----------

   function Hash
     (Font_Options : Cairo_Font_Options'Class)
      return unsigned_long
   is
   begin
      return cairo_font_options_hash (Font_Options.Ptr);
   end Hash;

   -------------------
   -- Get_Antialias --
   -------------------

   function Get_Antialias
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Antialias
   is
   begin
      return cairo_font_options_get_antialias (Font_Options.Ptr);
   end Get_Antialias;

   ------------------------
   -- Get_Subpixel_Order --
   ------------------------

   function Get_Subpixel_Order
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Subpixel_Order
   is
   begin
      return cairo_font_options_get_subpixel_order (Font_Options.Ptr);
   end Get_Subpixel_Order;

   --------------------
   -- Get_Hint_Style --
   --------------------

   function Get_Hint_Style
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Hint_Style
   is
   begin
      return cairo_font_options_get_hint_style (Font_Options.Ptr);
   end Get_Hint_Style;

   ----------------------
   -- Get_Hint_Metrics --
   ----------------------

   function Get_Hint_Metrics
     (Font_Options : Cairo_Font_Options'Class)
      return Cairo_Hint_Metrics
   is
   begin
      return cairo_font_options_get_hint_metrics (Font_Options.Ptr);
   end Get_Hint_Metrics;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Cairo_Font_Options'Class) return Boolean is
   begin
      return cairo_font_options_equal (Left.Ptr, Right.Ptr) /= 0;
   end "=";

   ---------------------
   -- To_Font_Options --
   ---------------------

   function To_Font_Options
     (Ptr  : Font_Options_Ptr)
      return Cairo_Font_Options
   is
   begin
      if Ptr = null then
         return Cairo_Font_Options'(Ada.Finalization.Controlled with Ptr => cairo_font_options_create);
      else
         return Cairo_Font_Options'(Ada.Finalization.Controlled with Ptr => Ptr);
      end if;
   end To_Font_Options;

   ---------
   -- Ptr --
   ---------

   function Ptr
     (Font_Options : Cairo_Font_Options'Class)
      return Font_Options_Ptr
   is
   begin
      return Font_Options.Ptr;
   end Ptr;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (O : in out Cairo_Font_Options) is
   begin
      O.Ptr := cairo_font_options_create;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Cairo_Font_Options) is
   begin
      O.Ptr := cairo_font_options_copy (O.Ptr);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Font_Options) is
   begin
      cairo_font_options_destroy (O.Ptr);
      O.Ptr := null;
   end Finalize;

end Cairo.Font_Options;
