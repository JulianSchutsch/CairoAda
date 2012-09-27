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

package body Cairo.Pattern.Gradient is

   ------------------------
   -- Add_Color_Stop_RGB --
   ------------------------

   procedure Add_Color_Stop_RGB
     (Pattern          : in out Cairo_Gradient'Class;
      Offset           : double;
      Red, Green, Blue : double)
   is
   begin
      cairo_pattern_add_color_stop_rgb
        (Pattern.Ptr,
         Offset,
         Red,
         Green,
         Blue);
   end Add_Color_Stop_RGB;

   -------------------------
   -- Add_Color_Stop_RGBA --
   -------------------------

   procedure Add_Color_Stop_RGBA
     (Pattern                 : in out Cairo_Gradient'Class;
      Offset                  : double;
      Red, Green, Blue, Alpha : double)
   is
   begin
      cairo_pattern_add_color_stop_rgba
        (Pattern.Ptr,
         Offset,
         Red,
         Green,
         Blue,
         Alpha);
   end Add_Color_Stop_RGBA;

   -------------------------
   -- Get_Color_Stop_RGBA --
   -------------------------

   procedure Get_Color_Stop_RGBA
     (Pattern                 : Cairo_Gradient'Class;
      Index                   : int;
      Offset                  : out double;
      Red, Green, Blue, Alpha : out double;
      Status                  : out Cairo_Status)
   is
      O, R, G, B, A : aliased double;
   begin
      Status :=
         cairo_pattern_get_color_stop_rgba
           (Ptr (Pattern),
            Index,
            O'Access,
            R'Access,
            G'Access,
            B'Access,
            A'Access);
      Offset := O;
      Red    := R;
      Green  := G;
      Blue   := B;
      Alpha  := A;
   end Get_Color_Stop_RGBA;

   --------------------------
   -- Get_Color_Stop_Count --
   --------------------------

   procedure Get_Color_Stop_Count
     (Pattern : Cairo_Gradient'Class;
      Count   : out int;
      Status  : out Cairo_Status)
   is
      C : aliased int;
   begin
      Status :=
         cairo_pattern_get_color_stop_count (Ptr (Pattern), C'Access);
      Count  := C;
   end Get_Color_Stop_Count;

end Cairo.Pattern.Gradient;
