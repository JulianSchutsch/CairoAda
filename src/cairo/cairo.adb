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
with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body Cairo is

   ---------------
   -- To_String --
   ---------------

   function To_String (Status : Cairo_Status) return String is
      C_Result : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      C_Result := cairo_status_to_string (Status);
      if C_Result /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (C_Result);
      else
         return "";
      end if;
   end To_String;

   ----------
   -- Init --
   ----------

   procedure Init
     (Matrix : out Cairo_Matrix;
      XX, YX, XY, YY, X0, Y0 : double)
   is
   begin
      cairo_matrix_init (Matrix, XX, YX, XY, YY, X0, Y0);
   end Init;

   -------------------
   -- Init_Identity --
   -------------------

   procedure Init_Identity (Matrix : out Cairo_Matrix) is
   begin
      cairo_matrix_init_identity (Matrix);
   end Init_Identity;

   --------------------
   -- Init_Translate --
   --------------------

   procedure Init_Translate (Matrix : out Cairo_Matrix; T : Cairo_Tuple) is
   begin
      cairo_matrix_init_translate (Matrix, T.X, T.Y);
   end Init_Translate;

   ----------------
   -- Init_Scale --
   ----------------

   procedure Init_Scale (Matrix : out Cairo_Matrix; SX, SY : double) is
   begin
      cairo_matrix_init_scale (Matrix, SX, SY);
   end Init_Scale;

   -----------------
   -- Init_Rotate --
   -----------------

   procedure Init_Rotate (Matrix : out Cairo_Matrix; Radians : double)
   is
   begin
      cairo_matrix_init_rotate (Matrix, Radians);
   end Init_Rotate;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Matrix : in out Cairo_Matrix; T : Cairo_Tuple) is
   begin
      cairo_matrix_translate (Matrix, T.X, T.Y);
   end Translate;

   -----------
   -- Scale --
   -----------

   procedure Scale (Matrix : in out Cairo_Matrix; SX, SY : double) is
   begin
      cairo_matrix_scale (Matrix, SX, SY);
   end Scale;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Matrix : in out Cairo_Matrix; Radians : double)
   is
   begin
      cairo_matrix_rotate (Matrix, Radians);
   end Rotate;

   ------------
   -- Invert --
   ------------

   procedure Invert (Matrix : in out Cairo_Matrix; Status : out Cairo_Status)
   is
      package Conversions is new System.Address_To_Access_Conversions (Cairo_Matrix);
   begin
      Status := cairo_matrix_invert (Conversions.To_Pointer (Matrix'Address));
   end Invert;

   --------------
   -- Multiply --
   --------------

   procedure Multiply (Result : out Cairo_Matrix; A, B : Cairo_Matrix)
   is
   begin
      cairo_matrix_multiply (Result, A, B);
   end Multiply;

   ------------------------
   -- Transform_Distance --
   ------------------------

   procedure Transform_Distance (Matrix : Cairo_Matrix; Vector : in out Cairo_Tuple)
   is
   begin
      cairo_matrix_transform_distance (Matrix, Vector.X, Vector.Y);
   end Transform_Distance;

   ---------------------
   -- Transform_Point --
   ---------------------

   procedure Transform_Point (Matrix : Cairo_Matrix; Point : in out Cairo_Tuple)
   is
   begin
      cairo_matrix_transform_point (Matrix, Point.X, Point.Y);
   end Transform_Point;

   -------------
   -- Version --
   -------------

   function Version return int is
   begin
      return cairo_version;
   end Version;

   --------------------
   -- Version_String --
   --------------------

   function Version_String return String is
      C_Result : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      C_Result := cairo_version_string;
      if C_Result /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (C_Result);
      else
         return "";
      end if;
   end Version_String;

   -----------------------------
   -- Debug_Reset_Static_Data --
   -----------------------------

   procedure Debug_Reset_Static_Data is
   begin
      cairo_debug_reset_static_data;
   end Debug_Reset_Static_Data;

end Cairo;
