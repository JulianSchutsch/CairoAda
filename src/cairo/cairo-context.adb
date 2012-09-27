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
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

--  with Ada.Text_IO; -- Debug
--  with Cairo.Context.Debug; use Cairo.Context.Debug;

package body Cairo.Context is

--     package Path_Import is
--        function Ptr (Path : Cairo_Path) return Path_Ptr;
--        pragma Import (Ada, Ptr, "cairo__path__ptr");
--        procedure Set_Ptr (Path : in out Cairo_Path; Ptr : Path_Ptr);
--        pragma Import (Ada, Set_Ptr, "cairo__path__set_ptr");
--     end Path_Import;
--     use Path_Import;
--
--     package Font_Face_Import is
--        function To_Handle (Ptr : Font_Face_Ptr; Is_Referenced : Boolean) return Cairo_Font_Face_Handle;
--        pragma Import (Ada, To_Handle, "cairo__font_face__to_handle");
--        function Ptr (Font_Face : Cairo_Font_Face'Class) return Font_Face_Ptr;
--        pragma Import (Ada, Ptr, "cairo__font_face__ptr");
--     end Font_Face_Import;
--     use Font_Face_Import;

   Ada_Context_Key : aliased Void;
   -- The key used to attach the Ada created context to the C context

   pragma Warnings (Off);
   --  Suppress possible aliasin problem warning
   function To_Ref is new Ada.Unchecked_Conversion
     (Cairo_User_Data, Cairo_Context_Ref);
   pragma Warnings (On);
   function To_User_Data is new Ada.Unchecked_Conversion
     (Cairo_Context_Ref, Cairo_User_Data);

   procedure Destroy_Context (Data : Cairo_User_Data);
   pragma Convention (C, Destroy_Context);

   ---------------------
   -- Destroy_Context --
   ---------------------

   procedure Destroy_Context (Data : Cairo_User_Data) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Cairo_Context'Class, Cairo_Context_Ref);
      Ref : Cairo_Context_Ref := To_Ref (Data);
   begin
--      Ada.Text_IO.Put_Line ("Cairo.Context.Destroy_Context Ref:" & Img (Ref));
      Free (Ref);
   end Destroy_Context;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (Target : access Cairo_Surface'Class)
      return Cairo_Context_Handle
   is
--      Handle : Cairo_Context_Handle;
   begin
      return To_Handle
        (Ptr => cairo_create (Ptr (Target.all)),
         Is_Referenced => True);
--        Handle.Ref := new Cairo_Context;
--        pragma Assert (Handle.Ref /= null);
--        Handle.Ref.Ptr := cairo_create (Ptr (Target.all));
--        pragma Assert (Handle.Ref.Ptr /= null);
--      return Handle;
   end New_Context;

   -------------------
   -- Set_User_Data --
   -------------------

   procedure Set_User_Data
     (Context : in out Cairo_Context'Class;
      Key : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy : Cairo_Destroy_Func;
      Status : out Cairo_Status)
   is
   begin
      Status := cairo_set_user_data (Context.Ptr, Key, User_Data, Destroy);
   end Set_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Context : Cairo_Context'Class;
      Key : Cairo_User_Data_Key)
      return Cairo_User_Data
   is
   begin
      return cairo_get_user_data (Context.Ptr, Key);
   end Get_User_Data;

   ----------
   -- Save --
   ----------

   procedure Save (Context : in out Cairo_Context'Class) is
   begin
      cairo_save (Context.Ptr);
   end Save;

   -------------
   -- Restore --
   -------------

   procedure Restore (Context : in out Cairo_Context'Class) is
   begin
      cairo_restore (Context.Ptr);
   end Restore;

   ----------------
   -- Push_Group --
   ----------------

   procedure Push_Group (Context : in out Cairo_Context'Class) is
   begin
      cairo_push_group (Context.Ptr);
   end Push_Group;

   -----------------------------
   -- Push_Group_With_Content --
   -----------------------------

   procedure Push_Group_With_Content
     (Context : in out Cairo_Context'Class;
      Content : Cairo_Content)
   is
   begin
      cairo_push_group_with_content (Context.Ptr, Content);
   end Push_Group_With_Content;

   ---------------
   -- Pop_Group --
   ---------------

   procedure Pop_Group
     (Context : in out Cairo_Context'Class;
      Pattern : out Cairo_Pattern_Handle)
   is
      Ptr : Cairo.Pattern_Ptr;
   begin
      Ptr := cairo_pop_group (Context.Ptr);
      Pattern := To_Handle (Ptr, Is_Referenced => True);
   end Pop_Group;

   -------------------------
   -- Pop_Group_To_Source --
   -------------------------

   procedure Pop_Group_To_Source (Context : in out Cairo_Context'Class) is
   begin
      cairo_pop_group_to_source (Context.Ptr);
   end Pop_Group_To_Source;

   ------------------
   -- Set_Operator --
   ------------------

   procedure Set_Operator
     (Context : in out Cairo_Context'Class;
      Operator : Cairo_Operator)
   is
   begin
      cairo_set_operator (Context.Ptr, Operator);
   end Set_Operator;

   ----------------
   -- Set_Source --
   ----------------

   procedure Set_Source
     (Context : in out Cairo_Context'Class;
      Source : access Cairo_Pattern'Class)
   is
   begin
      cairo_set_source (Context.Ptr, Ptr (Source.all));
   end Set_Source;

   --------------------
   -- Set_Source_RGB --
   --------------------

   procedure Set_Source_RGB
     (Context : in out Cairo_Context'Class;
      Red, Green, Blue : double)
   is
   begin
      cairo_set_source_rgb (Context.Ptr, Red, Green, Blue);
   end Set_Source_RGB;

   ---------------------
   -- Set_Source_RGBA --
   ---------------------

   procedure Set_Source_RGBA
     (Context : in out Cairo_Context'Class;
      Red, Green, Blue, Alpha : double)
   is
   begin
      cairo_set_source_rgba (Context.Ptr, Red, Green, Blue, Alpha);
   end Set_Source_RGBA;

   ------------------------
   -- Set_Source_Surface --
   ------------------------

   procedure Set_Source_Surface
     (Context : in out Cairo_Context'Class;
      Surface : access Cairo_Surface'Class;
      X, Y : double)
   is
   begin
      cairo_set_source_surface (Context.Ptr, Ptr (Surface.all), X, Y);
   end Set_Source_Surface;

   -------------------
   -- Set_Tolerance --
   -------------------

   procedure Set_Tolerance
     (Context : in out Cairo_Context'Class;
      Tolerance : double)
   is
   begin
      cairo_set_tolerance (Context.Ptr, Tolerance);
   end Set_Tolerance;

   -------------------
   -- Set_Antialias --
   -------------------

   procedure Set_Antialias
     (Context : in out Cairo_Context'Class;
      Antialias : Cairo_Antialias)
   is
   begin
      cairo_set_antialias (Context.Ptr, Antialias);
   end Set_Antialias;

   -------------------
   -- Set_Fill_Rule --
   -------------------

   procedure Set_Fill_Rule
     (Context : in out Cairo_Context'Class;
      Fill_Rule : Cairo_Fill_Rule)
   is
   begin
      cairo_set_fill_rule (Context.Ptr, Fill_Rule);
   end Set_Fill_Rule;

   --------------------
   -- Set_Line_Width --
   --------------------

   procedure Set_Line_Width
     (Context : in out Cairo_Context'Class;
      Width : double)
   is
   begin
      cairo_set_line_width (Context.Ptr, Width);
   end Set_Line_Width;

   ------------------
   -- Set_Line_Cap --
   ------------------

   procedure Set_Line_Cap
     (Context : in out Cairo_Context'Class;
      Line_Cap : Cairo_Line_Cap)
   is
   begin
      cairo_set_line_cap (Context.Ptr, Line_Cap);
   end Set_Line_Cap;

   -------------------
   -- Set_Line_Join --
   -------------------

   procedure Set_Line_Join
     (Context : in out Cairo_Context'Class;
      Line_Join : Cairo_Line_Join)
   is
   begin
      cairo_set_line_join (Context.Ptr, Line_Join);
   end Set_Line_Join;

   --------------
   -- Set_Dash --
   --------------

   procedure Set_Dash
     (Context : in out Cairo_Context'Class;
      Dashes : double_Array;
      Offset : double)
   is
   begin
      cairo_set_dash
        (Context.Ptr,
         To_Double_Ptr (Dashes'Address),
         Dashes'Length,
         Offset);
   end Set_Dash;

   ---------------------
   -- Set_Miter_Limit --
   ---------------------

   procedure Set_Miter_Limit
     (Context : in out Cairo_Context'Class;
      Limit : double)
   is
   begin
      cairo_set_miter_limit (Context.Ptr, Limit);
   end Set_Miter_Limit;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Context : in out Cairo_Context'Class; T : Cairo_Tuple)
   is
   begin
      cairo_translate (Context.Ptr, T.X, T.Y);
   end Translate;

   procedure Translate (Context : in out Cairo_Context'Class; TX, TY : double)
   is
   begin
      cairo_translate (Context.Ptr, TX, TY);
   end Translate;

   -----------
   -- Scale --
   -----------

   procedure Scale (Context : in out Cairo_Context'Class; SX, SY : double)
   is
   begin
      cairo_scale (Context.Ptr, SX, SY);
   end Scale;

   ------------
   -- Rotate --
   ------------

   procedure Rotate (Context : in out Cairo_Context'Class; Angle : double)
   is
   begin
      cairo_rotate (Context.Ptr, Angle);
   end Rotate;

   ---------------
   -- Transform --
   ---------------

   procedure Transform
     (Context : in out Cairo_Context'Class;
      Matrix : Cairo_Matrix)
   is
   begin
      cairo_transform (Context.Ptr, Matrix);
   end Transform;

   ----------------
   -- Set_Matrix --
   ----------------

   procedure Set_Matrix
     (Context : in out Cairo_Context'Class;
      Matrix : Cairo_Matrix)
   is
   begin
      cairo_set_matrix (Context.Ptr, Matrix);
   end Set_Matrix;

   -------------------------
   -- Set_Identity_Matrix --
   -------------------------

   procedure Set_Identity_Matrix (Context : in out Cairo_Context'Class) is
   begin
      cairo_identity_matrix (Context.Ptr);
   end Set_Identity_Matrix;

   --------------
   -- New_Path --
   --------------

   procedure New_Path (Context : in out Cairo_Context'Class) is
   begin
      cairo_new_path (Context.Ptr);
   end New_Path;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Context : in out Cairo_Context'Class;
      Point : Cairo_Tuple)
   is
   begin
      cairo_move_to (Context.Ptr, Point.X, Point.Y);
   end Move_To;

   procedure Move_To
     (Context : in out Cairo_Context'Class;
      X, Y : double)
   is
   begin
      cairo_move_to (Context.Ptr, X, Y);
   end Move_To;

   ------------------
   -- New_Sub_Path --
   ------------------

   procedure New_Sub_Path (Context : in out Cairo_Context'Class) is
   begin
      cairo_new_sub_path (Context.Ptr);
   end New_Sub_Path;

   -------------
   -- Line_To --
   -------------

   procedure Line_To
     (Context : in out Cairo_Context'Class;
      Point : Cairo_Tuple)
   is
   begin
      cairo_line_to (Context.Ptr, Point.X, Point.Y);
   end Line_To;

   procedure Line_To
     (Context : in out Cairo_Context'Class;
      X, Y : double)
   is
   begin
      cairo_line_to (Context.Ptr, X, Y);
   end Line_To;

   --------------
   -- Curve_To --
   --------------

   procedure Curve_To
     (Context : in out Cairo_Context'Class;
      Point1, Point2, Point3 : Cairo_Tuple)
   is
   begin
      cairo_curve_to
        (Context.Ptr,
         Point1.X, Point1.Y,
         Point2.X, Point2.Y,
         Point3.X, Point3.Y);
   end Curve_To;

   procedure Curve_To
     (Context : in out Cairo_Context'Class;
      X1, Y1, X2, Y2, X3, Y3 : double)
   is
   begin
      cairo_curve_to (Context.Ptr, X1, Y1, X2, Y2, X3, Y3);
   end Curve_To;

   ---------
   -- Arc --
   ---------

   procedure Arc
     (Context : in out Cairo_Context'Class;
      Center : Cairo_Tuple;
      Radius : double;
      Angle1, Angle2 : double)
   is
   begin
      cairo_arc (Context.Ptr, Center.X, Center.Y, Radius, Angle1, Angle2);
   end Arc;

   procedure Arc
     (Context : in out Cairo_Context'Class;
      Center_X, Center_Y : double;
      Radius : double;
      Angle1, Angle2 : double)
   is
   begin
      cairo_arc (Context.Ptr, Center_X, Center_Y, Radius, Angle1, Angle2);
   end Arc;


   ------------------
   -- Arc_Negative --
   ------------------

   procedure Arc_Negative
     (Context : in out Cairo_Context'Class;
      Center : Cairo_Tuple;
      Radius : double;
      Angle1, Angle2 : double)
   is
   begin
      cairo_arc_negative
        (Context.Ptr,
         Center.X, Center.Y,
         Radius,
         Angle1, Angle2);
   end Arc_Negative;

   procedure Arc_Negative
     (Context : in out Cairo_Context'Class;
      Center_X, Center_Y : double;
      Radius : double;
      Angle1, Angle2 : double)
   is
   begin
      cairo_arc_negative
        (Context.Ptr,
         Center_X, Center_Y,
         Radius,
         Angle1, Angle2);
   end Arc_Negative;

   -----------------
   -- Rel_Move_To --
   -----------------

   procedure Rel_Move_To
     (Context : in out Cairo_Context'Class;
      Offset : Cairo_Tuple)
   is
   begin
      cairo_rel_move_to (Context.Ptr, Offset.X, Offset.Y);
   end Rel_Move_To;

   procedure Rel_Move_To
     (Context : in out Cairo_Context'Class;
      Offset_X, Offset_Y : double)
   is
   begin
      cairo_rel_move_to (Context.Ptr, Offset_X, Offset_Y);
   end Rel_Move_To;

   -----------------
   -- Rel_Line_To --
   -----------------

   procedure Rel_Line_To
     (Context : in out Cairo_Context'Class;
      Offset : Cairo_Tuple)
   is
   begin
      cairo_rel_line_to (Context.Ptr, Offset.X, Offset.Y);
   end Rel_Line_To;

   procedure Rel_Line_To
     (Context : in out Cairo_Context'Class;
      Offset_X, Offset_Y : double)
   is
   begin
      cairo_rel_line_to (Context.Ptr, Offset_X, Offset_Y);
   end Rel_Line_To;

   ------------------
   -- Rel_Curve_To --
   ------------------

   procedure Rel_Curve_To
     (Context : in out Cairo_Context'Class;
      Offset1, Offset2, Offset3 : Cairo_Tuple)
   is
   begin
      cairo_rel_curve_to
        (Context.Ptr,
         Offset1.X, Offset1.Y,
         Offset2.X, Offset2.Y,
         Offset3.X, Offset3.Y);
   end Rel_Curve_To;

   procedure Rel_Curve_To
     (Context : in out Cairo_Context'Class;
      Offset1_X, Offset1_Y, Offset2_X, Offset2_Y, Offset3_X, Offset3_Y : double)
   is
   begin
      cairo_rel_curve_to
        (Context.Ptr,
         Offset1_X, Offset1_Y,
         Offset2_X, Offset2_Y,
         Offset3_X, Offset3_Y);
   end Rel_Curve_To;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle
     (Context : in out Cairo_Context'Class;
      Point : Cairo_Tuple;
      Width, Height : double)
   is
   begin
      Support.cairo_rectangle (Context.Ptr, Point.X, Point.Y, Width, Height);
   end Rectangle;

   procedure Rectangle
     (Context : in out Cairo_Context'Class;
      X, Y : double;
      Width, Height : double)
   is
   begin
      Support.cairo_rectangle (Context.Ptr, X, Y, Width, Height);
   end Rectangle;

   ----------------
   -- Close_Path --
   ----------------

   procedure Close_Path (Context : in out Cairo_Context'Class) is
   begin
      cairo_close_path (Context.Ptr);
   end Close_Path;

   -----------------
   -- Append_Path --
   -----------------

   procedure Append_Path
     (Context : in out Cairo_Context'Class;
      Path : Cairo_Path)
   is
   begin
      cairo_append_path (Context.Ptr, Ptr (Path));
   end Append_Path;

   -----------
   -- Paint --
   -----------

   procedure Paint (Context : in out Cairo_Context'Class) is
   begin
      cairo_paint (Context.Ptr);
   end Paint;

   ----------------------
   -- Paint_With_Alpha --
   ----------------------

   procedure Paint_With_Alpha
     (Context : in out Cairo_Context'Class;
      Alpha : double)
   is
   begin
      cairo_paint_with_alpha (Context.Ptr, Alpha);
   end Paint_With_Alpha;

   ----------
   -- Mask --
   ----------

   procedure Mask
     (Context : in out Cairo_Context'Class;
      Pattern : access Cairo_Pattern'Class)
   is
   begin
      cairo_mask (Context.Ptr, Ptr (Pattern.all));
   end Mask;

   ------------------
   -- Mask_Surface --
   ------------------

   procedure Mask_Surface
     (Context : in out Cairo_Context'Class;
      Surface : access Cairo_Surface'Class;
      Surface_Origin : Cairo_Tuple)
   is
   begin
      cairo_mask_surface
        (Context.Ptr,
         Ptr (Surface.all),
         Surface_Origin.X, Surface_Origin.Y);
   end Mask_Surface;

   ------------
   -- Stroke --
   ------------

   procedure Stroke (Context : in out Cairo_Context'Class) is
   begin
      cairo_stroke (Context.Ptr);
   end Stroke;

   ---------------------
   -- Stroke_Preserve --
   ---------------------

   procedure Stroke_Preserve (Context : in out Cairo_Context'Class) is
   begin
      cairo_stroke_preserve (Context.Ptr);
   end Stroke_Preserve;

   ----------
   -- Fill --
   ----------

   procedure Fill (Context : in out Cairo_Context'Class) is
   begin
      cairo_fill (Context.Ptr);
   end Fill;

   -------------------
   -- Fill_Preserve --
   -------------------

   procedure Fill_Preserve (Context : in out Cairo_Context'Class) is
   begin
      cairo_fill_preserve (Context.Ptr);
   end Fill_Preserve;

   ---------------
   -- Copy_Page --
   ---------------

   procedure Copy_Page (Context : in out Cairo_Context'Class) is
   begin
      cairo_copy_page (Context.Ptr);
   end Copy_Page;

   ---------------
   -- Show_Page --
   ---------------

   procedure Show_Page (Context : in out Cairo_Context'Class) is
   begin
      cairo_show_page (Context.Ptr);
   end Show_Page;

   ----------------
   -- Reset_Clip --
   ----------------

   procedure Reset_Clip (Context : in out Cairo_Context'Class) is
   begin
      cairo_reset_clip (Context.Ptr);
   end Reset_Clip;

   ----------
   -- Clip --
   ----------

   procedure Clip (Context : in out Cairo_Context'Class) is
   begin
      cairo_clip (Context.Ptr);
   end Clip;

   -------------------
   -- Clip_Preserve --
   -------------------

   procedure Clip_Preserve (Context : in out Cairo_Context'Class) is
   begin
      cairo_clip_preserve (Context.Ptr);
   end Clip_Preserve;

   ----------------------
   -- Select_Font_Face --
   ----------------------

   procedure Select_Font_Face
     (Context : in out Cairo_Context'Class;
      Family : String;
      Slant : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight)
   is
      C_Family : aliased Interfaces.C.char_array
        := Interfaces.C.To_C (Family, Append_Nul => True);
   begin
      cairo_select_font_face
        (Context.Ptr,
         To_Chars_Ptr (C_Family'Address),
         Slant,
         Weight);
   end Select_Font_Face;

   -------------------
   -- Set_Font_Size --
   -------------------

   procedure Set_Font_Size
     (Context : in out Cairo_Context'Class;
      Size : double)
   is
   begin
      cairo_set_font_size (Context.Ptr, Size);
   end Set_Font_Size;

   ---------------------
   -- Set_Font_Matrix --
   ---------------------

   procedure Set_Font_Matrix
     (Context : in out Cairo_Context'Class;
      Matrix : Cairo_Matrix)
   is
   begin
      cairo_set_font_matrix (Context.Ptr, Matrix);
   end Set_Font_Matrix;

   ----------------------
   -- Set_Font_Options --
   ----------------------

   procedure Set_Font_Options
     (Context : in out Cairo_Context'Class;
      Options : Cairo_Font_Options)
   is
   begin
      cairo_set_font_options (Context.Ptr, Ptr (Options));
   end Set_Font_Options;

   ---------------------
   -- Set_Scaled_Font --
   ---------------------

   procedure Set_Scaled_Font
     (Context : in out Cairo_Context'Class;
      Scaled_Font : access Cairo_Scaled_Font'Class)
   is
   begin
      cairo_set_scaled_font (Context.Ptr, Ptr (Scaled_Font.all));
   end Set_Scaled_Font;

   ---------------
   -- Show_Text --
   ---------------

   procedure Show_Text
     (Context : in out Cairo_Context'Class;
      UTF8 : String)
   is
      C_UTF8 : aliased Interfaces.C.char_array
        := Interfaces.C.To_C (UTF8, Append_Nul => True);
   begin
      cairo_show_text (Context.Ptr, To_Chars_Ptr (C_UTF8'Address));
   end Show_Text;

   -----------------
   -- Show_Glyphs --
   -----------------

   procedure Show_Glyphs
     (Context : in out Cairo_Context'Class;
      Glyphs : Cairo_Glyph_Array)
   is
   begin
      cairo_show_glyphs
        (Context.Ptr,
         To_Glyph_Ptr (Glyphs'Address),
         Glyphs'Length);
   end Show_Glyphs;

   ----------------------
   -- Show_Text_Glyphs --
   ----------------------

   procedure Show_Text_Glyphs
     (Context : in out Cairo_Context'Class;
      UTF8 : String;
      Glyphs : Cairo_Glyph_Array;
      Clusters : Cairo_Text_Cluster_Array;
      Cluster_Flags : Cairo_Text_Cluster_Flags)
   is
   begin
      cairo_show_text_glyphs
        (Context.Ptr,
         To_Chars_Ptr (UTF8'Address), UTF8'Length,
         To_Glyph_Ptr (Glyphs'Address), Glyphs'Length,
         To_Text_Cluster_Ptr (Clusters'Address), Clusters'Length,
         Cluster_Flags);
   end Show_Text_Glyphs;

   -------------------
   -- Set_Font_Face --
   -------------------

   procedure Set_Font_Face
     (Context : in out Cairo_Context'Class;
      Font_Face : access Cairo_Font_Face'Class)
   is
   begin
      cairo_set_font_face (Context.Ptr, Ptr (Font_Face.all));
   end Set_Font_Face;

   ---------------
   -- Text_Path --
   ---------------

   procedure Text_Path
     (Context : in out Cairo_Context'Class;
      UTF8 : String)
   is
      C_UTF8 : aliased Interfaces.C.char_array
        := Interfaces.C.To_C (UTF8, Append_Nul => True);
   begin
      cairo_text_path (Context.Ptr, To_Chars_Ptr (C_UTF8'Address));
   end Text_Path;

   ----------------
   -- Glyph_Path --
   ----------------

   procedure Glyph_Path
     (Context : in out Cairo_Context'Class;
      Glyphs : Cairo_Glyph_Array)
   is
   begin
      cairo_glyph_path
        (Context.Ptr,
         To_Glyph_Ptr (Glyphs'Address),
         Glyphs'Length);
   end Glyph_Path;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Context : Cairo_Context'Class)
      return Cairo_Status
   is
   begin
      return Support.cairo_status (Context.Ptr);
   end Get_Status;

   ---------------
   -- In_Stroke --
   ---------------

   function In_Stroke
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Boolean
   is
   begin
      return cairo_in_stroke (Context.Ptr, Point.X, Point.Y) /= 0;
   end In_Stroke;

   -------------
   -- In_Fill --
   -------------

   function In_Fill
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Boolean
   is
   begin
      return cairo_in_fill (Context.Ptr, Point.X, Point.Y) /= 0;
   end In_Fill;

   ---------------
   -- Copy_Path --
   ---------------

   procedure Copy_Path
     (Context : Cairo_Context'Class;
      Path : in out Cairo_Path)
   is
   begin
      Set_Ptr (Path, cairo_copy_path (Context.Ptr));
   end Copy_Path;

   --------------------
   -- Copy_Path_Flat --
   --------------------

   procedure Copy_Path_Flat
     (Context : Cairo_Context'Class;
      Path : in out Cairo_Path)
   is
   begin
      Set_Ptr (Path, cairo_copy_path_flat (Context.Ptr));
   end Copy_Path_Flat;

   ----------------------
   -- Get_Font_Extents --
   ----------------------

   function Get_Font_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Font_Extents
   is
      Result : Cairo_Font_Extents;
   begin
      Support.cairo_font_extents (Context.Ptr, Result);
      return Result;
   end Get_Font_Extents;

   ------------------------
   -- Get_Stroke_Extents --
   ------------------------

   function Get_Stroke_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box
   is
      Result : Cairo_Box;
   begin
      cairo_stroke_extents
        (Context.Ptr,
         Result.X1, Result.Y1, Result.X2, Result.Y2);
      return Result;
   end Get_Stroke_Extents;

   ----------------------
   -- Get_Fill_Extents --
   ----------------------

   function Get_Fill_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box
   is
      Result : Cairo_Box;
   begin
      cairo_fill_extents
        (Context.Ptr,
         Result.X1, Result.Y1, Result.X2, Result.Y2);
      return Result;
   end Get_Fill_Extents;

   ----------------------
   -- Get_Path_Extents --
   ----------------------

   function Get_Path_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box
   is
      Result : Cairo_Box;
   begin
      cairo_path_extents
        (Context.Ptr,
         Result.X1, Result.Y1, Result.X2, Result.Y2);
      return Result;
   end Get_Path_Extents;

   ----------------------
   -- Get_Text_Extents --
   ----------------------

   function Get_Text_Extents
     (Context : Cairo_Context'Class;
      UTF8 : String)
      return Cairo_Text_Extents
   is
      C_UTF8 : aliased Interfaces.C.char_array
        := Interfaces.C.To_C (UTF8, Append_Nul => True);
      Text_Extents : Cairo_Text_Extents;
   begin
      Support.cairo_text_extents
        (Context.Ptr,
         To_Chars_Ptr (C_UTF8'Address), Text_Extents);
      return Text_Extents;
   end Get_Text_Extents;

   -----------------------
   -- Get_Glyph_Extents --
   -----------------------

   function Get_Glyph_Extents
     (Context : Cairo_Context'Class;
      Glyphs : Cairo_Glyph_Array)
      return Cairo_Text_Extents
   is
      Text_Extents : Cairo_Text_Extents;
   begin
      cairo_glyph_extents
        (Context.Ptr,
         To_Glyph_Ptr (Glyphs'Address), Glyphs'Length, Text_Extents);
      return Text_Extents;
   end Get_Glyph_Extents;

   ----------------------
   -- Get_Font_Options --
   ----------------------

   function Get_Font_Options
     (Context : Cairo_Context'Class)
      return Cairo_Font_Options
   is
      Font_Options : Cairo_Font_Options;
   begin
      cairo_get_font_options (Context.Ptr, Ptr (Font_Options));
      return Font_Options;
   end Get_Font_Options;

   -------------------
   -- Get_Font_Face --
   -------------------

   function Get_Font_Face
     (Context : Cairo_Context'Class)
      return Cairo_Font_Face_Handle
   is
   begin
      return To_Handle
        (cairo_get_font_face (Context.Ptr),
         Is_Referenced => False);
   end Get_Font_Face;

   ---------------------
   -- Get_Font_Matrix --
   ---------------------

   function Get_Font_Matrix
     (Context : Cairo_Context'Class)
      return Cairo_Matrix
   is
      Matrix : Cairo_Matrix;
   begin
      cairo_get_font_matrix (Context.Ptr, Matrix);
      return Matrix;
   end Get_Font_Matrix;

   ------------------
   -- Get_Operator --
   ------------------

   function Get_Operator
     (Context : Cairo_Context'Class)
      return Cairo_Operator
   is
   begin
      return cairo_get_operator (Context.Ptr);
   end Get_Operator;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source
     (Context : Cairo_Context'Class)
      return Cairo_Pattern_Handle
   is
   begin
      return To_Handle
        (cairo_get_source (Context.Ptr),
         Is_Referenced => False);
   end Get_Source;

   -------------------
   -- Get_Tolerance --
   -------------------

   function Get_Tolerance
     (Context : Cairo_Context'Class)
      return double
   is
   begin
      return cairo_get_tolerance (Context.Ptr);
   end Get_Tolerance;

   -------------------
   -- Get_Antialias --
   -------------------

   function Get_Antialias
     (Context : Cairo_Context'Class)
      return Cairo_Antialias
   is
   begin
      return cairo_get_antialias (Context.Ptr);
   end Get_Antialias;

   -----------------------
   -- Has_Current_Point --
   -----------------------

   function Has_Current_Point
     (Context : Cairo_Context'Class)
      return Boolean
   is
   begin
      return cairo_has_current_point (Context.Ptr) /= 0;
   end Has_Current_Point;

   -----------------------
   -- Get_Current_Point --
   -----------------------

   function Get_Current_Point
     (Context : Cairo_Context'Class)
      return Cairo_Tuple
   is
      Point : Cairo_Tuple;
   begin
      cairo_get_current_point (Context.Ptr, Point.X, Point.Y);
      return Point;
   end Get_Current_Point;

   -------------------
   -- Get_Fill_Rule --
   -------------------

   function Get_Fill_Rule
     (Context : Cairo_Context'Class)
      return Cairo_Fill_Rule
   is
   begin
      return cairo_get_fill_rule (Context.Ptr);
   end Get_Fill_Rule;

   --------------------
   -- Get_Line_Width --
   --------------------

   function Get_Line_Width
     (Context : Cairo_Context'Class)
      return double
   is
   begin
      return cairo_get_line_width (Context.Ptr);
   end Get_Line_Width;

   ------------------
   -- Get_Line_Cap --
   ------------------

   function Get_Line_Cap
     (Context : Cairo_Context'Class)
      return Cairo_Line_Cap
   is
   begin
      return cairo_get_line_cap (Context.Ptr);
   end Get_Line_Cap;

   -------------------
   -- Get_Line_Join --
   -------------------

   function Get_Line_Join
     (Context : Cairo_Context'Class)
      return Cairo_Line_Join
   is
   begin
      return cairo_get_line_join (Context.Ptr);
   end Get_Line_Join;

   ---------------------
   -- Get_Miter_Limit --
   ---------------------

   function Get_Miter_Limit
     (Context : Cairo_Context'Class)
      return double
   is
   begin
      return cairo_get_miter_limit (Context.Ptr);
   end Get_Miter_Limit;

   --------------------
   -- Get_Dash_Count --
   --------------------

   function Get_Dash_Count (Context : Cairo_Context'Class) return Natural is
   begin
      return Natural (cairo_get_dash_count (Context.Ptr));
   end Get_Dash_Count;

   ----------------
   -- Get_Dashes --
   ----------------

   function Get_Dashes (Context : Cairo_Context'Class) return double_Array is
      Count : constant Natural := Get_Dash_Count (Context);
   begin
      declare
         Dashes : double_Array (0 .. Count - 1);
         Offset : aliased double;
      begin
         if Count > 0 then
            cairo_get_dash
              (Context.Ptr, Dashes (0)'Access, Offset'Access);
         end if;
         return Dashes;
      end;
   end Get_Dashes;

   ---------------------
   -- Get_Dash_Offset --
   ---------------------

   function Get_Dash_Offset (Context : Cairo_Context'Class) return double is
      Count : constant Natural := Get_Dash_Count (Context);
   begin
      if Count > 0 then
         declare
            Dashes : double_Array (0 .. Count - 1);
            Offset : aliased double;
         begin
            cairo_get_dash
              (Context.Ptr, Dashes (0)'Access, Offset'Access);
            return Offset;
         end;
      else
         return 0.0;
      end if;
   end Get_Dash_Offset;

   ----------------
   -- Get_Matrix --
   ----------------

   function Get_Matrix
     (Context : Cairo_Context'Class)
      return Cairo_Matrix
   is
      Matrix : Cairo_Matrix;
   begin
      cairo_get_matrix (Context.Ptr, Matrix);
      return Matrix;
   end Get_Matrix;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target
     (Context : Cairo_Context'Class)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
        (cairo_get_target (Context.Ptr),
         Is_Referenced => False);
   end Get_Target;

   ----------------------
   -- Get_Group_Target --
   ----------------------

   function Get_Group_Target
     (Context : Cairo_Context'Class)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
        (cairo_get_group_target (Context.Ptr),
         Is_Referenced => False);
   end Get_Group_Target;

   --------------------
   -- User_To_Device --
   --------------------

   function User_To_Device
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Cairo_Tuple
   is
      T : Cairo_Tuple := Point;
   begin
      cairo_user_to_device (Context.Ptr, T.X, T.Y);
      return T;
   end User_To_Device;

   -----------------------------
   -- User_To_Device_Distance --
   -----------------------------

   function User_To_Device_Distance
     (Context : Cairo_Context'Class;
      Vector : Cairo_Tuple)
      return Cairo_Tuple
   is
      T : Cairo_Tuple := Vector;
   begin
      cairo_user_to_device_distance (Context.Ptr, T.X, T.Y);
      return T;
   end User_To_Device_Distance;

   --------------------
   -- Device_To_User --
   --------------------

   function Device_To_User
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Cairo_Tuple
   is
      T : Cairo_Tuple := Point;
   begin
      cairo_device_to_user (Context.Ptr, T.X, T.Y);
      return T;
   end Device_To_User;

   -----------------------------
   -- Device_To_User_Distance --
   -----------------------------

   function Device_To_User_Distance
     (Context : Cairo_Context'Class;
      Vector : Cairo_Tuple)
      return Cairo_Tuple
   is
      T : Cairo_Tuple := Vector;
   begin
      cairo_device_to_user_distance (Context.Ptr, T.X, T.Y);
      return T;
   end Device_To_User_Distance;

   ----------------------
   -- Get_Clip_Extents --
   ----------------------

   function Get_Clip_Extents
     (Context : Cairo_Context'Class)
     return Cairo_Box
   is
      Result : Cairo_Box;
   begin
      cairo_clip_extents
        (Context.Ptr, Result.X1, Result.Y1, Result.X2, Result.Y2);
      return Result;
   end Get_Clip_Extents;

   ------------------------------
   -- Copy_Clip_Rectangle_List --
   ------------------------------

   procedure Copy_Clip_Rectangle_List
     (Context : Cairo_Context'Class;
      Rectangles : in out Cairo_Rectangle_List)
   is
   begin
      Set_Ptr (Rectangles, cairo_copy_clip_rectangle_list (Context.Ptr));
   end Copy_Clip_Rectangle_List;

   ---------
   -- Ref --
   ---------

   function Ref (Handle : Cairo_Context_Handle) return Cairo_Context_Ref is
   begin
      return Handle.Ref;
   end Ref;

   -----------
   -- Reset --
   -----------

   procedure Reset (Handle : in out Cairo_Context_Handle) is
   begin
      Handle := Cairo_Context_Null_Handle;
   end Reset;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Handle : Cairo_Context_Handle) return Boolean is
   begin
      return Handle /= Cairo_Context_Null_Handle;
   end Is_Set;

   ---------------
   -- To_Handle --
   ---------------

   function To_Handle
     (Ptr : Context_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Context_Handle
   is
      Ref : Cairo_Context_Ref;
   begin
--      Ada.Text_IO.Put_Line ("Cairo.Context.To_Handle Ptr:" & Img (Ptr));
      if Ptr /= null then
         declare
            Data : constant Cairo_User_Data
              := cairo_get_user_data (Ptr, Ada_Context_Key'Access);
            Status : Cairo_Status;
         begin
            if Data /= null then
               -- An Ada context was previously created and attached.
               -- So we reuse it
               Ref := To_Ref (Data);
            else
               -- No Ada context was ever created and atached.
               -- So we do this.
               Ref := new Cairo_Context;
               Status := cairo_set_user_data
                 (Ptr, Ada_Context_Key'Access,
                  To_User_Data (Ref),
                  Destroy_Context'Access);
               pragma Assert (Status = CAIRO_STATUS_SUCCESS);
            end if;
            pragma Assert (Ref /= null);
            if Is_Referenced then
               Ref.Ptr := Ptr;
            else
               Ref.Ptr := cairo_reference (Ptr);
            end if;
         end;
      end if;
      return Cairo_Context_Handle'(Ada.Finalization.Controlled with Ref => Ref);
   end To_Handle;

   ---------
   -- Ptr --
   ---------

   function Ptr (Context : Cairo_Context'Class) return Context_Ptr is
   begin
      return Context.Ptr;
   end Ptr;

   ----------------
   -- Initialize --
   ----------------

--     procedure Initialize (O : in out Cairo_Context_Handle) is
--        pragma Unreferenced (O);
--     begin
--        null;
--  --      Ada.Text_IO.Put_Line ("Cairo.Context.Initialize H:" & Img (O));
--     end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Cairo_Context_Handle) is
   begin
--      Ada.Text_IO.Put_Line ("Cairo.Context.Adjust H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         O.Ref.Ptr := cairo_reference (O.Ref.Ptr);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Context_Handle) is
   begin
--      Ada.Text_IO.Put_Line ("Cairo.Context.Finalize H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         cairo_destroy (O.Ref.Ptr);
      end if;
   end Finalize;

end Cairo.Context;
