with Cairo; use Cairo;
--with Cairo.Font_Options; use Cairo.Font_Options;
--with Cairo.Path; use Cairo.Path;
with Cairo.Surface; use Cairo.Surface;
with Cairo.Surface.Image; use Cairo.Surface.Image;
with Cairo.Surface.PDF; use Cairo.Surface.PDF;
with Cairo.Surface.SVG; use Cairo.Surface.SVG;
with Cairo.Surface.PS; use Cairo.Surface.PS;
with Cairo.Pattern; use Cairo.Pattern;
--with Cairo.Pattern.Solid; use Cairo.Pattern.Solid;
with Cairo.Pattern.Surface; use Cairo.Pattern.Surface;
with Cairo.Pattern.Gradient; use Cairo.Pattern.Gradient;
with Cairo.Pattern.Gradient.Linear; use Cairo.Pattern.Gradient.Linear;
with Cairo.Pattern.Gradient.Radial; use Cairo.Pattern.Gradient.Radial;
with Cairo.Context; use Cairo.Context;
with Interfaces.C; use Interfaces.C;
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Streams.Stream_IO;
with Ada.Numerics.Float_Random;
with RSVG; use RSVG;
with Glib.Error;
--with Gtk.Main;

package body Cairo_Test_Snippets_Pkg is

   WIDTH_IN_INCHES  : constant := 3.0;
   HEIGHT_IN_INCHES : constant := 3.0;
   WIDTH_IN_POINTS  : constant := WIDTH_IN_INCHES  * 72.0;
   HEIGHT_IN_POINTS : constant := HEIGHT_IN_INCHES * 72.0;

--   procedure Print (Options : Cairo_Font_Options);
--   function Image (T : Cairo.Tuple) return String;
--   procedure Print (Path : Cairo_Path);


--   procedure Print (Options : Cairo_Font_Options) is
--      use Cairo;
--      use Interfaces.C;
--   begin
--      Ada.Text_IO.Put_Line ("Font_Options:");
--      Ada.Text_IO.Put_Line ("   Status        :" & Cairo.Status'Image (Get_Status (Options)));
--      Ada.Text_IO.Put_Line ("   Antialias     :" & Cairo.Antialias'Image (Get_Antialias (Options)));
--      Ada.Text_IO.Put_Line ("   Subpixel_Order:" & Cairo.Subpixel_Order'Image (Get_Subpixel_Order (Options)));
--      Ada.Text_IO.Put_Line ("   Hint_Style    :" & Cairo.Hint_Style'Image (Get_Hint_Style (Options)));
--      Ada.Text_IO.Put_Line ("   Hint_Metrics  :" & Cairo.Hint_Metrics'Image (Get_Hint_Metrics (Options)));
--      Ada.Text_IO.Put_Line ("   Hash          :" & unsigned_long'Image (Hash (Options)));
--   end Print;

--   function Image (T : Cairo.Tuple) return String is
--   begin
--      return "(" & double'Image (T.X) & ", " & double'Image (T.Y) & ")";
--   end Image;

--   procedure Print (Path : Cairo_Path) is
--      Cursor : Cairo_Path_Cursor;
--   begin
--      Cursor := To_Cursor (Path);
--      Ada.Text_IO.Put_Line ("Path (" & Cairo.Status'Image (Get_Status (Path)) & " " & int'Image (Get_Num_Data (Path)) & "):");
--      while Is_Valid (Cursor) loop
--         case Get_Type (Cursor) is
--         when Cairo.PATH_MOVE_TO =>
--            Ada.Text_IO.Put_Line ("   MOVE_TO:" & Image (Get_Point(Cursor, 1)));
--         when Cairo.PATH_LINE_TO =>
--            Ada.Text_IO.Put_Line ("   LINE_TO:" & Image (Get_Point(Cursor, 1)));
--         when Cairo.PATH_CURVE_TO =>
--            Ada.Text_IO.Put_Line ("   CURVE_TO:" & Image (Get_Point(Cursor, 1)) &
--                                  " " & Image (Get_Point(Cursor, 2)) &
--                                  " " & Image (Get_Point(Cursor, 3)));
--         when Cairo.PATH_CLOSE_PATH  =>
--            Ada.Text_IO.Put_Line ("   CLOSE_PATH");
--         end case;
--         Next (Cursor);
--      end loop;
--   end Print;

   type Checker_Ref is access procedure (Context : in out Cairo_Context'Class; Width, Height : double);
   type Checker_Slot is record
      Ref : Checker_Ref;
      Name : String (1 .. 128);
      Name_Last : Natural := 0;
   end record;
   type Checker_Slot_Array is array (Positive range <>) of Checker_Slot;
   Checker_Slots : Checker_Slot_Array (1 .. 100); -- 100 checkers max
   Checker_Slots_Last : Natural := 0; -- Number of Registered checkers

   procedure Register (Ref : Checker_Ref; Name : String) is
   begin
      if Ref = null then
         return;
      end if;

      if Checker_Slots_Last < Checker_Slots'Last then
         Checker_Slots_Last := Checker_Slots_Last + 1;
         Checker_Slots (Checker_Slots_Last).Ref := Ref;
         -- Dirty ! No bound check
         Checker_Slots (Checker_Slots_Last).Name (1 .. Name'Length) := Name;
         Checker_Slots (Checker_Slots_Last).Name_Last := Name'Length;
      end if;
   end Register;

   function Normalize (S : String) return String is
      T : String := S;
   begin
      for I in T'Range loop
         if T (I) = ' ' then
            T (I) := '-';
         elsif T (I) in 'A' .. 'Z' then
            T (I) := Character'Val (Character'Pos (T (I)) - Character'Pos ('A') + Character'Pos ('a'));
         end if;
      end loop;
      return T;
   end Normalize;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Scale (Context, Width, Height);
      Set_Line_Width (Context, 0.04);
   end Normalize;

   procedure Set_BG_SVG (Context : in out Cairo_Context'Class; Filename : String) is
      use Glib.Error;
      Handle : RSVG_Handle;
      Error : GError;
      Success : Boolean;
      Dimensions : RSVG_Dimension_Data;
   begin

--     unsigned int width,height;
--     svg_cairo_t *svgc;
--     svg_cairo_create (&svgc);
--     svg_cairo_parse (svgc, file);
--     svg_cairo_get_size (svgc, &width, &height);
--     cairo_save (cr);
--       cairo_scale (cr, 1.0/width, 1.0/height);
--       svg_cairo_render (svgc, cr);
--     cairo_restore (cr);
--     svg_cairo_destroy (svgc);

      RSVG_New_From_File (Handle, Filename, Error);
      Dimensions := Get_Dimensions (Handle);
      Save (Context);
      Scale (Context, 1.0 / double (Dimensions.Width), 1.0 / double (Dimensions.Height));
      Render (Handle, Context, Success);
      Restore (Context);
      Unref (Handle);
   end Set_BG_SVG;

   ---------------
   -- Check_Arc --
   ---------------

   procedure Check_Arc (Context : in out Cairo_Context'Class; Width, Height : double) is
      Center : constant Cairo_Tuple := (0.5, 0.5);
      Radius : constant double := 0.4;
      Angle1 : constant double :=  Pi / 4.0;
      Angle2 : constant double := Pi;
   begin
      Normalize (Context, Width, Height);

      -- Draw Arc
      Arc (Context, Center, Radius, Angle1, Angle2);
      Stroke (Context);

      -- Draw helping lines
      Set_Source_RGBA (Context, 1.0, 0.2, 0.2, 0.6);
      Arc (Context, Center, 0.05, 0.0, 2.0*Pi);
      Fill (Context);
      Set_Line_Width (Context, 0.03);
      -- Move Current point to Arc end 1
      Arc (Context, Center, Radius, Angle1, Angle1);
      Line_To (Context, Center);
      -- Move Current point to Arc end 2
      Arc (Context, Center, Radius, Angle2, Angle2);
      Line_To (Context, Center);
      Stroke (Context);
   end Check_Arc;

   ------------------------
   -- Check_Arc_Negative --
   ------------------------

   procedure Check_Arc_Negative (Context : in out Cairo_Context'Class; Width, Height : double) is
      Center : constant Cairo_Tuple := (0.5, 0.5);
      Radius : constant double := 0.4;
      Angle1 : constant double :=  Pi / 4.0;
      Angle2 : constant double := Pi;
   begin
      Normalize (Context, Width, Height);

      -- Draw Arc
      Arc_Negative (Context, Center, Radius, Angle1, Angle2);
      Stroke (Context);

      -- Draw helping lines
      Set_Source_RGBA (Context, 1.0, 0.2, 0.2, 0.6);
      Arc (Context, Center, 0.05, 0.0, 2.0*Pi);
      Fill (Context);
      Set_Line_Width (Context, 0.03);
      -- Move Current point to Arc end 1
      Arc (Context, Center, Radius, Angle1, Angle1);
      Line_To (Context, Center);
      -- Move Current point to Arc end 2
      Arc (Context, Center, Radius, Angle2, Angle2);
      Line_To (Context, Center);
      Stroke (Context);
   end Check_Arc_Negative;

   ----------------
   -- Check_Clip --
   ----------------

   procedure Check_Clip (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      Arc (Context, (0.5, 0.5), 0.3, 0.0, 2.0 * Pi);
      Clip (Context);

      New_Path (Context);  -- current path is not consumed by Clip
      Rectangle (Context, (0.0, 0.0), 1.0, 1.0);
      Fill (Context);
      Set_Source_RGB (Context, 0.0, 1.0, 0.0);
      Move_To (Context, (0.0, 0.0));
      Line_To (Context, (1.0, 1.0));
      Move_To (Context, (1.0, 0.0));
      Line_To (Context, (0.0, 1.0));
      Stroke (Context);
   end Check_Clip;

   ----------------------
   -- Check_Clip_Image --
   ----------------------

   procedure Check_Clip_Image (Context : in out Cairo_Context'Class; Width, Height : double) is
      W, H : double;
      Image : Cairo_Surface_Handle;
   begin
      Normalize (Context, Width, Height);

      Arc (Context, (0.5, 0.5), 0.3, 0.0, 2.0*Pi);
      Clip (Context);
      New_Path (Context); -- path not consumed by Clip

      Image := New_Image_Surface_From_PNG ("data/romedalen.png");
      W := double (Get_Width (Cairo_Image_Surface_Ref (Ref (Image)).all));
      H := double (Get_Height (Cairo_Image_Surface_Ref (Ref (Image)).all));

      Scale (Context, 1.0/W, 1.0/H);

      Set_Source_Surface (Context, Ref (Image), 0.0, 0.0);
      Paint (Context);

   end Check_Clip_Image;

   ---------------------
   -- Check_Rectangle --
   ---------------------

   procedure Check_Rectangle (Context : in out Cairo_Context'Class; Width, Height : double) is
      X0 : constant double := 0.1;   -- parameters like Rectangle
      Y0 : constant double := 0.1;
      Rect_Width  : constant double := 0.8;
      Rect_Height : constant double := 0.8;
   begin
      Normalize (Context, Width, Height);

      Rectangle (Context, X0, Y0, Rect_Width, Rect_Height);

      Set_Source_RGB (Context, 0.5, 0.5, 1.0);
      Fill_Preserve (Context);
      Set_Source_RGBA (Context, 0.5, 0.0, 0.0, 0.5);
      Stroke (Context);
   end Check_Rectangle;

   ---------------------------
   -- Check_Curve_Rectangle --
   ---------------------------

   procedure Check_Curve_Rectangle (Context : in out Cairo_Context'Class; Width, Height : double) is
      -- a custom shape, that could be wrapped in a function
      X0 : constant double := 0.1;   -- parameters like Rectangle
      Y0 : constant double := 0.1;
      Rect_Width  : constant double := 0.8;
      Rect_Height : constant double := 0.8;
      Radius : constant double := 0.4;   -- and an approximate curvature Radius
      X1, Y1 : double;
   begin
      Normalize (Context, Width, Height);
      X1 := X0 + Rect_Width;
      Y1 := Y0 + Rect_Height;

      if Rect_Width = 0.0 or Rect_Height = 0.0 then
         return;
      end if;

      if Rect_Width / 2.0 < Radius then
         if Rect_Height / 2.0 < Radius then
            Move_To  (Context, X0, (Y0 + Y1) / 2.0);
            Curve_To (Context, X0, Y0, X0, Y0, (X0 + X1) / 2.0, Y0);
            Curve_To (Context, X1, Y0, X1, Y0, X1, (Y0 + Y1) / 2.0);
            Curve_To (Context, X1, Y1, X1, Y1, (X1 + X0) / 2.0, Y1);
            Curve_To (Context, X0, Y1, X0, Y1, X0, (Y0 + Y1) / 2.0);
         else
            Move_To  (Context, X0, Y0 + Radius);
            Curve_To (Context, X0, Y0, X0, Y0, (X0 + X1) / 2.0, Y0);
            Curve_To (Context, X1, Y0, X1, Y0, X1, Y0 + Radius);
            Line_To (Context, X1, Y1 - Radius);
            Curve_To (Context, X1, Y1, X1, Y1, (X1 + X0) / 2.0, Y1);
            Curve_To (Context, X0, Y1, X0, Y1, X0, Y1 - Radius);
         end if;
      else
         if Rect_Height / 2.0 < Radius then
            Move_To  (Context, X0, (Y0 + Y1) / 2.0);
            Curve_To (Context, X0, Y0, X0, Y0, X0 + Radius, Y0);
            Line_To (Context, X1 - Radius, Y0);
            Curve_To (Context, X1, Y0, X1, Y0, X1, (Y0 + Y1) / 2.0);
            Curve_To (Context, X1, Y1, X1, Y1, X1 - Radius, Y1);
            Line_To (Context, X0 + Radius, Y1);
            Curve_To (Context, X0, Y1, X0, Y1, X0, (Y0 + Y1) / 2.0);
         else
            Move_To  (Context, X0, Y0 + Radius);
            Curve_To (Context, X0, Y0, X0, Y0, X0 + Radius, Y0);
            Line_To (Context, X1 - Radius, Y0);
            Curve_To (Context, X1, Y0, X1, Y0, X1, Y0 + Radius);
            Line_To (Context, X1, Y1 - Radius);
            Curve_To (Context, X1, Y1, X1, Y1, X1 - Radius, Y1);
            Line_To (Context, X0 + Radius, Y1);
            Curve_To (Context, X0, Y1, X0, Y1, X0, Y1 - Radius);
         end if;
      end if;

      Close_Path (Context);

      Set_Source_RGB (Context, 0.5, 0.5, 1.0);
      Fill_Preserve (Context);
      Set_Source_RGBA (Context, 0.5, 0.0, 0.0, 0.5);
      Stroke (Context);
   end Check_Curve_Rectangle;

   --------------------
   -- Check_Curve_To --
   --------------------

   procedure Check_Curve_To (Context : in out Cairo_Context'Class; Width, Height : double) is
      P0 : constant Cairo_Tuple := (0.1, 0.5);
      P1 : constant Cairo_Tuple := (0.4, 0.9);
      P2 : constant Cairo_Tuple := (0.6, 0.1);
      P3 : constant Cairo_Tuple := (0.9, 0.5);
   begin

      Normalize (Context, Width, Height);

      Move_To (Context,  P0);
      Curve_To (Context, P1, P2, P3);

      Stroke (Context);

      Set_Source_RGBA (Context, 1.0, 0.2, 0.2, 0.6);
      Set_Line_Width (Context, 0.03);
      Move_To (Context, P0);
      Line_To (Context, P1);
      Move_To (Context, P2);
      Line_To (Context, P3);
      Stroke (Context);
   end Check_Curve_To;

   ----------------------------
   -- Check_Fill_And_Stroke2 --
   ----------------------------

   procedure Check_Fill_And_Stroke2 (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      Move_To (Context, (0.5, 0.1));
      Line_To (Context, (0.9, 0.9));
      Rel_Line_To (Context, (-0.4, 0.0));
      Curve_To (Context, 0.2, 0.9, 0.2, 0.5, 0.5, 0.5);
      Close_Path (Context);

      Move_To (Context, 0.25, 0.1);
      Rel_Line_To (Context, 0.2, 0.2);
      Rel_Line_To (Context, -0.2, 0.2);
      Rel_Line_To (Context, -0.2, -0.2);
      Close_Path (Context);

      Set_Source_RGB (Context, 0.0, 0.0, 1.0);
      Fill_Preserve (Context);
      Set_Source_RGB (Context, 0.0, 0.0, 0.0);
      Stroke (Context);
   end Check_Fill_And_Stroke2;

   ---------------------------
   -- Check_Fill_And_Stroke --
   ---------------------------

   procedure Check_Fill_And_Stroke (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      Move_To (Context, 0.5, 0.1);
      Line_To (Context, 0.9, 0.9);
      Rel_Line_To (Context, -0.4, 0.0);
      Curve_To (Context, 0.2, 0.9, 0.2, 0.5, 0.5, 0.5);
      Close_Path (Context);

      Set_Source_RGB (Context, 0.0, 0.0, 1.0);
      Fill_Preserve (Context);
      Set_Source_RGB (Context, 0.0, 0.0, 0.0);
      Stroke (Context);
   end Check_Fill_And_Stroke;

   --------------------
   -- Check_Gradient --
   --------------------

   procedure Check_Gradient (Context : in out Cairo_Context'Class; Width, Height : double) is
      Pattern : Cairo_Pattern_Handle;
   begin
      Normalize (Context, Width, Height);

      Pattern := New_Linear_Gradient ((0.0, 0.0), (0.0, 1.0));
      Add_Color_Stop_RGBA (Cairo_Gradient_Ref (Ref (Pattern)).all, 1.0, 0.0, 0.0, 0.0, 1.0);
      Add_Color_Stop_RGBA (Cairo_Gradient_Ref (Ref (Pattern)).all, 0.0, 1.0, 1.0, 1.0, 1.0);
      Rectangle (Context, 0.0, 0.0, 1.0, 1.0);
      Set_Source (Context, Ref (Pattern));
      Fill (Context);

      Pattern := New_Radial_Gradient ((0.45, 0.4), 0.1,
                                  (0.4,  0.4), 0.5);
      Add_Color_Stop_RGBA (Cairo_Gradient_Ref (Ref (Pattern)).all, 0.0, 1.0, 1.0, 1.0, 1.0);
      Add_Color_Stop_RGBA (Cairo_Gradient_Ref (Ref (Pattern)).all, 1.0, 0.0, 0.0, 0.0, 1.0);
      Set_Source (Context, Ref (Pattern));
      Arc (Context, 0.5, 0.5, 0.3, 0.0, 2.0 * Pi);
      Fill (Context);
   end Check_Gradient;

   -----------------
   -- Check_Image --
   -----------------

   procedure Check_Image (Context : in out Cairo_Context'Class; Width, Height : double) is

      W, H : double;
      Image : Cairo_Surface_Handle;
   begin
      Normalize (Context, Width, Height);

      Image := New_Image_Surface_From_PNG ("data/romedalen.png");
      W := double (Get_Width (Cairo_Image_Surface_Ref (Ref (Image)).all));
      H := double (Get_Height (Cairo_Image_Surface_Ref (Ref (Image)).all));

      Translate (Context, 0.5, 0.5);
      Rotate (Context, Pi / 4.0);
      Scale  (Context, 1.0 / W, 1.0 / H);
      Translate (Context, -0.5*W, -0.5*H);

      Set_Source_Surface (Context, Ref (Image), 0.0, 0.0);
      Paint (Context);
   end Check_Image;

   -------------------------
   -- Check_Image_Pattern --
   -------------------------

   procedure Check_Image_Pattern (Context : in out Cairo_Context'Class; Width, Height : double) is
      W, H : double;
      Image : Cairo_Surface_Handle;
      Pattern : Cairo_Pattern_Handle;
      Matrix : Cairo_Matrix;
   begin
      Normalize (Context, Width, Height);

      Image := New_Image_Surface_From_PNG ("data/romedalen.png");
      W := double (Get_Width (Cairo_Image_Surface_Ref (Ref (Image)).all));
      H := double (Get_Height (Cairo_Image_Surface_Ref (Ref (Image)).all));

      Pattern := New_Surface_Pattern (Ref (Image));
      Set_Extend (Cairo_Surface_Pattern_Ref (Ref (Pattern)).all, CAIRO_EXTEND_REPEAT);

      Translate (Context, 0.5, 0.5);
      Rotate (Context, Pi / 4.0);
      Scale (Context, 1.0 / double (Sqrt (2.0)), 1.0 / double (Sqrt (2.0)));
      Translate (Context, -0.5, -0.5);

      Init_Scale (Matrix, W * 5.0, H * 5.0);
      Set_Matrix (Ref (Pattern).all, Matrix);

      Set_Source (Context, Ref (Pattern));

      Rectangle (Context, 0.0, 0.0, 1.0, 1.0);
      Fill (Context);

   end Check_Image_Pattern;

   ----------------
   -- Check_RSVG --
   ----------------

   procedure Check_RSVG (Context : in out Cairo_Context'Class; Width, Height : double) is
      use Glib.Error;
      Handle : RSVG_Handle;
      Error : GError;
      Dimensions : RSVG_Dimension_Data;
      Success : Boolean;
   begin
      Normalize (Context, Width, Height);
      RSVG_New_From_File (Handle, "data/home.svg", Error);
      Dimensions := Get_Dimensions (Handle);
      Scale (Context, 1.0 / double (Dimensions.Width), 1.0 / double (Dimensions.Height));
      Render (Handle, Context, Success);
      Unref (Handle);
   end Check_RSVG;

   --------------------
   -- Check_Operator --
   --------------------

   procedure Check_Operator (Context : in out Cairo_Context'Class; Width, Height : double; Operator : Cairo_Operator) is
   begin
      Normalize (Context, Width, Height);
      Set_BG_SVG (Context, "data/freedesktop.svg");
      Set_Operator (Context, Operator);
      Set_Source_RGBA (Context, 1.0, 0.0, 0.0, 0.5);
      Rectangle (Context, 0.2, 0.2, 0.5, 0.5);
      Fill (Context);
      Set_Source_RGB (Context, 0.0, 1.0, 0.0);
      Rectangle (Context, 0.4, 0.4, 0.4, 0.4);
      Fill (Context);
      Set_Source_RGB (Context, 0.0, 0.0, 1.0);
      Rectangle (Context, 0.6, 0.6, 0.3, 0.3);
      Fill (Context);
   end Check_Operator;

   -------------------------
   -- Check_Operator_Add --
   -------------------------

   procedure Check_Operator_Add (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_ADD);
   end Check_Operator_Add;

   -------------------------
   -- Check_Operator_Atop --
   -------------------------

   procedure Check_Operator_Atop (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_ATOP);
   end Check_Operator_Atop;

   ---------------------------------
   -- Check_Operator_Atop_Reverse --
   ---------------------------------

   procedure Check_Operator_Atop_Reverse (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_DEST_ATOP);
   end Check_Operator_Atop_Reverse;

   -----------------------
   -- Check_Operator_In --
   -----------------------

   procedure Check_Operator_In (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_IN);
   end Check_Operator_In;

   -------------------------------
   -- Check_Operator_In_Reverse --
   -------------------------------

   procedure Check_Operator_In_Reverse (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_DEST_IN);
   end Check_Operator_In_Reverse;

   ------------------------
   -- Check_Operator_Out --
   ------------------------

   procedure Check_Operator_Out (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_OUT);
   end Check_Operator_Out;

   --------------------------------
   -- Check_Operator_Out_Reverse --
   --------------------------------

   procedure Check_Operator_Out_Reverse (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_DEST_OUT);
   end Check_Operator_Out_Reverse;

   -------------------------
   -- Check_Operator_Over --
   -------------------------

   procedure Check_Operator_Over (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_OVER);
   end Check_Operator_Over;

   ---------------------------------
   -- Check_Operator_Over_Reverse --
   ---------------------------------

   procedure Check_Operator_Over_Reverse (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_DEST_OVER);
   end Check_Operator_Over_Reverse;

   -----------------------------
   -- Check_Operator_Saturate --
   -----------------------------

   procedure Check_Operator_Saturate (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_SATURATE);
   end Check_Operator_Saturate;

   ------------------------
   -- Check_Operator_Xor --
   ------------------------

   procedure Check_Operator_Xor (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Check_Operator (Context, Width, Height, CAIRO_OPERATOR_XOR);
   end Check_Operator_Xor;

   ----------------
   -- Check_Path --
   ----------------

   procedure Check_Path (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);
      Move_To (Context, 0.5, 0.1);
      Line_To (Context, 0.9, 0.9);
      Rel_Line_To (Context, -0.4, 0.0);
      Curve_To (Context, 0.2, 0.9, 0.2, 0.5, 0.5, 0.5);
      Stroke (Context);
   end Check_Path;

   ------------------------
   -- Check_Pattern_Fill --
   ------------------------

   procedure Check_Pattern_Fill (Context : in out Cairo_Context'Class; Width, Height : double) is
      -- Is this really a pattern fill example ???
      X_FUZZ : constant := 0.08;
      Y_FUZZ : constant := 0.08;
      X_INNER_RADIUS : constant := 0.3;
      Y_INNER_RADIUS : constant := 0.2;
      X_OUTER_RADIUS : constant := 0.45;
      Y_OUTER_RADIUS : constant := 0.35;
      SPIKES : constant := 10.0;

      I : Float;
      X : double;
      Y : double;
      Text : constant String := "KAPOW!";
      Extents : Cairo_Text_Extents;
      Gen : Ada.Numerics.Float_Random.Generator;
      use Ada.Numerics.Float_Random;
   begin
      Normalize (Context, Width, Height); -- DC
      -- Draw a star shape
      Reset (Gen, 0);
      Set_Line_Width (Context, 0.01);
      I := 0.0;
      loop
         exit when Integer (I) >= 2 * Integer (SPIKES);

         X := double (0.5 + Cos (Pi * I / SPIKES) * X_INNER_RADIUS + Random (Gen) * X_FUZZ);
         Y := double (0.5 + Sin (Pi * I / SPIKES) * Y_INNER_RADIUS + Random (Gen) * Y_FUZZ);
         if I = 0.0 then
            Move_To (Context, X, Y);
         else
            Line_To (Context, X, Y);
         end if;

         I := I + 1.0;

         X := double (0.5 + Cos (Pi * I / SPIKES) * X_OUTER_RADIUS + Random (Gen) * X_FUZZ);
         Y := double (0.5 + Sin (Pi * I / SPIKES) * Y_OUTER_RADIUS + Random (Gen) * Y_FUZZ);

         Line_To (Context, X, Y);
         I := I + 1.0;
      end loop;

      Close_Path (Context);
      Stroke (Context);

      --
      Select_Font_Face (Context, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
      Move_To (Context, X, Y);
      Text_Path (Context, Text);

      Set_Font_Size (Context, 0.2);
      Extents := Get_Text_Extents (Context, Text);
      X := 0.5 - (Extents.Width / 2.0 + Extents.X_Bearing);
      Y := 0.5 - (Extents.Height / 2.0 + Extents.Y_Bearing);

      Move_To (Context, X, Y); -- DC
      Text_Path (Context, Text); -- DC


      Set_Source_RGB (Context, 1.0, 1.0, 0.5);
      Fill (Context);

      Move_To (Context, X, Y);
      Text_Path (Context, Text);
      Set_Source_RGB (Context, 0.0, 0.0, 0.0);
      Stroke (Context);
   end Check_Pattern_Fill;

   ------------------------
   -- Check_Set_Line_Cap --
   ------------------------

   procedure Check_Set_Line_Cap (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);
      Set_Line_Width (Context, 0.12);
      Set_Line_Cap  (Context, CAIRO_LINE_CAP_BUTT); -- default
      Move_To (Context, 0.25, 0.2);
      Line_To (Context, 0.25, 0.8);
      Stroke (Context);
      Set_Line_Cap  (Context, CAIRO_LINE_CAP_ROUND);
      Move_To (Context, 0.5, 0.2);
      Line_To (Context, 0.5, 0.8);
      Stroke (Context);
      Set_Line_Cap  (Context, CAIRO_LINE_CAP_SQUARE);
      Move_To (Context, 0.75, 0.2);
      Line_To (Context, 0.75, 0.8);
      Stroke (Context);

      -- Draw helping lines
      Set_Source_RGB (Context, 1.0, 0.2, 0.2);
      Set_Line_Width (Context, 0.01);
      Move_To (Context, 0.25, 0.2);
      Line_To (Context, 0.25, 0.8);
      Move_To (Context, 0.5, 0.2);
      Line_To (Context, 0.5, 0.8);
      Move_To (Context, 0.75, 0.2);
      Line_To (Context, 0.75, 0.8);
      Stroke (Context);
   end Check_Set_Line_Cap;

   -------------------------
   -- Check_Set_Line_Join --
   -------------------------

   procedure Check_Set_Line_Join (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);
      Set_Line_Width (Context, 0.16);
      Move_To (Context, 0.3, 0.33);
      Rel_Line_To (Context, 0.2, -0.2);
      Rel_Line_To (Context, 0.2, 0.2);
      Set_Line_Join (Context, CAIRO_LINE_JOIN_MITER); -- Default
      Stroke (Context);

      Move_To (Context, 0.3, 0.63);
      Rel_Line_To (Context, 0.2, -0.2);
      Rel_Line_To (Context, 0.2, 0.2);
      Set_Line_Join (Context, CAIRO_LINE_JOIN_BEVEL);
      Stroke (Context);

      Move_To (Context, 0.3, 0.93);
      Rel_Line_To (Context, 0.2, -0.2);
      Rel_Line_To (Context, 0.2, 0.2);
      Set_Line_Join (Context, CAIRO_LINE_JOIN_ROUND);
      Stroke (Context);
   end Check_Set_Line_Join;

   -----------------------------
   -- Check_Text_Align_Center --
   -----------------------------

   procedure Check_Text_Align_Center (Context : in out Cairo_Context'Class; Width, Height : double) is
      Extents : Cairo_Text_Extents;
      UTF8 : constant String := "cairo";
      X, Y : double;
   begin
      Normalize (Context, Width, Height);

      Select_Font_Face (Context, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);

      Set_Font_Size (Context, 0.2);
      Extents := Get_Text_Extents (Context, UTF8);
      X := 0.5 - (Extents.Width / 2.0 + Extents.X_Bearing);
      Y := 0.5 - (Extents.Height / 2.0 + Extents.Y_Bearing);

      Move_To (Context, X, Y);
      Show_Text (Context, UTF8);

      -- Draw helping lines
      Set_Source_RGBA (Context, 1.0, 0.2, 0.2, 0.6);
      Arc (Context, X, Y, 0.05, 0.0, 2.0*Pi);
      Fill (Context);
      Move_To (Context, 0.5, 0.0);
      Rel_Line_To (Context, 0.0, 1.0);
      Move_To (Context, 0.0, 0.5);
      Rel_Line_To (Context, 1.0, 0.0);
      Stroke (Context);
   end Check_Text_Align_Center;

   ----------------
   -- Check_Text --
   ----------------

   procedure Check_Text (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);
      Select_Font_Face (Context, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
      Set_Font_Size (Context, 0.35);

      Move_To (Context, 0.04, 0.53);
      Show_Text (Context, "Hello");

      Move_To (Context, 0.27, 0.65);
      Text_Path (Context, "void");
      Set_Source_RGB (Context, 0.5, 0.5, 1.0);
      Fill_Preserve (Context);
      Set_Source_RGB (Context, 0.0, 0.0, 0.0);
      Set_Line_Width (Context, 0.01);
      Stroke (Context);

      -- Draw helping lines
      Set_Source_RGBA (Context, 1.0, 0.2, 0.2, 0.6);
      Arc (Context, 0.04, 0.53, 0.02, 0.0, 2.0*Pi);
      Arc (Context, 0.27, 0.65, 0.02, 0.0, 2.0*Pi);
      Fill (Context);
   end Check_Text;

   ------------------------
   -- Check_Text_Extents --
   ------------------------

   procedure Check_Text_Extents (Context : in out Cairo_Context'Class; Width, Height : double) is
      Extents : Cairo_Text_Extents;
      UTF8 : constant String := "cairo";
      X, Y : double;
   begin
      Normalize (Context, Width, Height);

      Select_Font_Face (Context, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
      Set_Font_Size (Context, 0.4);
      Extents := Get_Text_Extents (Context, UTF8);

      X := 0.1;
      Y := 0.6;

      Move_To (Context, X, Y);
      Show_Text (Context, UTF8);

      -- Draw helping lines
      Set_Source_RGBA (Context, 1.0, 0.2, 0.2, 0.6);
      Arc (Context, X, Y, 0.05, 0.0, 2.0*Pi);
      Fill (Context);
      Move_To (Context, X, Y);
      Rel_Line_To (Context, 0.0, -Extents.Height);
      Rel_Line_To (Context, Extents.Width, 0.0);
      Rel_Line_To (Context, Extents.X_Bearing, -Extents.Y_Bearing);
      Stroke (Context);
   end Check_Text_Extents;

   --------------------------
   -- Check_Clip_Rectangle --
   --------------------------

   procedure Check_Clip_Rectangle (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      New_Path (Context);
      Move_To (Context, 0.25, 0.25);
      Line_To (Context, 0.25, 0.75);
      Line_To (Context, 0.75, 0.75);
      Line_To (Context, 0.75, 0.25);
      Line_To (Context, 0.25, 0.25);
      Close_Path (Context);

      Clip (Context);

      Move_To (Context, 0.0, 0.0);
      Line_To (Context, 1.0, 1.0);
      Stroke (Context);
   end Check_Clip_Rectangle;

   ----------------
   -- Check_Dash --
   ----------------

   procedure Check_Dash (Context : in out Cairo_Context'Class; Width, Height : double) is
      Dashes : constant double_Array := (0.20,  -- ink
                                         0.05,  -- skip
                                         0.05,  -- ink
                                         0.05); -- skip
      Offset : constant double := -0.2;
   begin

      Normalize (Context, Width, Height);

      Set_Dash (Context, Dashes, Offset);

      Move_To (Context, 0.5, 0.1);
      Line_To (Context, 0.9, 0.9);
      Rel_Line_To (Context, -0.4, 0.0);
      Curve_To (Context, 0.2, 0.9, 0.2, 0.5, 0.5, 0.5);

      Stroke (Context);
   end Check_Dash;

   ----------------------
   -- Check_Long_Lines --
   ----------------------

   procedure Check_Long_Lines (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      Move_To (Context, 0.1, -50.0);
      Line_To (Context, 0.1,  50.0);
      Set_Source_RGB (Context, 1.0, 0.0, 0.0);
      Stroke (Context);

      Move_To (Context, 0.2, -60.0);
      Line_To (Context, 0.2,  60.0);
      Set_Source_RGB (Context, 1.0, 1.0, 0.0);
      Stroke (Context);

      Move_To (Context, 0.3, -70.0);
      Line_To (Context, 0.3,  70.0);
      Set_Source_RGB (Context, 0.0, 1.0, 0.0);
      Stroke (Context);

      Move_To (Context, 0.4, -80.0);
      Line_To (Context, 0.4,  80.0);
      Set_Source_RGB (Context, 0.0, 0.0, 1.0);
      Stroke (Context);
   end Check_Long_Lines;

   ------------------------------
   -- Check_Multi_Segment_Caps --
   ------------------------------

   procedure Check_Multi_Segment_Caps (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      Move_To (Context, 0.2, 0.3);
      Line_To (Context, 0.8, 0.3);

      Move_To (Context, 0.2, 0.5);
      Line_To (Context, 0.8, 0.5);

      Move_To (Context, 0.2, 0.7);
      Line_To (Context, 0.8, 0.7);

      Set_Line_Width (Context, 0.12);
      Set_Line_Cap  (Context, CAIRO_LINE_CAP_ROUND);
      Stroke (Context);
   end Check_Multi_Segment_Caps;

   --------------------------
   -- Check_Self_Intersect --
   --------------------------

   procedure Check_Self_Intersect (Context : in out Cairo_Context'Class; Width, Height : double) is
   begin
      Normalize (Context, Width, Height);

      Move_To (Context, 0.3, 0.3);
      Line_To (Context, 0.7, 0.3);

      Line_To (Context, 0.5, 0.3);
      Line_To (Context, 0.5, 0.7);

      Set_Line_Width (Context, 0.22);
      Set_Line_Cap (Context, CAIRO_LINE_CAP_ROUND);
      Set_Line_Join (Context, CAIRO_LINE_JOIN_ROUND);
      Stroke (Context);
   end Check_Self_Intersect;

   ------------------
   -- Register_All --
   ------------------

   procedure Register_All is
   begin
      Register (Check_Arc'Access,               "Arc");
      Register (Check_Arc_Negative'Access,      "Arc Negative");
      Register (Check_Clip'Access,              "Clip");
      Register (Check_Clip_Image'Access,        "Clip Image");
      Register (Check_Rectangle'Access,         "Rectangle");
      Register (Check_Curve_Rectangle'Access,   "Curve Rectangle");
      Register (Check_Curve_To'Access,          "Curve To");
      Register (Check_Fill_And_Stroke2'Access,  "Fill And Stroke2");
      Register (Check_Fill_And_Stroke'Access,   "Fill And Stroke");
      Register (Check_Gradient'Access,          "Gradient");
      Register (Check_Image'Access,             "Image");
      Register (Check_Image_Pattern'Access,     "Image Pattern");

      Register (Check_RSVG'Access,                  "RSVG");
      Register (Check_Operator_Add'Access,          "Operator ADD");
      Register (Check_Operator_Atop'Access,         "Operator ATOP");
      Register (Check_Operator_Atop_Reverse'Access, "Operator ATOP REVERSE");
      Register (Check_Operator_In'Access,           "Operator IN");
      Register (Check_Operator_In_Reverse'Access,   "Operator IN REVERSE");
      Register (Check_Operator_Out'Access,          "Operator OUT");
      Register (Check_Operator_Out_Reverse'Access,  "Operator OUT REVERSE");
      Register (Check_Operator_Over'Access,         "Operator OVER");
      Register (Check_Operator_Over_Reverse'Access, "Operator OVER REVERSE");
      Register (Check_Operator_Saturate'Access,     "Operator SATURATE");
      Register (Check_Operator_Xor'Access,          "Operator XOR");

      Register (Check_Path'Access,              "Path");
      Register (Check_Pattern_Fill'Access,      "Pattern Fill");
      Register (Check_Set_Line_Cap'Access,      "Set Line Cap");
      Register (Check_Set_Line_Join'Access,     "Set Line Join");
      Register (Check_Text_Align_Center'Access, "Text Align Center");
      Register (Check_Text'Access,              "Text");
      Register (Check_Text_Extents'Access,      "Text Extents");
      Register (Check_Clip_Rectangle'Access,    "Clip Rectangle");
      Register (Check_Dash'Access,              "Dash");
      Register (Check_Long_Lines'Access,        "Long Lines");
      Register (Check_Multi_Segment_Caps'Access, "Multi Segment Caps");
      Register (Check_Self_Intersect'Access,    "Self Intersect");
   end Register_All;

   -----------
   -- Check --
   -----------

   procedure Check (Index : Positive; Context : in out Cairo_Context'Class; Width, Height : double; DT : out Duration) is
      T0, T1 : Ada.Calendar.Time;
      use Ada.Calendar;
   begin
      Ada.Text_IO.Put ("   " & Checker_Slots (Index).Name (1 .. Checker_Slots (Index).Name_Last));
      Ada.Text_IO.Set_Col (25);
      T0 := Ada.Calendar.Clock;
      Checker_Slots (Index).Ref.all (Context, Width, Height);
      Show_Page (Context);
      T1 := Ada.Calendar.Clock;
      DT := T1 - T0;
      Ada.Text_IO.Put_Line (" " & Duration'Image (DT));
   end Check;

   ---------------
   -- Check_All --
   ---------------

   procedure Check_All is
      Width : constant := 3.0 * 72.0;
      Height : constant := 3.0 * 72.0;
      Surface : Cairo_Surface_Handle;
      Context : Cairo_Context_Handle;
      Total_DT : Duration := 0.0;
      DT : Duration;
   begin
      -- PDF (All tests in 1 file)
      Ada.Text_IO.Put_Line ("Check PDF");
      Surface := New_PDF_Surface ("test-snippets.pdf", Width_In_Points => Width, Height_In_Points => Height);
      Context := New_Context (Ref (Surface));

      Total_DT := 0.0;
      for I in 1 .. Checker_Slots_Last loop
         Save (Ref (Context).all);
         Check (I, Ref (Context).all, Width, Height, DT);
         Total_DT := Total_DT + DT;
         Restore (Ref (Context).all);
      end loop;
      Ada.Text_IO.Put_Line ("   Total                 " & Duration'Image (Total_DT));

      -- PS (All test in 1 file)
      Ada.Text_IO.Put_Line ("Check PS");
      Surface := New_PS_Surface ("test-snippets.ps", Width_In_Points => Width, Height_In_Points => Height);
      Context := New_Context (Ref (Surface));

      Total_DT := 0.0;
      for I in 1 .. Checker_Slots_Last loop
         Save (Ref (Context).all);
         Check (I, Ref (Context).all, Width, Height, DT);
         Total_DT := Total_DT + DT;
         Restore (Ref (Context).all);
      end loop;
      Ada.Text_IO.Put_Line ("   Total                 " & Duration'Image (Total_DT));

      -- PNG (1 file per test)
      Ada.Text_IO.Put_Line ("Check PNG");
      Total_DT := 0.0;
      for I in 1 .. Checker_Slots_Last loop
         declare
            Filename : constant String := "test-snippets-" & Normalize (Checker_Slots (I).Name (1 .. Checker_Slots (I).Name_Last)) & ".png";
            Status : Cairo_Status;
         begin
            Surface := New_Image_Surface (CAIRO_FORMAT_ARGB32, int (Width), int (Height));
            Context := New_Context (Ref (Surface));
            Check (I, Ref (Context).all, Width, Height, DT);
            Total_DT := Total_DT + DT;
            Write_To_PNG (Ref (Surface).all, Filename, Status);
         end;
      end loop;
      Ada.Text_IO.Put_Line ("   Total                 " & Duration'Image (Total_DT));

      -- SVG (1 file per test)
      Ada.Text_IO.Put_Line ("Check SVG");
      Total_DT := 0.0;
      for I in 1 .. Checker_Slots_Last loop
         declare
            Filename : constant String := "test-snippets-" & Normalize (Checker_Slots (I).Name (1 .. Checker_Slots (I).Name_Last)) & ".svg";
         begin
            Surface := New_SVG_Surface (Filename, Width, Height);
            Context := New_Context (Ref (Surface));
            Check (I, Ref (Context).all, Width, Height, DT);
            Total_DT := Total_DT + DT;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("   Total                 " & Duration'Image (Total_DT));

   end Check_All;

--   procedure Draw (Context : in out Cairo_Context'Class) is
--   begin
--     Set_Source_RGB (Context, 0.5, 0.5, 0.5);
--     Show_Page (Context);
--   end Draw;

   -----------------
   -- Draw_Smiley --
   -----------------

   procedure Draw_Smiley (Context : in out Cairo_Context'Class; Width, Height, Smile_Ratio : double) is
      STROKE_WIDTH : constant := 0.04;
      Size : double;
      Theta : constant double := Ada.Numerics.Pi / 4.0 * Smile_Ratio;
      DX : constant double := double (Sqrt (0.005) * Cos (Float (Theta)));
      DY : constant double := double (Sqrt (0.005) * Sin (Float (Theta)));
   begin
      --Print (Path);
      Save (Context);

      if Width > Height then
         Size := Height;
      else
         Size := Width;
      end if;

      Translate (Context, ((Width - Size) / 2.0, (Height - Size) / 2.0));
      Scale (Context, Size, Size);

      -- Fill face
      Arc (Context, (0.5, 0.5), 0.5 - STROKE_WIDTH, 0.0, 2.0 * Ada.Numerics.Pi);
      Set_Source_RGB (Context, 1.0, 1.0, 0.0);
      Fill_Preserve (Context);

      Set_Source_RGB (Context, 0.0, 0.0, 0.0);

      -- Stroke face
      Set_Line_Width (Context, STROKE_WIDTH / 2.0);
      Stroke (Context);

      -- Eyes
      Set_Line_Width (Context, STROKE_WIDTH);
      Arc (Context, (0.3, 0.4), STROKE_WIDTH, 0.0, 2.0 * Ada.Numerics.Pi);
      Fill (Context);
      Arc (Context, (0.7, 0.4), STROKE_WIDTH, 0.0, 2.0 * Ada.Numerics.Pi);
      Fill (Context);

      -- Mouth
      Move_To (Context, (0.35 - DX, 0.75 - DY));
      Curve_To (Context,
       (0.35 + DX, 0.75 + DY),
       (0.65 - DX, 0.75 + DY),
       (0.65 + DX, 0.75 - DY));
      --Copy_Path (Context, Path);
      --Print (Path);
      Stroke (Context);

      --Copy_Path (Context, Path);
      --Print (Path);

      Restore (Context);
   end Draw_Smiley;

   procedure Draw_Some_Pages (Surface : access Cairo_Surface'Class) is
      Context : Cairo_Context_Handle;
      NUM_FRAMES : constant := 5;
   begin
      Context := New_Context (Surface);

      for I in 0 .. NUM_FRAMES - 1 loop
         Draw_Smiley (Ref (Context).all, WIDTH_IN_POINTS, HEIGHT_IN_POINTS, double (I) / double (NUM_FRAMES));

         -- Duplicate the last frame onto another page. (This is just a
         -- way to sneak cairo_copy_page into the test).

         if I = (NUM_FRAMES - 1) then
            Copy_Page (Ref (Context).all);
         end if;

         Show_Page (Ref (Context).all);
      end loop;
   end Draw_Some_Pages;

   ----------
   -- Main --
   ----------

   procedure Main is
      Surface : Cairo_Surface_Handle;
      --Context : Cairo_Context_Handle;
      --Options1 : Cairo_Font_Options;
      --Options2 : Cairo_Font_Options;
      File : Ada.Streams.Stream_IO.File_Type;
      Status : Cairo_Status;
      use Ada.Streams.Stream_IO;

      --use Interfaces.C;
   begin
--      Gtk.Main.Init;
      RSVG.Init;
      Ada.Text_IO.Put_Line ("---------------------------");
      Ada.Text_IO.Put_Line ("Version:" & int'Image (Cairo.Version));
      Ada.Text_IO.Put_Line ("Version String:" & Cairo.Version_String);
--        for Status in Cairo.Status'Range loop
--           Ada.Text_IO.Put_Line (Cairo.Status'Image (Status) & ":" & Cairo.To_String(Status));
--        end loop;
--        Cairo.Debug_Reset_Static_Data;
      Ada.Text_IO.Put_Line ("---------------------------");
      Surface := New_PDF_Surface ("test-cairo.pdf", Width_In_Points => 3.0 * 72.0, Height_In_Points => 3.0 * 72.0);
--      Ada.Text_IO.Put_Line ("Surface status:" & Cairo.Status'Image(Get_Status (Ref (Surface).all)));
      if Get_Status (Ref (Surface).all) /= CAIRO_STATUS_SUCCESS then
         return;
      end if;
--      Ada.Text_IO.Put_Line ("Surface type:" & Cairo.Surface_Type'Image(Get_Type(Ref (Surface).all)));
      --Context := New_Context (Ref (Surface));
      Draw_Some_Pages (Ref (Surface));

      Ada.Text_IO.Put_Line ("---------------------------");
      Create (File, Name => "test-cairo-stream.pdf");
      Surface := New_PDF_Surface_For_Stream (Stream (File),  Width_In_Points => 3.0 * 72.0, Height_In_Points => 3.0 * 72.0);
      Draw_Some_Pages (Ref (Surface));
      Finish (Ref (Surface).all);
      --Reset (Surface);
      Close (File);

      Ada.Text_IO.Put_Line ("---------------------------");
      Surface := New_PS_Surface ("test-cairo.ps", Width_In_Points => 3.0 * 72.0, Height_In_Points => 3.0 * 72.0);
      if Get_Status (Ref (Surface).all) /= CAIRO_STATUS_SUCCESS then
         return;
      end if;
      Draw_Some_Pages (Ref (Surface));

      Ada.Text_IO.Put_Line ("---------------------------");
      Surface := New_SVG_Surface ("test-cairo.svg", Width_In_Points => 3.0 * 72.0, Height_In_Points => 3.0 * 72.0);
      if Get_Status (Ref (Surface).all) /= CAIRO_STATUS_SUCCESS then
         return;
      end if;
      Draw_Some_Pages (Ref (Surface));

      Ada.Text_IO.Put_Line ("---------------------------");
      Surface := New_Image_Surface (CAIRO_FORMAT_ARGB32, 256, 256);
      Draw_Some_Pages (Ref (Surface));
      Write_To_PNG (Ref (Surface).all, "test-cairo.png", Status);
      if Status /= CAIRO_STATUS_SUCCESS then
         Ada.Text_IO.Put_Line ("Write_To_PNG status:" & Cairo_Status'Image (Status));
      end if;

      Register_All;
      Check_All;


      --Reset (Surface);
      --Draw (Ref (Context).all);
      --Set_Source_RGB (Ref (Context).all, 0.5, 0.5, 0.5);
      --Show_Page (Ref (Context).all);
      --Rectangle (Ref (Context).all, );
      --Save (Ref (Context).all);

--      if False then
--         Ada.Text_IO.Put_Line ("---------------------------");
--         Print (Options1);
--         Ada.Text_IO.Put_Line ("Options1 = Options2 :" & Boolean'Image (Options1 = Options2));
--         Set_Antialias (Options1, ANTIALIAS_GRAY);
--         Set_Subpixel_Order (Options1, SUBPIXEL_ORDER_RGB);
--         Set_Hint_Style (Options1, HINT_STYLE_MEDIUM);
--         Set_Hint_Metrics (Options1, HINT_METRICS_ON);
--         Print (Options1);
--         Ada.Text_IO.Put_Line ("Options1 = Options2 :" & Boolean'Image (Options1 = Options2));
--         Options1 := Options2;
--         Print (Options1);
--         Ada.Text_IO.Put_Line ("Auto set");
--         Options1 := Options1;
--         Print (Options1);
--         Ada.Text_IO.Put_Line ("Options1 = Options2 :" & Boolean'Image (Options1 = Options2));
--
--         Ada.Text_IO.Put_Line ("---------------------------");
--         declare
--            Versions : constant SVG_Version_Array := Get_SVG_Versions;
--         begin
--            Ada.Text_IO.Put ("Supported SVG versions:");
--            for V in Versions'Range loop
--               Ada.Text_IO.Put (" " & SVG_Version'Image (Versions(V)));
--            end loop;
--            Ada.Text_IO.New_Line;
--         end;
--         Ada.Text_IO.Put_Line ("---------------------------");
--      end if;

   end Main;

end Cairo_Test_Snippets_Pkg;
