with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;
with Cairo.Font_Face; use Cairo.Font_Face;
with Cairo.Font_Face.User; use Cairo.Font_Face.User;
with Cairo.Scaled_Font; use Cairo.Scaled_Font;
with Cairo.Context; use Cairo.Context;
with Cairo.Surface; use Cairo.Surface;
with Cairo.Surface.Image; use Cairo.Surface.Image;
use Cairo;
--with Cairo.Font_Face.Debug; use Cairo.Font_Face.Debug;

--with Cairo.Font_Face.Toy; use Cairo.Font_Face.Toy;

package body Cairo_Test_User_Fonts_Pkg is

   G_ROTATED   : constant Boolean := False;
   G_BORDER    : constant := 10.0;
   G_TEXT_SIZE : constant := 64.0;
   G_WIDTH     : constant := G_TEXT_SIZE * 15.0 + 2.0*G_BORDER;
   G_HEIGHT    : constant := (G_TEXT_SIZE + 2.0*G_BORDER)*2.0;
   G_TEXT      : constant String := "geez... cairo user-font";

   type Code is mod 2**8;
   END_GLYPH : constant Code := 0;
   STROKE    : constant Code := 126;
   CLOSE     : constant Code := 127;
   type Code_Array is array (Natural range <>) of Code;

-- Simple glyph definition: 1 - 15 means lineto (or moveto for first
-- point) for one of the points on this grid:
--
--       1  2  3
--       4  5  6
--       7  8  9
--  ----10 11 12----(baseline)
--      13 14 15

   type Scaled_Font_Glyph is record
      UCS4 : unsigned_long;
      Width : double;
      Data : Code_Array (1 .. 16);
   end record;
   type Scaled_Font_Glyph_Array is array (unsigned_long range <>) of Scaled_Font_Glyph;

   function To_Glyph (C : Character; Width : double; Data : Code_Array) return Scaled_Font_Glyph;
   function To_Glyph
     (C : Character;
      Width : double;
      Data : Code_Array)
      return Scaled_Font_Glyph
   is
      Result : Scaled_Font_Glyph;
   begin
      Result.UCS4 := unsigned_long (Character'Pos (C));
      Result.Width := Width;
      Result.Data := (others => 0);
      Result.Data (1 .. Data'Length) := Data;
      return Result;
   end To_Glyph;

   type Test_Data is new Cairo.Font_Face.User.Cairo_User_Scaled_Font_Data with record
      Glyphs : Scaled_Font_Glyph_Array (0 .. 18);
   end record;

   overriding
   procedure Initialize (Data : in out Test_Data);
   overriding
   procedure Finalize (Data : in out Test_Data);

   overriding
   procedure Init
     (Data : in out Test_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Context : Cairo_Context_Handle;
      Extents : in out Cairo_Font_Extents;
      Status : out Cairo_Status);

   overriding
   procedure Unicode_To_Glyph
     (Data : in out Test_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Unicode : unsigned_long;
      Glyph_Index : out unsigned_long;
      Status : out Cairo_Status);

   overriding
   procedure Render_Glyph
     (Data : in out Test_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Glyph : unsigned_long;
      Context : Cairo_Context_Handle;
      Extents : in out Cairo_Text_Extents;
      Status : out Cairo_Status);


   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Data : in out Test_Data) is
      pragma Unreferenced (Data);
   begin
      Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Initialize");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Data : in out Test_Data) is
      pragma Unreferenced (Data);
   begin
      Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Finalize");
   end Finalize;

   ----------
   -- Init --
   ----------

   procedure Init
     (Data : in out Test_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Context : Cairo_Context_Handle;
      Extents : in out Cairo_Font_Extents;
      Status : out Cairo_Status)
   is
      pragma Unreferenced (Scaled_Font, Context);
   begin
      Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Init");
      Data.Glyphs :=
        (To_Glyph (Character'Val (0), 1.0, (1 => END_GLYPH)),
         To_Glyph (' ',  1.0, (1 => END_GLYPH)),
         To_Glyph ('-',  2.0, (7, 8, STROKE, END_GLYPH)),
         To_Glyph ('.',  1.0, (10, 10, STROKE, END_GLYPH)),
         To_Glyph ('a',  3.0, (4, 6, 12, 10, 7, 9, STROKE, END_GLYPH)),
         To_Glyph ('c',  3.0, (6, 4, 10, 12, STROKE, END_GLYPH)),
         To_Glyph ('e',  3.0, (12, 10, 4, 6, 9, 7, STROKE, END_GLYPH)),
         To_Glyph ('f',  3.0, (3, 2, 11, STROKE, 4, 6, STROKE, END_GLYPH)),
         To_Glyph ('g',  3.0, (12, 10, 4, 6, 15, 13, STROKE, END_GLYPH)),
         To_Glyph ('h',  3.0, (1, 10, STROKE, 7, 5, 6, 12, STROKE, END_GLYPH)),
         To_Glyph ('i',  1.0, (1, 1, STROKE, 4, 10, STROKE, END_GLYPH)),
         To_Glyph ('l',  1.0, (1, 10, STROKE, END_GLYPH)),
         To_Glyph ('n',  3.0, (10, 4, STROKE, 7, 5, 6, 12, STROKE, END_GLYPH)),
         To_Glyph ('o',  3.0, (4, 10, 12, 6, CLOSE, END_GLYPH)),
         To_Glyph ('r',  3.0, (4, 10, STROKE, 7, 5, 6, STROKE, END_GLYPH)),
         To_Glyph ('s',  3.0, (6, 4, 7, 9, 12, 10, STROKE, END_GLYPH)),
         To_Glyph ('t',  3.0, (2, 11, 12, STROKE, 4, 6, STROKE, END_GLYPH)),
         To_Glyph ('u',  3.0, (4, 10, 12, 6, STROKE, END_GLYPH)),
         To_Glyph ('z',  3.0, (4, 6, 10, 12, STROKE, END_GLYPH))
        );


      Extents.Ascent := 0.75;
      Extents.Descent := 0.25;

      Status := CAIRO_STATUS_SUCCESS;
   end Init;

   ----------------------
   -- Unicode_To_Glyph --
   ----------------------

   procedure Unicode_To_Glyph
     (Data : in out Test_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Unicode : unsigned_long;
      Glyph_Index : out unsigned_long;
      Status : out Cairo_Status)
   is
      pragma Unreferenced (Scaled_Font);
   begin
--      Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Unicode_To_Glyph(" & Unicode'Img & ")");
      for I in Data.Glyphs'Range loop
         if Data.Glyphs (I).UCS4 = Unicode then
            Glyph_Index := I;
            Status := CAIRO_STATUS_SUCCESS;
            return;
         end if;
      end loop;

      Glyph_Index := 0;
      Status := CAIRO_STATUS_SUCCESS;

   end Unicode_To_Glyph;

   ------------------
   -- Render_Glyph --
   ------------------

   procedure Render_Glyph
     (Data : in out Test_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Glyph : unsigned_long;
      Context : Cairo_Context_Handle;
      Extents : in out Cairo_Text_Extents;
      Status : out Cairo_Status)
   is
      pragma Unreferenced (Scaled_Font);
      C : Code;
      Q, R : Code;
      X, Y : double;
   begin
--      Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Render_Glyph(" & Glyph'Img & ")");
      Extents.X_Advance := Data.Glyphs (Glyph).Width / 4.0;

      Ref (Context).Set_Line_Width (0.1);
      Ref (Context).Set_Line_Join (CAIRO_LINE_JOIN_ROUND);
      Ref (Context).Set_Line_Cap (CAIRO_LINE_CAP_ROUND);

      for I in Data.Glyphs (Glyph).Data'Range loop
         C := Data.Glyphs (Glyph).Data (I);
         case C is
            when END_GLYPH =>
               exit;

            when STROKE =>
--               Ada.Text_IO.Put_Line ("   New_Sub_Path");
               Ref (Context).New_Sub_Path;

            when CLOSE =>
--               Ada.Text_IO.Put_Line ("   Close_Path");
               Ref (Context).Close_Path;

            when others =>
               Q := (C - 1) / 3;
               R := (C - 1) rem 3;
               X := double (R) / 4.0 + 0.125;
               Y := double (Q) / 5.0 + 0.4 - 1.0;
--               Ada.Text_IO.Put_Line ("   Line_To (" & X'Img & Y'Img & ")");
               Ref (Context).Line_To (X, Y);

         end case;
      end loop;
      Ref (Context).Stroke;
      Status := CAIRO_STATUS_SUCCESS;
   end Render_Glyph;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Context : in out Cairo_Context'Class;
      Width : int;
      Height : int)
   is
      pragma Unreferenced (Width, Height);
      Font_Face : Cairo_Font_Face_Handle;
      Font_Extents : Cairo_Font_Extents;
      Text_Extents : Cairo_Text_Extents;
   begin
      --Ada.Text_IO.Put_Line ("1>>>>>>>> FF:" & Img (Font_Face));
      --Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Draw");
      Font_Face := New_User_Font_Face;
--      Font_Face := New_Toy_Font_Face ("Arial", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);

      --Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Draw 111");
      --Ada.Text_IO.Put_Line ("2>>>>>>>> FF:" & Img (Font_Face));

      Set_Data (Cairo_User_Font_Face_Ref (Ref (Font_Face)).all,
                Data => new Test_Data,
                Auto_Destroy => True);
      --Ada.Text_IO.Put_Line ("Cairo_Test_User_Fonts.Draw 222");
      --Ada.Text_IO.Put_Line ("3>>>>>>>> FF:" & Img (Font_Face));

      Context.Set_Source_RGB (1.0, 1.0, 1.0);
      Context.Paint;
      --Ada.Text_IO.Put_Line ("4>>>>>>>> FF:" & Img (Font_Face));

      if G_ROTATED then
         Context.Translate (G_TEXT_SIZE, 0.0);
         Context.Rotate (0.6);
      end if;

      --Ada.Text_IO.Put_Line ("5>>>>>>>> FF:" & Img (Font_Face));
      Context.Set_Font_Face (Font_Face => Ref (Font_Face));
      --Ada.Text_IO.Put_Line ("6>>>>>>>> FF:" & Img (Font_Face));
      Context.Set_Font_Size (G_TEXT_SIZE);
      --Ada.Text_IO.Put_Line ("7>>>>>>>> FF:" & Img (Font_Face));

      Font_Extents := Context.Get_Font_Extents;
      --Ada.Text_IO.Put_Line ("8>>>>>>>> FF:" & Img (Font_Face));
      Ada.Text_IO.Put_Line ("FE.Ascent:" & Font_Extents.Ascent'Img);
      Ada.Text_IO.Put_Line ("FE.Descent:" & Font_Extents.Descent'Img);
      Ada.Text_IO.Put_Line ("FE.Height:" & Font_Extents.Height'Img);
      Ada.Text_IO.Put_Line ("FE.Max_X_Advance:" & Font_Extents.Max_X_Advance'Img);
      Ada.Text_IO.Put_Line ("FE.Max_Y_Advance:" & Font_Extents.Max_Y_Advance'Img);
      Text_Extents := Context.Get_Text_Extents (G_TEXT);
      Ada.Text_IO.Put_Line ("TE.X_Bearing;" & Text_Extents.X_Bearing'Img);
      Ada.Text_IO.Put_Line ("TE.Y_Bearing;" & Text_Extents.Y_Bearing'Img);
      Ada.Text_IO.Put_Line ("TE.Width:" & Text_Extents.Width'Img);
      Ada.Text_IO.Put_Line ("TE.Height:" & Text_Extents.Height'Img);
      Ada.Text_IO.Put_Line ("TE.X_Advance;" & Text_Extents.X_Advance'Img);
      Ada.Text_IO.Put_Line ("TE.Y_Advance;" & Text_Extents.Y_Advance'Img);


      -- Logical boundaries in red
      Context.Move_To (0.0, G_BORDER);
      Context.Rel_Line_To (G_WIDTH, 0.0);
      Context.Move_To (0.0, G_BORDER + Font_Extents.Ascent);
      Context.Rel_Line_To (G_WIDTH, 0.0);
      Context.Move_To (0.0, G_BORDER + Font_Extents.Ascent + Font_Extents.Descent);
      Context.Rel_Line_To (G_WIDTH, 0.0);
      Context.Move_To (G_BORDER, 0.0);
      Context.Rel_Line_To (0.0, 2.0*G_BORDER + G_TEXT_SIZE);
      Context.Move_To (G_BORDER + Text_Extents.X_Advance, 0.0);
      Context.Rel_Line_To (0.0, 2.0*G_BORDER + G_TEXT_SIZE);
      Context.Set_Source_RGB (1.0, 0.0, 0.0);
      Context.Set_Line_Width (2.0);
      Context.Stroke;

      -- Ink boundaries in green
      Context.Rectangle (G_BORDER + Text_Extents.X_Bearing,
                         G_BORDER + Font_Extents.Ascent + Text_Extents.Y_Bearing,
                         Text_Extents.Width,
                         Text_Extents.Height);
      Context.Set_Source_RGB (0.0, 1.0, 0.0);
      Context.Set_Line_Width (2.0);
      Context.Stroke;

      --Ada.Text_IO.Put_Line ("9>>>>>>>> FF:" & Img (Font_Face));
      -- Text in black
      Context.Set_Source_RGB (0.0, 0.0, 0.0);
      Context.Move_To (G_BORDER, G_BORDER + Font_Extents.Ascent);
      --Ada.Text_IO.Put_Line ("0>>>>>>>> FF:" & Img (Font_Face));
      Context.Show_Text (G_TEXT);
      --Ada.Text_IO.Put_Line ("1>>>>>>>> FF:" & Img (Font_Face));

      -- Filled version of text in blue
      Context.Set_Source_RGB (0.0, 0.0, 1.0);
      Context.Move_To (G_BORDER, G_BORDER + Font_Extents.Height + 2.0*G_BORDER + Font_Extents.Ascent);
      Context.Text_Path (G_TEXT);
      --Ada.Text_IO.Put_Line ("2>>>>>>>> FF:" & Img (Font_Face));
      Context.Fill;
      --Ada.Text_IO.Put_Line ("3>>>>>>>> FF:" & Img (Font_Face));

   end Draw;

   procedure Main is
   begin
      declare
         Surface : Cairo_Surface_Handle;
         Context : Cairo_Context_Handle;
         Status : Cairo_Status;
      begin
         Surface := New_Image_Surface (CAIRO_FORMAT_ARGB32, int (G_WIDTH), int (G_HEIGHT));
         Context := New_Context (Ref (Surface));
         Draw (Ref (Context).all, int (G_WIDTH), int (G_HEIGHT));
         Write_To_PNG (Ref (Surface).all, "test-user-fonts.png", Status);
         if Status /= CAIRO_STATUS_SUCCESS then
            Ada.Text_IO.Put_Line ("Write_To_PNG status:" & Cairo_Status'Image (Status));
         end if;
      end;
      Debug_Reset_Static_Data;
      --Ada.Text_IO.Put_Line ("End of Main");
   end Main;

end Cairo_Test_User_Fonts_Pkg;
