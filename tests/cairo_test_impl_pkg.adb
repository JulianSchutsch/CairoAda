with Cairo.Context; use Cairo.Context;
with Cairo.Surface; use Cairo.Surface;
with Cairo.Surface.Image; use Cairo.Surface.Image;
with Cairo; use Cairo;
with Ada.Text_IO;

package body Cairo_Test_Impl_Pkg is
   procedure Main is
      Surface : Cairo_Surface_Handle;
      Context : Cairo_Context_Handle;
   begin
      Ada.Text_IO.Put_Line ("1>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Context := Cairo_Context_Null_Handle;
      Ada.Text_IO.Put_Line ("2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Surface := New_Image_Surface (CAIRO_FORMAT_ARGB32, 100, 100);
      Ada.Text_IO.Put_Line ("3>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Context := New_Context (Ref (Surface));
      Ada.Text_IO.Put_Line ("4>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Context := Context;
      Ada.Text_IO.Put_Line ("5>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Context := New_Context (Ref (Surface));
      Ada.Text_IO.Put_Line ("6>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
      Context := Cairo_Context_Null_Handle;
      Ada.Text_IO.Put_Line ("7>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
   end Main;
end Cairo_Test_Impl_Pkg;
