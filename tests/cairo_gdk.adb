with Gtk.Main; use Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
--with Glib; use Glib;
with Interfaces.C;
with Ada.Numerics; use Ada.Numerics;
with Ada.Numerics.Generic_Elementary_Functions;
with Cairo.Context; use Cairo.Context;
with Cairo.Surface; use Cairo.Surface;
use Cairo;
with Gdk.Cairo;
with Ada.Text_IO;
with Ada.Calendar;

package body Cairo_Gdk is

   package C renames Interfaces.C;
   package EF is new Ada.Numerics.Generic_Elementary_Functions (C.double);
   use EF;

   package Widget_Handler is new Gtk.Handlers.Callback
     (Gtk.Widget.Gtk_Widget_Record);

   package Expose_Handler is new Gtk.Handlers.Return_Callback
     (Gtk.Widget.Gtk_Widget_Record, Boolean);

   G_Counter : Integer := 0;

   ---------------
   -- Oval Path --
   ---------------

   procedure Oval_Path
     (Context : in out Cairo_Context'Class;
      XC, YC : C.double;
      XR, YR : C.double)
   is
      use C;
   begin
      Save (Context);
      Translate (Context, XC, YC);
      Scale (Context, 1.0, YR / XR);
      Move_To (Context, XR, 0.0);
      Arc (Context, 0.0, 0.0,
           XR, 0.0, 2.0 * Pi);
      Close_Path (Context);
      Restore (Context);
   end Oval_Path;

   -----------------
   -- Fill_Checks --
   -----------------

   procedure Fill_Checks
     (Context : in out Cairo_Context'Class;
      X, Y : C.int;
      Width, Height : C.int)
   is
      CHECK_SIZE : constant := 32;
      I, J : C.int;
      use C;
--      function "+"(X : C.int) return String renames C.int'Image;
   begin

      -- Fill background with a dark grey

      Rectangle (Context, C.double (X), C.double (Y), C.double (Width), C.double (Height));
      Set_Source_RGB (Context, 0.4, 0.4, 0.4);
      Fill (Context);

      -- Fill squares with a light grey

      J := X / CHECK_SIZE;
      while J < Height loop
         I := Y / CHECK_SIZE;
         while I < Width loop
            if ((I + J) / CHECK_SIZE) mod 2 = 0 then
               Rectangle (Context, C.double (I), C.double (J), C.double (CHECK_SIZE), C.double (CHECK_SIZE));
            end if;
            I := I + CHECK_SIZE;
         end loop;
         J := J + CHECK_SIZE;
      end loop;

      Set_Source_RGB (Context, 0.7, 0.7, 0.7);
      Fill (Context);
   end Fill_Checks;

   -------------------
   -- Draw_3Circles --
   -------------------

   procedure Draw_3Circles
     (Context : in out Cairo_Context'Class;
      XC, YC : C.double;
      Radius : C.double;
      Alpha : C.double)
   is
      use C;
      Subradius : constant C.double := Radius * (2.0 / 3.0 - 0.1);
   begin
      -- Red circle
      Set_Source_RGBA (Context, 1.0, 0.0, 0.0, Alpha);
      Oval_Path (Context,
                 XC + Radius / 3.0 * Cos (Pi * (0.5)),
                 YC - Radius / 3.0 * Sin (Pi * (0.5)),
                 Subradius, Subradius);
      Fill (Context);

      -- Green circle
      Set_Source_RGBA (Context, 0.0, 1.0, 0.0, Alpha);
      Oval_Path (Context,
                 XC + Radius / 3.0 * Cos (Pi * (0.5 + 2.0/0.3)),
                 YC - Radius / 3.0 * Sin (Pi * (0.5 + 2.0/0.3)),
                 Subradius, Subradius);
      Fill (Context);

      -- Blue circle
      Set_Source_RGBA (Context, 0.0, 0.0, 1.0, Alpha);
      Oval_Path (Context,
                 XC + Radius / 3.0 * Cos (Pi * (0.5 + 4.0/0.3)),
                 YC - Radius / 3.0 * Sin (Pi * (0.5 + 4.0/0.3)),
                 Subradius, Subradius);
      Fill (Context);
   end Draw_3Circles;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Context : in out Cairo_Context'Class;
      Width, Height : C.int)
   is
      use C;
      Overlay, Punch, Circles : Cairo_Surface_Handle;
      Overlay_Ctx, Punch_Ctx, Circles_Ctx : Cairo_Context_Handle;
      Radius : constant C.double := 0.5 * C.double (C.int'Min (Width, Height) - 10);
      XC : constant C.double := C.double (Width) / 2.0;
      YC : constant C.double := C.double (Height) / 2.0;
   begin

      -- Fill the background
      Overlay := Create_Similar (Ref (Get_Target (Context)).all,
                                 CAIRO_CONTENT_COLOR_ALPHA,
                                 Width, Height);
      if not Is_Set (Overlay) then
         return;
      end if;

      Punch := Create_Similar (Ref (Get_Target (Context)).all,
                               CAIRO_CONTENT_ALPHA,
                               Width, Height);
      if not Is_Set (Punch) then
         return;
      end if;

      Circles := Create_Similar (Ref (Get_Target (Context)).all,
                                 CAIRO_CONTENT_COLOR_ALPHA,
                                 Width, Height);
      if not Is_Set (Circles) then
         return;
      end if;

      Fill_Checks (Context, 0, 0, Width, Height);

      -- Draw a black circle on the overlay
      Overlay_Ctx := New_Context (Ref (Overlay));
      Set_Source_RGB (Ref (Overlay_Ctx).all, 0.0, 0.0, 0.0);
      Oval_Path (Ref (Overlay_Ctx).all, XC, YC, Radius, Radius);
      Fill (Ref (Overlay_Ctx).all);

      -- Draw 3 circles to the punch surface, then cut
      -- that out of the main circle in the overlay
      Punch_Ctx := New_Context (Ref (Punch));
      Draw_3Circles (Ref (Punch_Ctx).all, XC, YC, Radius, 1.0);

      Set_Operator (Ref (Overlay_Ctx).all, CAIRO_OPERATOR_DEST_OUT);
      Set_Source_Surface (Ref (Overlay_Ctx).all, Ref (Punch), 0.0, 0.0);
      Paint (Ref (Overlay_Ctx).all);

      -- Now draw the 3 circles in a subgroup again
      -- at half intensity, and use OperatorAdd to join up
      -- without seams.
      Circles_Ctx := New_Context (Ref (Circles));

      Set_Operator (Ref (Circles_Ctx).all, CAIRO_OPERATOR_OVER);
      Draw_3Circles (Ref (Circles_Ctx).all, XC, YC, Radius, 0.5);

      Set_Operator (Ref (Overlay_Ctx).all, CAIRO_OPERATOR_ADD);
      Set_Source_Surface (Ref (Overlay_Ctx).all, Ref (Circles), 0.0, 0.0);
      Paint (Ref (Overlay_Ctx).all);

      Set_Source_Surface (Context, Ref (Overlay), 0.0, 0.0);
      Paint (Context);

   end Draw;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end On_Destroy;

   ---------------
   -- On_Expose --
   ---------------

   function On_Expose (Widget : access Gtk_Widget_Record'Class) return Boolean is
      Context : Cairo.Context.Cairo_Context_Handle;
      use Ada.Calendar;
      T0 : Time;
      DT1, DT2 : Duration;
   begin
      T0 := Clock;
      Context := Gdk.Cairo.Create (Get_Window (Widget));
      DT1 := Clock - T0;
      G_Counter := G_Counter + 1;
      Ada.Text_IO.Put_Line ("Surface:" & Cairo_Surface_Type'Image (Get_Type (Ref (Get_Target (Ref (Context).all)).all)) & Integer'Image (G_Counter));
      Draw (Ref (Context).all,
            C.int (Get_Allocation_Width (Widget)),
            C.int (Get_Allocation_Height (Widget)));
      DT2 := Clock - T0;
      Ada.Text_IO.Put_Line ("   " & Duration'Image (DT1) & Duration'Image (DT2));
      return False;
   end On_Expose;

   ----------
   -- Main --
   ----------

   procedure Main is
      Window : Gtk_Window;
      Drawing_Area : Gtk_Drawing_Area;
   begin
      Gtk.Main.Init;
      Gtk_New (Window, Gtk.Enums.Window_Toplevel);
      Set_Default_Size (Window, 400, 400);
      Set_Title (Window, "cairo: Knockout Groups");

      Gtk_New (Drawing_Area);
      Add (Window, Drawing_Area);

      Expose_Handler.Connect
        (Drawing_Area, "expose_event",
         Expose_Handler.To_Marshaller (On_Expose'Access));

      Widget_Handler.Connect
        (Window, "destroy",
         Widget_Handler.To_Marshaller (On_Destroy'Access));

      Show_All (Window);
      Gtk.Main.Main;
   end Main;

end Cairo_Gdk;
