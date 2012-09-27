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
--with Ada.Tags;
with Ada.Unchecked_Conversion;

--  with Ada.Text_IO; -- Debug
--  with Cairo.Surface.Debug; use Cairo.Surface.Debug;

package body Cairo.Surface is

   Surface_Allocators : array (Cairo_Surface_Type) of Surface_Allocator;

   function Allocate_Ref (Ptr : Surface_Ptr) return Cairo_Surface_Ref;

   Ada_Surface_Key : aliased Void;
   -- The key used to attach the Ada created surface to the C surface

   pragma Warnings (Off);
   --  Suppress possible aliasing problem warning
   function To_Ref is new Ada.Unchecked_Conversion
     (Cairo_User_Data, Cairo_Surface_Ref);
   pragma Warnings (On);

   function To_User_Data is new Ada.Unchecked_Conversion
     (Cairo_Surface_Ref, Cairo_User_Data);

   procedure Destroy_Surface (Data : Cairo_User_Data);
   pragma Convention (C, Destroy_Surface);

   ---------------------
   -- Destroy_Surface --
   ---------------------

   procedure Destroy_Surface (Data : Cairo_User_Data) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Cairo_Surface'Class,
         Cairo_Surface_Ref);
      Ref : Cairo_Surface_Ref := To_Ref (Data);
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Surface.Destroy_Surface Ref:" & Img
      --(Ref));
      Free (Ref);
   end Destroy_Surface;

   --------------------
   -- Create_Similar --
   --------------------

   function Create_Similar
     (Other   : Cairo_Surface'Class;
      Content : Cairo_Content;
      Width   : int;
      Height  : int)
      return Cairo_Surface_Handle
   is
      Ptr : Surface_Ptr;
   begin
      Ptr := cairo_surface_create_similar (Other.Ptr, Content, Width, Height);
      return To_Handle (Ptr, Is_Referenced => True);
   end Create_Similar;

   -------------------
   -- Set_User_Data --
   -------------------

   procedure Set_User_Data
     (Surface   : in out Cairo_Surface'Class;
      Key       : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy   : Cairo_Destroy_Func;
      Status    : out Cairo_Status)
   is
   begin
      Status :=
         cairo_surface_set_user_data (Surface.Ptr, Key, User_Data, Destroy);
   end Set_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Surface : Cairo_Surface'Class;
      Key     : Cairo_User_Data_Key)
      return Cairo_User_Data
   is
   begin
      return cairo_surface_get_user_data (Surface.Ptr, Key);
   end Get_User_Data;

   ------------
   -- Finish --
   ------------

   procedure Finish (Surface : in out Cairo_Surface'Class) is
   begin
      cairo_surface_finish (Surface.Ptr);
   end Finish;

   -----------
   -- Flush --
   -----------

   procedure Flush (Surface : in out Cairo_Surface'Class) is
   begin
      cairo_surface_flush (Surface.Ptr);
   end Flush;

   ----------------
   -- Mark_Dirty --
   ----------------

   procedure Mark_Dirty (Surface : in out Cairo_Surface'Class) is
   begin
      cairo_surface_mark_dirty (Surface.Ptr);
   end Mark_Dirty;

   --------------------------
   -- Mark_Dirty_Rectangle --
   --------------------------

   procedure Mark_Dirty_Rectangle
     (Surface       : in out Cairo_Surface'Class;
      X, Y          : int;
      Width, Height : int)
   is
   begin
      cairo_surface_mark_dirty_rectangle (Surface.Ptr, X, Y, Width, Height);
   end Mark_Dirty_Rectangle;

   -----------------------
   -- Set_Device_Offset --
   -----------------------

   procedure Set_Device_Offset
     (Surface : in out Cairo_Surface'Class;
      Offset  : Cairo_Tuple)
   is
   begin
      cairo_surface_set_device_offset (Surface.Ptr, Offset.X, Offset.Y);
   end Set_Device_Offset;

   -----------------------------
   -- Set_Fallback_Resolution --
   -----------------------------

   procedure Set_Fallback_Resolution
     (Surface           : in out Cairo_Surface'Class;
      X_Pixels_Per_Inch : double;
      Y_Pixels_Per_Inch : double)
   is
   begin
      cairo_surface_set_fallback_resolution
        (Surface.Ptr,
         X_Pixels_Per_Inch,
         Y_Pixels_Per_Inch);
   end Set_Fallback_Resolution;

   ---------------
   -- Copy_Page --
   ---------------

   procedure Copy_Page (Surface : in out Cairo_Surface'Class) is
   begin
      cairo_surface_copy_page (Surface.Ptr);
   end Copy_Page;

   ---------------
   -- Show_Page --
   ---------------

   procedure Show_Page (Surface : in out Cairo_Surface'Class) is
   begin
      cairo_surface_show_page (Surface.Ptr);
   end Show_Page;

   ------------------
   -- Write_To_PNG --
   ------------------

   procedure Write_To_PNG
     (Surface  : Cairo_Surface'Class;
      Filename : String;
      Status   : out Cairo_Status)
   is
      C_Filename : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      Status :=
         cairo_surface_write_to_png
           (Surface.Ptr,
            To_Chars_Ptr (C_Filename'Address));
   end Write_To_PNG;

   -------------------------
   -- Write_To_PNG_Stream --
   -------------------------

   procedure Write_To_PNG_Stream
     (Surface : Cairo_Surface'Class;
      Stream  : access Ada.Streams.Root_Stream_Type'Class;
      Status  : out Cairo_Status)
   is
   begin
      Status :=
         cairo_surface_write_to_png_stream
           (Surface.Ptr,
            Stream_Write'Access,
            To_Closure (Stream_Access (Stream)));
   end Write_To_PNG_Stream;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Surface : Cairo_Surface'Class) return Cairo_Status is
   begin
      return cairo_surface_status (Surface.Ptr);
   end Get_Status;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Surface : Cairo_Surface'Class)
      return Cairo_Surface_Type
   is
   begin
      return cairo_surface_get_type (Surface.Ptr);
   end Get_Type;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
     (Surface : Cairo_Surface'Class)
      return Cairo_Content
   is
   begin
      return cairo_surface_get_content (Surface.Ptr);
   end Get_Content;

   ----------------------
   -- Get_Font_Options --
   ----------------------

   function Get_Font_Options
     (Surface : Cairo_Surface'Class)
      return Cairo_Font_Options
   is
      Ptr : constant Font_Options_Ptr := cairo_font_options_create;
   begin
      cairo_surface_get_font_options (Surface.Ptr, Ptr);
      return To_Font_Options (Ptr);
   end Get_Font_Options;

   -----------------------
   -- Get_Device_Offset --
   -----------------------

   function Get_Device_Offset
     (Surface : Cairo_Surface'Class)
      return Cairo_Tuple
   is
      Tuple : Cairo_Tuple;
   begin
      cairo_surface_get_device_offset (Surface.Ptr, Tuple.X, Tuple.Y);
      return Tuple;
   end Get_Device_Offset;

   -----------------------------
   -- Get_Fallback_Resolution --
   -----------------------------

   procedure Get_Fallback_Resolution
     (Surface           : Cairo_Surface'Class;
      X_Pixels_Per_Inch : out double;
      Y_Pixels_Per_Inch : out double)
   is
   begin
      cairo_surface_get_fallback_resolution
        (Surface.Ptr,
         X_Pixels_Per_Inch,
         Y_Pixels_Per_Inch);
   end Get_Fallback_Resolution;

   --------------------------
   -- Has_Show_Text_Glyphs --
   --------------------------

   function Has_Show_Text_Glyphs
     (Surface : Cairo_Surface'Class)
      return Boolean
   is
   begin
      return cairo_surface_has_show_text_glyphs (Surface.Ptr) /= 0;
   end Has_Show_Text_Glyphs;

   ---------
   -- Ref --
   ---------

   function Ref (Handle : Cairo_Surface_Handle) return Cairo_Surface_Ref is
   begin
      return Handle.Ref;
   end Ref;

   -----------
   -- Reset --
   -----------

   procedure Reset (Handle : in out Cairo_Surface_Handle) is
   begin
      Handle := Cairo_Surface_Null_Handle;
   end Reset;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Handle : Cairo_Surface_Handle) return Boolean is
   begin
      return Handle /= Cairo_Surface_Null_Handle;
   end Is_Set;

   ---------------
   -- To_Handle --
   ---------------

   function To_Handle
     (Ptr           : Surface_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Surface_Handle
   is
      Ref : Cairo_Surface_Ref := null;
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Surface.To_Handle Ptr:" & Img
      --(Ptr));
      if Ptr /= null then
         declare
            Data   : constant Cairo_User_Data :=
               cairo_surface_get_user_data (Ptr, Ada_Surface_Key'Access);
            Status : Cairo_Status;
         begin
            if Data /= null then
               -- An Ada surface was previously created and attached.
               -- So we reuse it
               Ref := To_Ref (Data);
            else
               -- No Ada surface was ever created and atached.
               -- So we do this.
               Ref    := Allocate_Ref (Ptr);
               Status :=
                  cairo_surface_set_user_data
                    (Ptr,
                     Ada_Surface_Key'Access,
                     To_User_Data (Ref),
                     Destroy_Surface'Access);
               pragma Assert (Status = CAIRO_STATUS_SUCCESS);
            end if;
            pragma Assert (Ref /= null);
            if Is_Referenced then
               Ref.Ptr := Ptr;
            else
               Ref.Ptr := cairo_surface_reference (Ptr);
            end if;
         end;
      end if;
      return Cairo_Surface_Handle'(Ada.Finalization.Controlled with Ref =>
        Ref);

   end To_Handle;

   ---------
   -- Ptr --
   ---------

   function Ptr (Surface : Cairo_Surface'Class) return Surface_Ptr is
   begin
      return Surface.Ptr;
   end Ptr;

   ----------------
   -- Initialize --
   ----------------

   --     procedure Initialize (O : in out Cairo_Surface_Handle) is
   --        pragma Unreferenced (O);
   --     begin
   --        --      Ada.Text_IO.Put_Line ("Cairo.Surface.Initialize H:" & Img
   --(O));
   --        null;
   --     end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Cairo_Surface_Handle) is
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Surface.Adjust H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         O.Ref.Ptr := cairo_surface_reference (O.Ref.Ptr);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Surface_Handle) is
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Surface.Finalize H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         cairo_surface_destroy (O.Ref.Ptr);
      end if;
   end Finalize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Surface_Type : Cairo_Surface_Type;
      Allocator    : Surface_Allocator)
   is
   begin
      pragma Assert (Surface_Allocators (Surface_Type) = null);
      Surface_Allocators (Surface_Type) := Allocator;
   end Register;

   ------------------
   -- Allocate_Ref --
   ------------------

   function Allocate_Ref (Ptr : Surface_Ptr) return Cairo_Surface_Ref is
   begin
      -- This should be the only place where a new surface Ada record
      -- is allocated
      if Ptr = null then
         return null;
      elsif Surface_Allocators (cairo_surface_get_type (Ptr)) /= null then
         return Surface_Allocators (cairo_surface_get_type (Ptr)).all;
      else
         return new Cairo_Surface;
      end if;
   end Allocate_Ref;

end Cairo.Surface;
