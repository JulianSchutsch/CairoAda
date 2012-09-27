------------------------------------------------------------------------------
--                                                                          --
--                  RsvgAda - Ada95 binding for Rsvg                        --
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

with System;
with Interfaces.C.Strings;

package body RSVG is

   use Glib.Object;

   function To_String (X : Interfaces.C.Strings.chars_ptr) return String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Interfaces.C.Strings.chars_ptr) return String is
      use Interfaces.C.Strings;
   begin
      if X /= Null_Ptr then
         return Interfaces.C.Strings.Value (X);
      else
         return "";
      end if;
   end To_String;

--     function Quark_To_String (Quark : GQuark) return String is
--        function Internal (Quark : GQuark) return Interfaces.C.Strings.chars_ptr;
--        pragma Import (C, Internal, "g_quark_to_string");
--     begin
--        return Interfaces.C.Strings.Value (Internal (Quark));
--     end Quark_To_String;
--
--     function Image (Error : GError) return String is
--     begin
--        if Error = null then
--           return "null";
--        else
--           return "Domain:" & Quark_To_String (Get_Domain (Error)) &
--           " Code:" & Gint'Image (Get_Code (Error)) &
--           " Message:" & Get_Message (Error);
--        end if;
--     end Image;

   --------------
   -- RSVG_New --
   --------------

   procedure RSVG_New (Handle : out RSVG_Handle) is
      function Internal return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new");
   begin
      Handle := new RSVG_Handle_Record;
      Set_Object (Handle, Internal);
   end RSVG_New;

   ------------------------
   -- RSVG_New_From_Data --
   ------------------------

   procedure RSVG_New_From_Data
     (Handle : out RSVG_Handle;
      Data   : Ada.Streams.Stream_Element_Array;
      Error  : out GError)
   is
      function Internal
        (Data : System.Address;
         Data_Len : Gsize;
         Error : access GError)
         return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new_from_data");
      Err : aliased GError;
   begin
      Handle := new RSVG_Handle_Record;
      Set_Object (Handle, Internal (Data'Address, Gsize (Data'Length), Err'Access));
      Error := Err;
   end RSVG_New_From_Data;

   ------------------------
   -- RSVG_New_From_File --
   ------------------------

   procedure RSVG_New_From_File
     (Handle   : out RSVG_Handle;
      Filename : String;
      Error    : out GError)
   is
      function Internal
        (Filename : System.Address;
         Error : access GError)
         return System.Address;
      pragma Import (C, Internal, "rsvg_handle_new_from_file");
      C_Filename : constant String := Filename & ASCII.NUL;
      Err : aliased GError;
   begin
      Handle := new RSVG_Handle_Record;
      Set_Object (Handle, Internal (C_Filename'Address, Err'Access));
      Error := Err;
   end RSVG_New_From_File;

   -------------
   -- Set_DPI --
   -------------

   procedure Set_DPI
     (Handle : access RSVG_Handle_Record'Class;
      DPI : Gdouble)
   is
      procedure Internal
        (Handle : System.Address;
         DPI : Gdouble);
      pragma Import (C, Internal, "rsvg_handle_set_dpi");
   begin
      Internal (Get_Object (Handle), DPI);
   end Set_DPI;

   -----------------
   -- Set_DPI_X_Y --
   -----------------

   procedure Set_DPI_X_Y
     (Handle : access RSVG_Handle_Record'Class;
      DPI_X : Gdouble;
      DPI_Y : Gdouble)
   is
      procedure Internal
        (Handle : System.Address;
         DPI_X : Gdouble;
         DPI_Y : Gdouble);
      pragma Import (C, Internal, "rsvg_handle_set_dpi_x_y");
   begin
      Internal (Get_Object (Handle), DPI_X, DPI_Y);
   end Set_DPI_X_Y;

   -----------
   -- Write --
   -----------

   procedure Write
     (Handle  : access RSVG_Handle_Record'Class;
      Buffer : Ada.Streams.Stream_Element_Array;
      Error  : out GError;
      Success : out Boolean)
   is
      function Internal
        (Handle : System.Address;
         Buffer : System.Address;
         Count : Gsize;
         Error : access GError)
         return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_write");
      Err : aliased GError;
      Result : Gboolean;
   begin
      Result := Internal (Get_Object (Handle),
                          Buffer'Address,
                          Gsize (Buffer'Length),
                          Err'Access);
      Error := Err;
      Success := Result /= 0;
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close
     (Handle  : access RSVG_Handle_Record'Class;
      Error   : out GError;
      Success : out Boolean)
   is
      function Internal
        (Handle : System.Address;
         Error : access GError)
         return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_close");

      Err : aliased GError;
   begin
      Success := (Internal (Get_Object (Handle), Err'Access) /= 0);
      Error := Err;
   end Close;

   ------------------
   -- Get_Base_URI --
   ------------------

   function Get_Base_URI
     (Handle : access RSVG_Handle_Record'Class)
      return String
   is
      function Internal
        (Handle : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "rsvg_handle_get_base_uri");
   begin
      return To_String (Internal (Get_Object (Handle)));
   end Get_Base_URI;

   ------------------
   -- Set_Base_URI --
   ------------------

   procedure Set_Base_URI
     (Handle : access RSVG_Handle_Record'Class;
      Base_URI : String)
   is
      procedure Internal
        (Handle : System.Address;
         Base_URI : System.Address);
      pragma Import (C, Internal, "rsvg_handle_set_base_uri");
      C_Base_URI : constant String := Base_URI & ASCII.NUL;
   begin
      Internal (Get_Object (Handle), C_Base_URI'Address);
   end Set_Base_URI;

   --------------------
   -- Get_Dimensions --
   --------------------

   function Get_Dimensions
     (Handle : access RSVG_Handle_Record'Class)
      return RSVG_Dimension_Data
   is
      procedure Internal
        (Handle : System.Address;
         Dimensions : access RSVG_Dimension_Data);
      pragma Import (C, Internal, "rsvg_handle_get_dimensions");

      Dim : aliased RSVG_Dimension_Data;
   begin
      Internal (Get_Object (Handle), Dim'Access);
      return Dim;
   end Get_Dimensions;

   ------------------------
   -- Get_Dimensions_Sub --
   ------------------------

   procedure Get_Dimensions_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id : String;
      Dimension_Data : out RSVG_Dimension_Data;
      Success : out Boolean)
   is
      function Internal
        (Handle : System.Address;
         Dimension_Data : System.Address;
         Id : System.Address)
        return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_get_dimensions_sub");

      C_Id : constant String := Id & ASCII.NUL;
      Result : Gboolean;
   begin
      if Id = "" then
         Result := Internal (Get_Object (Handle),
                             Dimension_Data'Address,
                             System.Null_Address);
      else
         Result := Internal (Get_Object (Handle),
                             Dimension_Data'Address,
                             C_Id'Address);
      end if;
      Success := Result /= 0;
   end Get_Dimensions_Sub;

   ----------------------
   -- Get_Position_Sub --
   ----------------------

   procedure Get_Position_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id : String;
      Position_Data : out RSVG_Position_Data;
      Success : out Boolean)
   is
      function Internal
        (Handle : System.Address;
         Position_Data : System.Address;
         Id : System.Address)
        return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_get_position_sub");

      C_Id : constant String := Id & ASCII.NUL;
      Result : Gboolean;
   begin
      if Id = "" then
         Result := Internal (Get_Object (Handle),
                             Position_Data'Address,
                             System.Null_Address);
      else
         Result := Internal (Get_Object (Handle),
                             Position_Data'Address,
                             C_Id'Address);
      end if;
      Success := Result /= 0;
   end Get_Position_Sub;

   -------------
   -- Has_Sub --
   -------------

   function Has_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id : String)
      return Boolean
   is
      function Internal
        (Handle : System.Address;
         Id : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_has_sub");
      C_Id : constant String := Id & ASCII.NUL;
   begin
      return Internal (Get_Object (Handle), C_Id'Address) /= 0;
   end Has_Sub;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title
     (Handle : access RSVG_Handle_Record'Class)
      return String
   is
      function Internal
        (Handle : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "rsvg_handle_get_title");
   begin
      return To_String (Internal (Get_Object (Handle)));
   end Get_Title;

   --------------
   -- Get_Desc --
   --------------

   function Get_Desc
     (Handle : access RSVG_Handle_Record'Class)
      return String
   is
      function Internal
        (Handle : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "rsvg_handle_get_desc");
   begin
      return To_String (Internal (Get_Object (Handle)));
   end Get_Desc;

   ------------------
   -- Get_Metadata --
   ------------------

   function Get_Metadata
     (Handle : access RSVG_Handle_Record'Class)
      return String
   is
      function Internal
        (Handle : System.Address)
         return Interfaces.C.Strings.chars_ptr;
      pragma Import (C, Internal, "rsvg_handle_get_metadata");
   begin
      return To_String (Internal (Get_Object (Handle)));
   end Get_Metadata;

   ------------
   -- Render --
   ------------

   procedure Render
     (Handle  : access RSVG_Handle_Record'Class;
      Context : in out Cairo_Context'Class;
      Success : out Boolean)
   is
      function Internal
        (Handle : System.Address;
         Context : Cairo.Context_Ptr)
         return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_render_cairo");
      Result : Gboolean;
   begin
      Result := Internal (Get_Object (Handle), Ptr (Context));
      Success := Result /= 0;
   end Render;

   ----------------
   -- Render_Sub --
   ----------------

   procedure Render_Sub
     (Handle  : access RSVG_Handle_Record'Class;
      Context : in out Cairo_Context'Class;
      Id      : String;
      Success : out Boolean)
   is
      function Internal
        (Handle : System.Address;
         Context : Cairo.Context_Ptr;
         Id : System.Address)
         return Gboolean;
      pragma Import (C, Internal, "rsvg_handle_render_cairo_sub");
      C_Id : constant String := Id & ASCII.NUL;
      Result : Gboolean;
   begin
      if Id = "" then
         Result := Internal (Get_Object (Handle),
                             Ptr (Context),
                             System.Null_Address);
      else
         Result := Internal (Get_Object (Handle),
                             Ptr (Context),
                             C_Id'Address);
      end if;
      Success := Result /= 0;
   end Render_Sub;

   ----------------
   -- Get_Pixbuf --
   ----------------

   function Get_Pixbuf
     (Handle : access RSVG_Handle_Record'Class)
      return Gdk_Pixbuf
   is
      function Internal
        (Handle : System.Address)
--         return System.Address;
-- CHANGE on SVN
         return Gdk_Pixbuf;
      pragma Import (C, Internal, "rsvg_handle_get_pixbuf");
   begin
      return Internal (Get_Object (Handle));
      --return Convert (Internal (Get_Object (Handle)));
   end Get_Pixbuf;

   --------------------
   -- Get_Pixbuf_Sub --
   --------------------

   function Get_Pixbuf_Sub
     (Handle : access RSVG_Handle_Record'Class;
      Id     : String)
      return Gdk_Pixbuf
   is
      function Internal
        (Handle : System.Address;
         Id : System.Address)
--         return System.Address;
      -- CHANGE on SVN
         return Gdk_Pixbuf;
      pragma Import (C, Internal, "rsvg_handle_get_pixbuf_sub");
      C_Id : constant String := Id & ASCII.NUL;
   begin
      return Internal (Get_Object (Handle), C_Id'Address);
      --return Convert (Internal (Get_Object (Handle), C_Id'Address));
   end Get_Pixbuf_Sub;

end RSVG;
