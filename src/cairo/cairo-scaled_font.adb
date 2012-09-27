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

with Cairo.Support;              use Cairo.Support;
with Interfaces.C;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

--  with Cairo.Scaled_Font.Debug; use Cairo.Scaled_Font.Debug;
--  with Ada.Text_IO; -- Debug

package body Cairo.Scaled_Font is

   --     package Font_Face_Import is
   --        function To_Handle (Ptr : Font_Face_Ptr; Is_Referenced : Boolean)
   --return Cairo_Font_Face_Handle;
   --        pragma Import (Ada, To_Handle, "cairo__font_face__to_handle");
   --        function Ptr (Font_Face : Cairo_Font_Face'Class) return
   --Font_Face_Ptr;
   --        pragma Import (Ada, Ptr, "cairo__font_face__ptr");
   --     end Font_Face_Import;
   --     use Font_Face_Import;

   Ada_Scaled_Font_Key : aliased Void;
   -- The key used to attach the Ada created scaled font to the C scaled font

   pragma Warnings (Off);
   --  Suppress possible aliasin problem warning
   function To_Ref is new Ada.Unchecked_Conversion
     (Cairo_User_Data, Cairo_Scaled_Font_Ref);
   pragma Warnings (On);
   function To_User_Data is new Ada.Unchecked_Conversion
     (Cairo_Scaled_Font_Ref, Cairo_User_Data);

   procedure Destroy_Scaled_Font (Data : Cairo_User_Data);
   pragma Convention (C, Destroy_Scaled_Font);

   ---------------------
   -- Destroy_Scaled_Font --
   ---------------------

   procedure Destroy_Scaled_Font (Data : Cairo_User_Data) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Cairo_Scaled_Font'Class,
         Cairo_Scaled_Font_Ref);
      Ref : Cairo_Scaled_Font_Ref := To_Ref (Data);
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Scaled_Font.Destroy_Scaled_Font
      --Ref:" & Img (Ref));
      Free (Ref);
   end Destroy_Scaled_Font;

   ---------------------
   -- New_Scaled_Font --
   ---------------------

   function New_Scaled_Font
     (Font_Face    : access Cairo_Font_Face'Class;
      Font_Matrix  : Cairo_Matrix;
      CTM          : Cairo_Matrix;
      Font_Options : Cairo_Font_Options)
      return Cairo_Scaled_Font_Handle
   is
   begin
      return To_Handle
               (cairo_scaled_font_create
                   (Ptr (Font_Face.all),
                    Font_Matrix,
                    CTM,
                    Ptr (Font_Options)),
                Is_Referenced => True);
   end New_Scaled_Font;

   -------------------
   -- Set_User_Data --
   -------------------

   procedure Set_User_Data
     (Scaled_Font : in out Cairo_Scaled_Font'Class;
      Key         : Cairo_User_Data_Key;
      User_Data   : Cairo_User_Data;
      Destroy     : Cairo_Destroy_Func;
      Status      : out Cairo_Status)
   is
   begin
      Status :=
         cairo_scaled_font_set_user_data
           (Scaled_Font.Ptr,
            Key,
            User_Data,
            Destroy);
   end Set_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Scaled_Font : Cairo_Scaled_Font'Class;
      Key         : Cairo_User_Data_Key)
      return Cairo_User_Data
   is
   begin
      return cairo_scaled_font_get_user_data (Scaled_Font.Ptr, Key);
   end Get_User_Data;

   --------------------
   -- Text_To_Glyphs --
   --------------------

   procedure Text_To_Glyphs
     (Scaled_Font   : in out Cairo_Scaled_Font'Class;
      X             : double;
      Y             : double;
      UTF8          : String;
      Glyphs        : in out Cairo_Glyph_List;
      Glyphs_Last   : out Integer;
      Clusters      : in out Cairo_Text_Cluster_List;
      Clusters_Last : out Integer;
      Cluster_Flags : out Cairo_Text_Cluster_Flags;
      Status        : out Cairo_Status)
   is
      C_UTF8 : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (UTF8, Append_Nul => True);
      Glyph_Ptr : aliased Cairo.Glyph_Ptr := Ptr (Glyphs);
      Num_Glyphs : aliased int := int (Get_Length (Glyphs));
      Cluster_Ptr : aliased Cairo.Text_Cluster_Ptr := Ptr (Clusters);
      Num_Clusters : aliased int := int (Get_Length (Clusters));
      Cluster_Flags2 : aliased Cairo_Text_Cluster_Flags;
   begin
      Status :=
         cairo_scaled_font_text_to_glyphs
           (Scaled_Font.Ptr,
            X,
            Y,
            To_Chars_Ptr (C_UTF8'Address),
            UTF8'Length,
            Glyph_Ptr'Access,
            Num_Glyphs'Access,
            Cluster_Ptr'Unchecked_Access,
            Num_Clusters'Unchecked_Access,
            Cluster_Flags2'Unchecked_Access);

      if Glyph_Ptr /= Ptr (Glyphs) then
         -- The glyph array has been reallocated (new size must be greater)
         Set_Ptr (Glyphs, Glyph_Ptr, Natural (Num_Glyphs));
      end if;
      Glyphs_Last := Natural (Num_Glyphs) - 1;

      if Cluster_Ptr /= Ptr (Clusters) then
         -- The cluster array has been reallocated (new size must be greater)
         Set_Ptr (Clusters, Cluster_Ptr, Natural (Num_Clusters));
      end if;
      Clusters_Last := Natural (Num_Clusters) - 1;
      Cluster_Flags := Cluster_Flags2;
   end Text_To_Glyphs;

   --------------------
   -- Text_To_Glyphs --
   --------------------

   procedure Text_To_Glyphs
     (Scaled_Font   : in out Cairo_Scaled_Font'Class;
      X             : double;
      Y             : double;
      UTF8          : String;
      Glyphs        : in out Cairo_Glyph_List;
      Glyphs_Last   : out Integer;
      Status        : out Cairo_Status)
   is
      C_UTF8 : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (UTF8, Append_Nul => True);
      Glyph_Ptr : aliased Cairo.Glyph_Ptr := Ptr (Glyphs);
      Num_Glyphs : aliased int := int (Get_Length (Glyphs));
   begin
      Status :=
         cairo_scaled_font_text_to_glyphs
           (Scaled_Font.Ptr,
            X,
            Y,
            To_Chars_Ptr (C_UTF8'Address),
            UTF8'Length,
            Glyph_Ptr'Access,
            Num_Glyphs'Access,
            null,
            null,
            null);

      if Glyph_Ptr /= Ptr (Glyphs) then
         -- The glyph array has been reallocated (new size must be greater)
         Set_Ptr (Glyphs, Glyph_Ptr, Natural (Num_Glyphs));
      end if;
      Glyphs_Last := Natural (Num_Glyphs) - 1;
   end Text_To_Glyphs;


   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Status
   is
   begin
      return cairo_scaled_font_status (Scaled_Font.Ptr);
   end Get_Status;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Type
   is
   begin
      return cairo_scaled_font_get_type (Scaled_Font.Ptr);
   end Get_Type;

   ----------------------
   -- Get_Scale_Matrix --
   ----------------------

   function Get_Scale_Matrix
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Matrix
   is
      Matrix : Cairo_Matrix;
   begin
      cairo_scaled_font_get_scale_matrix (Scaled_Font.Ptr, Matrix);
      return Matrix;
   end Get_Scale_Matrix;

   ----------------------
   -- Get_Font_Extents --
   ----------------------

   function Get_Font_Extents
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Extents
   is
      Font_Extents : Cairo_Font_Extents;
   begin
      cairo_scaled_font_extents (Scaled_Font.Ptr, Font_Extents);
      return Font_Extents;
   end Get_Font_Extents;

   ----------------------
   -- Get_Text_Extents --
   ----------------------

   function Get_Text_Extents
     (Scaled_Font : Cairo_Scaled_Font'Class;
      UTF8        : String)
      return Cairo_Text_Extents
   is
      Text_Extents : Cairo_Text_Extents;
      C_UTF8       : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (UTF8, Append_Nul => True);
   begin
      cairo_scaled_font_text_extents
        (Scaled_Font.Ptr,
         To_Chars_Ptr (C_UTF8'Address),
         Text_Extents);
      return Text_Extents;
   end Get_Text_Extents;

   -----------------------
   -- Get_Glyph_Extents --
   -----------------------

   function Get_Glyph_Extents
     (Scaled_Font : Cairo_Scaled_Font'Class;
      Glyphs      : Cairo_Glyph_Array)
      return Cairo_Text_Extents
   is
      Glyph_Extents : Cairo_Text_Extents;
   begin
      cairo_scaled_font_glyph_extents
        (Scaled_Font.Ptr,
         To_Glyph_Ptr (Glyphs'Address),
         int (Glyphs'Length),
         Glyph_Extents);
      return Glyph_Extents;
   end Get_Glyph_Extents;

   -------------------
   -- Get_Font_Face --
   -------------------

   function Get_Font_Face
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Face_Handle
   is
   begin
      return To_Handle
               (cairo_scaled_font_get_font_face (Scaled_Font.Ptr),
                Is_Referenced => False); -- Check Is_Referenced
   end Get_Font_Face;

   ---------------------
   -- Get_Font_Matrix --
   ---------------------

   function Get_Font_Matrix
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Matrix
   is
      Font_Matrix : Cairo_Matrix;
   begin
      cairo_scaled_font_get_font_matrix (Scaled_Font.Ptr, Font_Matrix);
      return Font_Matrix;
   end Get_Font_Matrix;

   -------------
   -- Get_CTM --
   -------------

   function Get_CTM
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Matrix
   is
      CTM : Cairo_Matrix;
   begin
      cairo_scaled_font_get_ctm (Scaled_Font.Ptr, CTM);
      return CTM;
   end Get_CTM;

   ----------------------
   -- Get_Font_Options --
   ----------------------

   function Get_Font_Options
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Options
   is
      Ptr : Font_Options_Ptr;
   begin
      Ptr := cairo_font_options_create;
      cairo_scaled_font_get_font_options (Scaled_Font.Ptr, Ptr);
      return To_Font_Options (Ptr);
   end Get_Font_Options;

   ---------
   -- Ref --
   ---------

   function Ref
     (Handle : Cairo_Scaled_Font_Handle)
      return Cairo_Scaled_Font_Ref
   is
   begin
      return Handle.Ref;
   end Ref;

   -----------
   -- Reset --
   -----------

   procedure Reset (Handle : in out Cairo_Scaled_Font_Handle) is
   begin
      Handle := Cairo_Scaled_Font_Null_Handle;
   end Reset;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Handle : Cairo_Scaled_Font_Handle) return Boolean is
   begin
      return Handle /= Cairo_Scaled_Font_Null_Handle;
   end Is_Set;

   ---------------
   -- To_Handle --
   ---------------

   function To_Handle
     (Ptr           : Scaled_Font_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Scaled_Font_Handle
   is
      Ref : Cairo_Scaled_Font_Ref;
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Scaled_Font.To_Handle Ptr:" &
      --Img (Ptr));
      if Ptr /= null then
         declare
            Data   : constant Cairo_User_Data :=
               cairo_scaled_font_get_user_data
                 (Ptr,
                  Ada_Scaled_Font_Key'Access);
            Status : Cairo_Status;
         begin
            if Data /= null then
               -- An Ada scaled font was previously created and attached.
               -- So we reuse it
               Ref := To_Ref (Data);
            else
               -- No Ada scaled font was ever created and atached.
               -- So we do this.
               Ref := new Cairo_Scaled_Font;
               Status :=
                  cairo_scaled_font_set_user_data
                    (Ptr,
                     Ada_Scaled_Font_Key'Access,
                     To_User_Data (Ref),
                     Destroy_Scaled_Font'Access);
               pragma Assert (Status = CAIRO_STATUS_SUCCESS);
            end if;
            pragma Assert (Ref /= null);
            if Is_Referenced then
               Ref.Ptr := Ptr;
            else
               Ref.Ptr := cairo_scaled_font_reference (Ptr);
            end if;
         end;
      end if;
      return Cairo_Scaled_Font_Handle'(Ada.Finalization.Controlled with Ref => Ref);
   end To_Handle;

   ---------
   -- Ptr --
   ---------

   function Ptr
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Scaled_Font_Ptr
   is
   begin
      return Scaled_Font.Ptr;
   end Ptr;

   ----------------
   -- Initialize --
   ----------------

   --     procedure Initialize (O : in out Cairo_Scaled_Font_Handle) is
   --     begin
   --        Ada.Text_IO.Put_Line ("Cairo.Scaled_Font.Initialize H:" & Img
   --(O));
   --     end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Cairo_Scaled_Font_Handle) is
   begin
      --        Ada.Text_IO.Put_Line ("Cairo.Scaled_Font.Adjust H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         O.Ref.Ptr := cairo_scaled_font_reference (O.Ref.Ptr);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Scaled_Font_Handle) is
   begin
      --        Ada.Text_IO.Put_Line ("Cairo.Scaled_Font.Finalize H:" & Img
      --(O));
      if O.Ref /= null then
         cairo_scaled_font_destroy (O.Ref.Ptr);
      end if;
   end Finalize;

end Cairo.Scaled_Font;
