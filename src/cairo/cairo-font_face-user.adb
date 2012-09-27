with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Cairo.Support;              use Cairo.Support;
with Cairo.Font_Face; -- For pragma
pragma Elaborate_All (Cairo.Font_Face);
--with Ada.Text_IO; -- Debug

package body Cairo.Font_Face.User is

   function Allocate_User_Font_Face return Cairo_Font_Face_Ref;

   Data_Key : aliased Void;
   pragma Warnings (Off);
   --  Suppress possible aliasin problem warning
   function To_Ada_Data is new Ada.Unchecked_Conversion
     (Cairo_User_Data, Cairo_User_Scaled_Font_Data_Ref);
   pragma Warnings (On);
   function To_User_Data is new Ada.Unchecked_Conversion
     (Cairo_User_Scaled_Font_Data_Ref, Cairo_User_Data);

   procedure Destroy_Data (Data : Cairo_User_Data);
   pragma Convention (C, Destroy_Data);

   --  Wrappers (C convention) around the Ada provided CB

   function Init_Forward
     (Scaled_Font : Scaled_Font_Ptr;
      Context     : Context_Ptr;
      Extents     : access Cairo_Font_Extents)
      return Cairo.Cairo_Status;
   pragma Convention (C, Init_Forward);

   function Render_Glyph_Forward
     (Scaled_Font : Scaled_Font_Ptr;
      Glyph       : unsigned_long;
      Context     : Context_Ptr;
      Extents     : access Cairo_Text_Extents)
      return Cairo.Cairo_Status;
   pragma Convention (C, Render_Glyph_Forward);

   function Text_To_Glyphs_Forward
     (Scaled_Font   : Scaled_Font_Ptr;
      UTF8          : Interfaces.C.Strings.chars_ptr;
      UTF8_Len      : int;
      Glyphs        : Glyph_Ptr_Ptr;
      Num_Glyphs    : Int_Ptr;
      Clusters      : Text_Cluster_Ptr_Ptr;
      Num_Clusters  : Int_Ptr;
      Cluster_Flags : Text_Cluster_Flags_Ptr)
      return Cairo.Cairo_Status;
   pragma Convention (C, Text_To_Glyphs_Forward);

   function Unicode_To_Glyph_Forward
     (Scaled_Font : Scaled_Font_Ptr;
      Unicode     : unsigned_long;
      Glyph_Index : access unsigned_long)
      return Cairo.Cairo_Status;
   pragma Convention (C, Unicode_To_Glyph_Forward);

   ----------
   -- Init --
   ----------

   procedure Init
     (Data        : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Context     : Cairo_Context_Handle;
      Extents     : in out Cairo_Font_Extents;
      Status      : out Cairo_Status)
   is
      pragma Unreferenced (Data, Scaled_Font, Context, Extents);
   begin
      -- Default Init implementation does nothing
      Status := CAIRO_STATUS_SUCCESS;
   end Init;

   --------------------
   -- Text_To_Glyphs --
   --------------------

   procedure Text_To_Glyphs
     (Data          : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font   : Cairo_Scaled_Font_Handle;
      UTF8          : String;
      Glyphs        : in out Cairo_Glyph_List;
      Glyphs_Last   : out Integer;
      Compute_Clusters : Boolean;
      Clusters      : in out Cairo_Text_Cluster_List;
      Clusters_Last : out Integer;
      Cluster_Flags : out Cairo_Text_Cluster_Flags;
      Status        : out Cairo_Status)
   is
      pragma Unreferenced (Data, Scaled_Font, UTF8, Glyphs, Compute_Clusters, Clusters, Cluster_Flags);
   begin
      --      Allocate (Glyphs, -1);
      Glyphs_Last := -1;
      Clusters_Last := -1;
      Status := CAIRO_STATUS_SUCCESS;
   end Text_To_Glyphs;

   ----------------------
   -- Unicode_To_Glyph --
   ----------------------

   procedure Unicode_To_Glyph
     (Data        : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Unicode     : unsigned_long;
      Glyph_Index : out unsigned_long;
      Status      : out Cairo_Status)
   is
      pragma Unreferenced (Data, Scaled_Font);
   begin
      Glyph_Index := Unicode;
      Status := CAIRO_STATUS_SUCCESS;
   end Unicode_To_Glyph;

   ------------------------
   -- New_User_Font_Face --
   ------------------------

   function New_User_Font_Face return Cairo_Font_Face_Handle is
   begin
      return To_Handle (cairo_user_font_face_create, Is_Referenced => True);
   end New_User_Font_Face;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Font_Face    : in out Cairo_User_Font_Face'Class;
      Data         : Cairo_User_Scaled_Font_Data_Ref;
      Auto_Destroy : Boolean := False)
   is
      Status : Cairo_Status;
   begin
      --Ada.Text_IO.Put_Line ("Cairo.Font_Face.User.Set_Data");
      Font_Face.Data := Data;
      --      Font_Face.Auto_Destroy := Auto_Destroy;

      if Auto_Destroy then
         Status :=
            cairo_font_face_set_user_data
              (Font_Face.Ptr,
               Data_Key'Access,
               To_User_Data (Data),
               Destroy_Data'Access);
      else
         Status :=
            cairo_font_face_set_user_data
              (Font_Face.Ptr,
               Data_Key'Access,
               To_User_Data (Data),
               null);
      end if;

      if Status = CAIRO_STATUS_SUCCESS then
         cairo_user_font_face_set_init_func
           (Font_Face.Ptr,
            Init_Forward'Access);
         cairo_user_font_face_set_render_glyph_func
           (Font_Face.Ptr,
            Render_Glyph_Forward'Access);
         cairo_user_font_face_set_text_to_glyphs_func
           (Font_Face.Ptr,
            Text_To_Glyphs_Forward'Access);
         cairo_user_font_face_set_unicode_to_glyph_func
           (Font_Face.Ptr,
            Unicode_To_Glyph_Forward'Access);
      end if;
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Font_Face : Cairo_User_Font_Face'Class)
      return Cairo_User_Scaled_Font_Data_Ref
   is
   begin
      return Font_Face.Data;
   end Get_Data;

   ------------------
   -- Destroy_Data --
   ------------------

   procedure Destroy_Data (Data : Cairo_User_Data) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Cairo_User_Scaled_Font_Data'Class,
         Cairo_User_Scaled_Font_Data_Ref);
      Ref : Cairo_User_Scaled_Font_Data_Ref := To_Ada_Data (Data);
   begin
      --      Ada.Text_IO.Put_Line
      --("*****************************************************");
      --      Ada.Text_IO.Put_Line ("Cairo.Font_Face.User.Destroy_Data Ref:
      --**************");-- & Img (Ref));
      --      Ada.Text_IO.Put_Line
      --("*****************************************************");
      Free (Ref);
   end Destroy_Data;

   ------------------
   -- Init_Forward --
   ------------------

   function Init_Forward
     (Scaled_Font : Scaled_Font_Ptr;
      Context     : Context_Ptr;
      Extents     : access Cairo_Font_Extents)
      return Cairo.Cairo_Status
   is
      Scaled_Font_Handle : constant Cairo_Scaled_Font_Handle :=
         To_Handle (Scaled_Font, Is_Referenced => False);
      Context_Handle : constant Cairo_Context_Handle :=
         To_Handle (Context, Is_Referenced => False);
      Font_Face : constant Font_Face_Ptr :=
         cairo_scaled_font_get_font_face (Scaled_Font);
      User_Data : constant Cairo_User_Data :=
         cairo_font_face_get_user_data (Font_Face, Data_Key'Access);
      Data : constant Cairo_User_Scaled_Font_Data_Ref := To_Ada_Data (User_Data);
      Status : Cairo_Status;
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Font_Face.User.Init_Forward");
      pragma Assert (Data /= null);
      Init
        (Data.all,
         Scaled_Font_Handle,
         Context_Handle,
         Extents.all,
         Status);
      return Status;
   end Init_Forward;

   --------------------------
   -- Render_Glyph_Forward --
   --------------------------

   function Render_Glyph_Forward
     (Scaled_Font : Scaled_Font_Ptr;
      Glyph       : unsigned_long;
      Context     : Context_Ptr;
      Extents     : access Cairo_Text_Extents)
      return Cairo.Cairo_Status
   is
      Scaled_Font_Handle : constant Cairo_Scaled_Font_Handle :=
         To_Handle (Scaled_Font, Is_Referenced => False); -- TODO check
      Context_Handle : constant Cairo_Context_Handle :=
         To_Handle (Context, Is_Referenced => False);
      -- TODO check
      Font_Face : constant Font_Face_Ptr :=
         cairo_scaled_font_get_font_face (Scaled_Font);
      User_Data : constant Cairo_User_Data :=
         cairo_font_face_get_user_data (Font_Face, Data_Key'Access);
      Data : constant Cairo_User_Scaled_Font_Data_Ref := To_Ada_Data (User_Data);
      Status : Cairo_Status;
   begin
      --      Ada.Text_IO.Put_Line
      --("Cairo.Font_Face.User.Render_Glyph_Forward");
      pragma Assert (Data /= null);
      Render_Glyph
        (Data.all,
         Scaled_Font_Handle,
         Glyph,
         Context_Handle,
         Extents.all,
         Status);
      return Status;
   end Render_Glyph_Forward;

   ----------------------------
   -- Text_To_Glyphs_Forward --
   ----------------------------

   function Text_To_Glyphs_Forward
     (Scaled_Font   : Scaled_Font_Ptr;
      UTF8          : Interfaces.C.Strings.chars_ptr;
      UTF8_Len      : int;
      Glyphs        : Glyph_Ptr_Ptr;
      Num_Glyphs    : Int_Ptr;
      Clusters      : Text_Cluster_Ptr_Ptr;
      Num_Clusters  : Int_Ptr;
      Cluster_Flags : Text_Cluster_Flags_Ptr)
      return Cairo.Cairo_Status
   is
      Scaled_Font_Handle : constant Cairo_Scaled_Font_Handle :=
         To_Handle (Scaled_Font, Is_Referenced => False); -- TODO check
      Font_Face : constant Font_Face_Ptr :=
         cairo_scaled_font_get_font_face (Scaled_Font);
      User_Data : constant Cairo_User_Data :=
         cairo_font_face_get_user_data (Font_Face, Data_Key'Access);
      Data : constant Cairo_User_Scaled_Font_Data_Ref := To_Ada_Data (User_Data);
      Ada_UTF8 : constant String :=
         Interfaces.C.Strings.Value (UTF8, Interfaces.C.size_t (UTF8_Len));
      Ada_Glyphs : Cairo_Glyph_List; -- Empty list
      Ada_Glyphs_Last : Integer;
      Ada_Clusters : Cairo_Text_Cluster_List; -- Empty list
      Ada_Clusters_Last : Integer;
      Ada_Cluster_Flags : Cairo_Text_Cluster_Flags := 0;
      Status : Cairo_Status;
   begin
      -- In that case, it is Cairo responsibility to free glyphs and clusters
      -- memory. Thus is true for initila value of Glyphs and Clusters,
      -- but also in the case when they have been reallocated by Ada application

      --      Ada.Text_IO.Put_Line
      --("Cairo.Font_Face.User.Text_To_Glyphs_Forward");
      pragma Assert (Data /= null);

      -- Glyphs should NOT be null, even if the buffer (Glyphs.all) may be null
      pragma Assert (Glyphs /= null);

      -- Wrap C Glyphs into Ada ones
      if Glyphs.all /= null then
         --  At the begining, Num_Glyphs contains the size of the allocated
         --  Glyphs array
         pragma Assert (Num_Glyphs.all >= 0);
         Set_Ptr (Ada_Glyphs, Glyphs.all, Natural (Num_Glyphs.all));
      end if;
      -- Otherwise, Ada_Glyphs is already set to (Ptr => null, Length => 0)

      -- Clusters may be null !
      if Clusters /= null then
         --  In that case, both Num_Clusters and Cluster_Flags must be non null
         pragma Assert (Num_Clusters /= null);
         pragma Assert (Cluster_Flags /= null);
         -- Wrap C Clusters into Ada ones
         if Clusters.all /= null then
            pragma Assert (Num_Clusters.all >= 0);
            Set_Ptr (Ada_Clusters, Clusters.all, Natural (Num_Clusters.all));
         end if;
         -- Otherwise, Ada_Clusters is already set to (Ptr => null, Length => 0)
      end if;

      -- TODO : How do we say to Ada that no cluster computation is needed ?

      Text_To_Glyphs
        (Data.all,
         Scaled_Font_Handle,
         Ada_UTF8,
         Ada_Glyphs,
         Ada_Glyphs_Last,
         Clusters /= null,
         Ada_Clusters,
         Ada_Clusters_Last,
         Ada_Cluster_Flags,
         Status);

      --  Send back (Ada) computed glyphs to Cairo.
      --  Ada side (Text_To_Glyphs) may have resized Glyphs.
      --  Normally, it should only have increased its size.
      Glyphs.all := Ptr (Ada_Glyphs);

      --  It is Cairo responsibility to free memory, even the one allocated
      --  by (Ada) application.
      --  So we reset Ada_Glyphs before it reaches end of its scope.
      Set_Ptr (Ada_Glyphs, null, 0);
      --  In the end, Num_Glyphs contains the number of filled glyphs
      Num_Glyphs.all := int (Ada_Glyphs_Last);

      if Clusters /= null then
         --  Send back (Ada) computed clusters to Cairo
         --  Ada side (Text_To_Glyphs) may have resized Clusters.
         --  Normally, it should only have increased its size.
         Clusters.all := Ptr (Ada_Clusters);
         --  It is Cairo responsibility to free memory, even the one allocated
         --  by (Ada) application.
         --  So we reset Ada_Clusters before it reaches end of its scope.
         Set_Ptr (Ada_Clusters, null, 0);
         Num_Clusters.all := int (Ada_Clusters_Last);
         Cluster_Flags.all := Ada_Cluster_Flags;
      end if;

      return Status;
   end Text_To_Glyphs_Forward;

   ------------------------------
   -- Unicode_To_Glyph_Forward --
   ------------------------------

   function Unicode_To_Glyph_Forward
     (Scaled_Font : Scaled_Font_Ptr;
      Unicode     : unsigned_long;
      Glyph_Index : access unsigned_long)
      return Cairo.Cairo_Status
   is
      Scaled_Font_Handle : constant Cairo_Scaled_Font_Handle :=
         To_Handle (Scaled_Font, Is_Referenced => False); -- TODO check
      Font_Face : constant Font_Face_Ptr :=
         cairo_scaled_font_get_font_face (Scaled_Font);
      User_Data : constant Cairo_User_Data :=
         cairo_font_face_get_user_data (Font_Face, Data_Key'Access);
      Data : constant Cairo_User_Scaled_Font_Data_Ref :=
         To_Ada_Data (User_Data);
      Status : Cairo_Status;
   begin
      --      Ada.Text_IO.Put_Line
      --("Cairo.Font_Face.User.Unicode_To_Glyph_Forward");
      pragma Assert (Data /= null);
      Unicode_To_Glyph
        (Data.all,
         Scaled_Font_Handle,
         Unicode,
         Glyph_Index.all,
         Status);
      return Status;
   end Unicode_To_Glyph_Forward;

   -----------------------------
   -- Allocate_User_Font_Face --
   -----------------------------

   function Allocate_User_Font_Face return Cairo_Font_Face_Ref is
   begin
      return new Cairo_User_Font_Face;
   end Allocate_User_Font_Face;

begin
   Register (CAIRO_FONT_TYPE_USER, Allocate_User_Font_Face'Access);
end Cairo.Font_Face.User;
