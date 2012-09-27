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

--  A Cairo_Scaled_Font is a font scaled to a particular size and device
--  resolution. A Cairo_Scaled_Font is most useful for low-level font
--  usage where a library or application wants to cache a reference
--  to a scaled font to speed up the computation of metrics.
--
--  There are various types of scaled fonts, depending on the
--  font backend they use. The type of a scaled font can be queried
--  using Get_Type.

with Ada.Finalization;
with Cairo.Font_Options; use Cairo.Font_Options;
with Cairo.Font_Face; use Cairo.Font_Face;
with Cairo.Glyph_List; use Cairo.Glyph_List;
with Cairo.Text_Cluster_List; use Cairo.Text_Cluster_List;

package Cairo.Scaled_Font is

   pragma Elaborate_Body;

   type Cairo_Scaled_Font (<>) is tagged limited private;
   type Cairo_Scaled_Font_Ref is access all Cairo_Scaled_Font'Class;
   type Cairo_Scaled_Font_Handle is private;
   Cairo_Scaled_Font_Null_Handle : constant Cairo_Scaled_Font_Handle;

   ------------------
   -- Construction --
   ------------------

   function New_Scaled_Font
     (Font_Face : access Cairo_Font_Face'Class;
      Font_Matrix : Cairo_Matrix;
      CTM : Cairo_Matrix;
      Font_Options : Cairo_Font_Options)
      return Cairo_Scaled_Font_Handle;
   --  <parameter name="font_face">a Cairo_Font_Face</parameter>
   --  <parameter name="font_matrix">font space to user space transformation matrix for the
   --        font. In the simplest case of a N point font, this matrix is
   --        just a scale by N, but it can also be used to shear the font
   --        or stretch it unequally along the two axes. See
   --        Set_Font_Matrix.</parameter>
   --  <parameter name="ctm">user to device transformation matrix with which the font will
   --        be used.</parameter>
   --  <parameter name="options">options to use when getting metrics for the font and
   --            rendering with it. A NULL pointer will be interpreted as
   --            meaning the default options.</parameter>
   --
   --  Creates a Cairo_Scaled_Font object from a font face and matrices that
   --  describe the size of the font and the environment in which it will
   --  be used.
   --
   --  Return value: a newly created Cairo_Scaled_Font. Destroy with
   --  Scaled_Font_Destroy


   ---------------
   -- User data --
   ---------------

   procedure Set_User_Data
     (Scaled_Font : in out Cairo_Scaled_Font'Class;
      Key : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy : Cairo_Destroy_Func;
      Status : out Cairo_Status);
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="key">the address of a Cairo_User_Data_Key to attach the user data to</parameter>
   --  <parameter name="user_data">the user data to attach to the Cairo_Scaled_Font</parameter>
   --  <parameter name="destroy">a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.</parameter>
   --
   --  Attach user data to Scaled_Font.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for Data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   function Get_User_Data
     (Scaled_Font : Cairo_Scaled_Font'Class;
      Key : Cairo_User_Data_Key)
      return Cairo_User_Data;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="key">the address of the Cairo_User_Data_Key the user data was
   --  attached to</parameter>
   --
   --  Return user data previously attached to Scaled_Font using the
   --  specified key.  If no user data has been attached with the given
   --  key this function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4

   procedure Text_To_Glyphs
     (Scaled_Font : in out Cairo_Scaled_Font'Class;
      X : double;
      Y : double;
      UTF8 : String;
      Glyphs : in out Cairo_Glyph_List;
      Glyphs_Last : out Integer;
      Clusters : in out Cairo_Text_Cluster_List;
      Clusters_Last : out Integer;
      Cluster_Flags : out Cairo_Text_Cluster_Flags;
      Status : out Cairo_Status);
   --  <parameter name="Scaled_Font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="X">X position to place first glyph</parameter>
   --  <parameter name="Y">Y position to place first glyph</parameter>
   --  <parameter name="UTF8">a string of text encoded in UTF-8</parameter>
   --  <parameter name="Glyphs">array of glyphs to fill</parameter>
   --  <parameter name="Glyphs_Last">index of last filled glyph</parameter>
   --  <parameter name="Clusters">array of cluster mapping information to fill</parameter>
   --  <parameter name="Clusters_Last">index of last filled cluster mapping information</parameter>
   --  <parameter name="Cluster_Flags">cluster flags corresponding to the output Clusters</parameter>
   --
   --  Converts UTF-8 text to an array of glyphs, with cluster
   --  mapping, that can be used to render later using Scaled_Font.
   --
   --  If Glyphs length is too small for the conversion, Glyphs is resized.
   --  Glyphs_Last is the index of the last filled glyph.
   --
   --  If Clusters length is too small to store cluster entries, Clusters
   --  is resized.
   --  Clusters_Last is the index of the last filled cluster entry.
   --
   --  <informalexample><programlisting>
   --  Cairo_Status status;
   --
   --  Glyphs        : Cairo_Glyph_List; -- Empty list
   --  Glyphs_Last   : Integer;
   --  Clusters      : Cairo_Text_Cluster_List; -- Empty list
   --  Clusters_Last : Integer;
   --  Cluster_Flags : Cairo_Text_Cluster_Flags;
   --
   --  Text_to_glyphs (Scaled_Font,
   --                  X, Y,
   --                  UTF8,
   --                  Glyphs, Glyphs_Last,
   --                  Clusters, Clusters_Last, Cluster_Flags,
   --                  Status);
   --
   --  if (Status = CAIRO_STATUS_SUCCESS) then
   --      Show_Text_Glyphs (Context,
   --                        UTF8,
   --                        To_Array (Glyphs) (0 .. Glyphs_Last),
   --                        To_Array (Clusters) (0 .. Clusters_Last), Cluster_Flags);
   --  end if;
   --  </programlisting></informalexample>
   --
   --  For details of how Clusters, Num_Clusters, and Cluster_flags map input
   --  UTF-8 text to the output glyphs see Show_Text_Glyphs.
   --
   --  The output values can be readily passed to Show_Text_Glyphs
   --  Show_Glyphs, or related functions, assuming that the exact
   --  same Scaled_font is used for the operation.
   --
   --  Return value: CAIRO_STATUS_SUCCESS upon success, or an error status
   --  if the input values are wrong or if conversion failed.  If the input
   --  values are correct but the conversion failed, the error status is also
   --  set on Scaled_font.
   --
   --  Since: 1.8

   procedure Text_To_Glyphs
     (Scaled_Font : in out Cairo_Scaled_Font'Class;
      X : double;
      Y : double;
      UTF8 : String;
      Glyphs : in out Cairo_Glyph_List;
      Glyphs_Last : out Integer;
      Status : out Cairo_Status);
   --  <parameter name="Scaled_Font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="X">X position to place first glyph</parameter>
   --  <parameter name="Y">Y position to place first glyph</parameter>
   --  <parameter name="UTF8">a string of text encoded in UTF-8</parameter>
   --  <parameter name="Glyphs">array of glyphs to fill</parameter>
   --  <parameter name="Glyphs_Last">index of last filled glyph</parameter>
   --
   --  Converts UTF-8 text to an array of glyphs, without cluster
   --  mapping, that can be used to render later using Scaled_Font.
   --
   --  If Glyphs length is too small for the conversion, Glyphs is resized.
   --  Glyphs_Last is the index of the last filled glyph.
   --  If no cluster mapping is needed:
   --  <informalexample><programlisting>
   --  Cairo_Status status;
   --
   --  Glyphs        : Cairo_Glyph_List; -- Empty list
   --  Glyphs_Last   : Integer;
   --
   --  Text_to_glyphs (Scaled_Font,
   --                  X, Y,
   --                  UTF8,
   --                  Glyphs, Glyphs_Last,
   --                  Status);
   --
   --  if (Status = CAIRO_STATUS_SUCCESS) then
   --      Show_Glyphs (Context,
   --                   UTF8,
   --                   To_Array (Glyphs) (0 .. Glyphs_Last));
   --  end if;
   --  </programlisting></informalexample>
   --
   --  The output values can be readily passed to Show_Glyphs,
   --  or related functions, assuming that the exact
   --  same Scaled_font is used for the operation.
   --
   --  Return value: CAIRO_STATUS_SUCCESS upon success, or an error status
   --  if the input values are wrong or if conversion failed.  If the input
   --  values are correct but the conversion failed, the error status is also
   --  set on Scaled_font.
   --
   --  Since: 1.8

   -------------
   -- Getters --
   -------------

   function Get_Status
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Status;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --
   --  Checks whether an error has previously occurred for this
   --  scaled_font.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or another error such as
   --  CAIRO_STATUS_NO_MEMORY.

   function Get_Type
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Type;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --
   --  This function returns the type of the backend used to create
   --  a scaled font. See Cairo_Fontype_T for available types.
   --
   --  Return value: The type of Scaled_Font.
   --
   --  Since: 1.2

   function Get_Scale_Matrix
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Matrix;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="scale_matrix">return value for the matrix</parameter>
   --
   --  Stores the scale matrix of Scaled_Font into Matrix.
   --  The scale matrix is product of the font matrix and the ctm
   --  associated with the scaled font, and hence is the matrix mapping from
   --  font space to device space.
   --
   --  Since: 1.8

   function Get_Font_Extents
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Extents;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="extents">a Cairo_Font_Extents which to store the retrieved extents.</parameter>
   --
   --  Gets the metrics for a Cairo_Scaled_Font.

   function Get_Text_Extents
     (Scaled_Font : Cairo_Scaled_Font'Class;
      UTF8 : String)
      return Cairo_Text_Extents;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="utf8">a NUL-terminated string of text, encoded in UTF-8</parameter>
   --  Return a Cairo_Text_Extents
   --
   --  Gets the extents for a string of text. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the text
   --  drawn at the origin (0,0) (as it would be drawn by Show_Text
   --  if the cairo graphics state were set to the same font_face,
   --  font_matrix, ctm, and font_options as Scaled_Font).  Additionally,
   --  the x_advance and y_advance values indicate the amount by which the
   --  current point would be advanced by Show_Text.
   --
   --  Note that whitespace characters do not directly contribute to the
   --  size of the rectangle (extents.width and extents.height). They do
   --  contribute indirectly by changing the position of non-whitespace
   --  characters. In particular, trailing whitespace characters are
   --  likely to not affect the size of the rectangle, though they will
   --  affect the x_advance and y_advance values.
   --
   --  Since: 1.2

   function Get_Glyph_Extents
     (Scaled_Font : Cairo_Scaled_Font'Class;
      Glyphs : Cairo_Glyph_Array)
      return Cairo_Text_Extents;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="glyphs">an array of glyph IDs with X and Y offsets.</parameter>
   --  <parameter name="num_glyphs">the number of glyphs in the Glyphs array</parameter>
   --  Return a Cairo_Text_Extents
   --
   --  Gets the extents for an array of glyphs. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the
   --  glyphs, (as they would be drawn by Show_Glyphs if the cairo
   --  graphics state were set to the same font_face, font_matrix, ctm,
   --  and font_options as Scaled_Font).  Additionally, the x_advance and
   --  y_advance values indicate the amount by which the current point
   --  would be advanced by Show_Glyphs.
   --
   --  Note that whitespace glyphs do not contribute to the size of the
   --  rectangle (extents.width and extents.height).

   function Get_Font_Face
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Face_Handle;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --
   --  Gets the font face that this scaled font was created for.
   --
   --  Return value: The Cairo_Font_Face with which Scaled_Font was
   --  created.
   --
   --  Since: 1.2

   function Get_Font_Matrix
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Matrix;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="font_matrix">return value for the matrix</parameter>
   --
   --  Stores the font matrix with which Scaled_Font was created into
   --  Matrix.
   --
   --  Since: 1.2

   function Get_CTM
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Matrix;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="ctm">return value for the CTM</parameter>
   --
   --  Stores the CTM with which Scaled_Font was created into CTM.
   --
   --  Since: 1.2

   function Get_Font_Options
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Cairo_Font_Options;
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --  <parameter name="options">return value for the font options</parameter>
   --
   --  Stores the font options with which Scaled_Font was created into
   --  Options.
   --
   --  Since: 1.2


   ------------
   -- Handle --
   ------------

   function Ref (Handle : Cairo_Scaled_Font_Handle) return Cairo_Scaled_Font_Ref;
   -- Deprecated
   procedure Reset (Handle : in out Cairo_Scaled_Font_Handle);
   function Is_Set (Handle : Cairo_Scaled_Font_Handle) return Boolean;

   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function To_Handle
     (Ptr : Scaled_Font_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Scaled_Font_Handle;
   --  Create a Handle from a C allocated structure (cairo_scaled_font_t*).
   --  If Is_Referenced is False, then cairo_scaled_font_reference is called
   --  to increment the reference counter.

   function Ptr
     (Scaled_Font : Cairo_Scaled_Font'Class)
      return Scaled_Font_Ptr;
   --  Return the pointer to the C allocated structure (cairo_scaled_font_t*).

private

   type Cairo_Scaled_Font is tagged limited record
      Ptr : Scaled_Font_Ptr;
   end record;

   type Cairo_Scaled_Font_Handle is new Ada.Finalization.Controlled with record
      Ref : Cairo_Scaled_Font_Ref;
   end record;
--   procedure Initialize (O : in out Cairo_Scaled_Font_Handle);
   procedure Adjust (O : in out Cairo_Scaled_Font_Handle);
   procedure Finalize (O : in out Cairo_Scaled_Font_Handle);

   Cairo_Scaled_Font_Null_Handle : constant Cairo_Scaled_Font_Handle :=
     (Ada.Finalization.Controlled with Ref => null);

end Cairo.Scaled_Font;
