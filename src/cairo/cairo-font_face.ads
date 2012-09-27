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

--  A Cairo_Font_Face specifies all aspects of a font other
--  than the size or font matrix (a font matrix is used to distort
--  a font by sheering it or scaling it unequally in the two
--  directions). A font face can be set on a Cairo_Context by using
--  Set_Font_Face; the size and font matrix are set with
--  Set_Font_Size() and Set_Font_Matrix.
--
--  There are various types of font faces, depending on the font backend
--  they use. The type of a font face can be queried using Get_Type.

with Ada.Finalization;

package Cairo.Font_Face is

   pragma Elaborate_Body;

   type Cairo_Font_Face (<>) is tagged limited private;
   type Cairo_Font_Face_Ref is access all Cairo_Font_Face'Class;
   type Cairo_Font_Face_Handle is private;
   Cairo_Font_Face_Null_Handle : constant Cairo_Font_Face_Handle;

   ---------------
   -- User data --
   ---------------

   procedure Set_User_Data
     (Font_Face : in out Cairo_Font_Face'Class;
      Key : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy : Cairo_Destroy_Func;
      Status : out Cairo_Status);
   --  <parameter name="font_face">a Cairo_Font_Face</parameter>
   --  <parameter name="key">the address of a Cairo_User_Data_Key to attach the user data to</parameter>
   --  <parameter name="user_data">the user data to attach to the font face</parameter>
   --  <parameter name="destroy">a Cairo_Destroy_Func which will be called when the
   --  font face is destroyed or when new user data is attached using the
   --  same key.</parameter>
   --
   --  Attach user data to Font_Face.  To remove user data from a font face,
   --  call this function with the key that was used to set it and NULL
   --  for Data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.

   function Get_User_Data
     (Font_Face : Cairo_Font_Face'Class;
      Key : Cairo_User_Data_Key)
      return Cairo_User_Data;
   --  <parameter name="font_face">a Cairo_Font_Face</parameter>
   --  <parameter name="key">the address of the Cairo_User_Data_Key the user data was</parameter>
   --  attached to
   --
   --  Return user data previously attached to Font_Face using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.

   -------------
   -- Getters --
   -------------

   function Get_Status
     (Font_Face : Cairo_Font_Face'Class)
      return Cairo_Status;
   --  <parameter name="font_face">a Cairo_Font_Face</parameter>
   --
   --  Checks whether an error has previously occurred for this
   --  font face
   --
   --  Return value: CAIRO_STATUS_SUCCESS or another error such as
   --  CAIRO_STATUS_NO_MEMORY.

   function Get_Type
     (Font_Face : Cairo_Font_Face'Class)
      return Cairo_Font_Type;
   --  <parameter name="font_face">a font face</parameter>
   --
   --  This function returns the type of the backend used to create
   --  a font face. See Cairo_Fontype_T for available types.
   --
   --  Return value: The type of font_face.
   --
   --  Since: 1.2


   ------------
   -- Handle --
   ------------

   function Ref (Handle : Cairo_Font_Face_Handle) return Cairo_Font_Face_Ref;
   --  Retrieve the font face access stored by the handle.

   -- Deprecated
   procedure Reset (Handle : in out Cairo_Font_Face_Handle);
   function Is_Set (Handle : Cairo_Font_Face_Handle) return Boolean;

   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function To_Handle
     (Ptr : Font_Face_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Font_Face_Handle;
   --  Create a Handle from a C allocated structure (cairo_font_face_t*).
   --  If Is_Referenced is False, then cairo_reference is called to increment
   --  the reference counter.

   function Ptr
     (Font_Face : Cairo_Font_Face'Class)
      return Font_Face_Ptr;
   --  Return the pointer to the C allocated structure (cairo_font_face_t*).

private

   type Cairo_Font_Face is tagged limited record
      Ptr : Font_Face_Ptr;
   end record;

   type Cairo_Font_Face_Handle is new Ada.Finalization.Controlled with record
      Ref : Cairo_Font_Face_Ref;
   end record;
--   procedure Initialize (O : in out Cairo_Font_Face_Handle);
   procedure Adjust (O : in out Cairo_Font_Face_Handle);
   procedure Finalize (O : in out Cairo_Font_Face_Handle);

   Cairo_Font_Face_Null_Handle : constant Cairo_Font_Face_Handle :=
     (Ada.Finalization.Controlled with Ref => null);


   -- Certain font faces have a corresponding Ada type derived from the root
   -- font face (Cairo_Font_Face).
   -- In order to build a correct Ada type from a C pointer, each specific Ada
   -- type must register an allocator.
   -- At most one allocator must be registered for each font type.
   type Font_Face_Allocator is access function return Cairo_Font_Face_Ref;
   procedure Register (Font_Type : Cairo_Font_Type; Allocator : Font_Face_Allocator);

end Cairo.Font_Face;
