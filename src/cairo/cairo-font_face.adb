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
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
--with Ada.Tags;

--  with Ada.Text_IO; -- Debug
--  with Cairo.Font_Face.Debug; use Cairo.Font_Face.Debug;

package body Cairo.Font_Face is

   Font_Face_Allocators : array (Cairo_Font_Type) of Font_Face_Allocator;

   function Allocate_Ref (Ptr : Font_Face_Ptr) return Cairo_Font_Face_Ref;
   --  Allocate a new Ada type corresponding to the C type.
   --  If Ptr is null, then null is returned.
   --  If no allocator has been registered for the correponding font type,
   --  then a default Cairo_Font_Face object is allocated.

   Ada_Font_Face_Key : aliased Void;
   --  The key used to attach the Ada created font face to the C font face

   pragma Warnings (Off);
   --  Suppress possible aliasin problem warning
   function To_Ref is new Ada.Unchecked_Conversion
     (Cairo_User_Data, Cairo_Font_Face_Ref);
   pragma Warnings (On);
   function To_User_Data is new Ada.Unchecked_Conversion
     (Cairo_Font_Face_Ref, Cairo_User_Data);

   procedure Destroy_Font_Face (Data : Cairo_User_Data);
   pragma Convention (C, Destroy_Font_Face);

   -----------------------
   -- Destroy_Font_Face --
   -----------------------

   procedure Destroy_Font_Face (Data : Cairo_User_Data) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Cairo_Font_Face'Class,
         Cairo_Font_Face_Ref);
      Ref : Cairo_Font_Face_Ref := To_Ref (Data);
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Font_Face.Destroy_Font_Face Ref:" &
      --Img (Ref));
      Free (Ref);
   end Destroy_Font_Face;

   -------------------
   -- Set_User_Data --
   -------------------

   procedure Set_User_Data
     (Font_Face : in out Cairo_Font_Face'Class;
      Key       : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy   : Cairo_Destroy_Func;
      Status    : out Cairo_Status)
   is
   begin
      Status :=
         cairo_font_face_set_user_data
           (Font_Face.Ptr,
            Key,
            User_Data,
            Destroy);
   end Set_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Font_Face : Cairo_Font_Face'Class;
      Key       : Cairo_User_Data_Key)
      return Cairo_User_Data
   is
   begin
      return cairo_font_face_get_user_data (Font_Face.Ptr, Key);
   end Get_User_Data;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Font_Face : Cairo_Font_Face'Class)
      return Cairo_Status
   is
   begin
      return cairo_font_face_status (Font_Face.Ptr);
   end Get_Status;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Font_Face : Cairo_Font_Face'Class)
      return Cairo_Font_Type
   is
   begin
      return cairo_font_face_get_type (Font_Face.Ptr);
   end Get_Type;

   ---------
   -- Ref --
   ---------

   function Ref
     (Handle : Cairo_Font_Face_Handle)
      return Cairo_Font_Face_Ref
   is
   begin
      return Handle.Ref;
   end Ref;

   -----------
   -- Reset --
   -----------

   procedure Reset (Handle : in out Cairo_Font_Face_Handle) is
   begin
      Handle := Cairo_Font_Face_Null_Handle;
   end Reset;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Handle : Cairo_Font_Face_Handle) return Boolean is
   begin
      return Handle /= Cairo_Font_Face_Null_Handle;
   end Is_Set;

   ---------------
   -- To_Handle --
   ---------------

   function To_Handle
     (Ptr           : Font_Face_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Font_Face_Handle
   is
      Ref : Cairo_Font_Face_Ref := null;
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Font_Face.To_Handle Ptr:" & Img
      --(Ptr));
      if Ptr /= null then
         declare
            Data   : constant Cairo_User_Data :=
               cairo_font_face_get_user_data
                 (Ptr,
                  Ada_Font_Face_Key'Access);
            Status : Cairo_Status;
         begin
            if Data /= null then
               -- An Ada font face was previously created and attached.
               -- So we reuse it
               Ref := To_Ref (Data);
            else
               -- No Ada font face was ever created and atached.
               -- So we do this.
               Ref    := Allocate_Ref (Ptr);
               Status :=
                  cairo_font_face_set_user_data
                    (Ptr,
                     Ada_Font_Face_Key'Access,
                     To_User_Data (Ref),
                     Destroy_Font_Face'Access);
               pragma Assert (Status = CAIRO_STATUS_SUCCESS);
            end if;
            pragma Assert (Ref /= null);
            if Is_Referenced then
               Ref.Ptr := Ptr;
            else
               Ref.Ptr := cairo_font_face_reference (Ptr);
            end if;
         end;
      end if;
      return Cairo_Font_Face_Handle'(Ada.Finalization.Controlled with Ref =>
        Ref);
   end To_Handle;

   ---------
   -- Ptr --
   ---------

   function Ptr (Font_Face : Cairo_Font_Face'Class) return Font_Face_Ptr is
   begin
      return Font_Face.Ptr;
   end Ptr;

   ----------------
   -- Initialize --
   ----------------

   --     procedure Initialize (O : in out Cairo_Font_Face_Handle) is
   --     begin
   --        Ada.Text_IO.Put_Line ("Cairo.Font_Face.Initialize H:" & Img (O));
   --     end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Cairo_Font_Face_Handle) is
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Font_Face.Adjust H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         O.Ref.Ptr := cairo_font_face_reference (O.Ref.Ptr);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Font_Face_Handle) is
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Font_Face.Finalize H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         cairo_font_face_destroy (O.Ref.Ptr);
      end if;
   end Finalize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Font_Type : Cairo_Font_Type;
      Allocator : Font_Face_Allocator)
   is
   begin
      pragma Assert (Font_Face_Allocators (Font_Type) = null);
      Font_Face_Allocators (Font_Type) := Allocator;
   end Register;

   ------------------
   -- Allocate_Ref --
   ------------------

   function Allocate_Ref (Ptr : Font_Face_Ptr) return Cairo_Font_Face_Ref is
   begin
      -- This should be the only place where a new Font Face Ada record
      -- is allocated
      if Ptr = null then
         return null;
      elsif Font_Face_Allocators (cairo_font_face_get_type (Ptr)) /=
            null
      then
         return Font_Face_Allocators (cairo_font_face_get_type (Ptr)).all;
      else
         return new Cairo_Font_Face;
      end if;
   end Allocate_Ref;

end Cairo.Font_Face;
