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
--with Ada.Tags;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

--  with Ada.Text_IO; -- Debug
--  with Cairo.Pattern.Debug; use Cairo.Pattern.Debug;

package body Cairo.Pattern is

   Pattern_Allocators : array (Cairo_Pattern_Type) of Pattern_Allocator;

   function Allocate_Ref (Ptr : Pattern_Ptr) return Cairo_Pattern_Ref;

   Ada_Pattern_Key : aliased Void;
   -- The key used to attach the Ada created pattern to the C pattern

   pragma Warnings (Off);
   --  Suppress possible aliasin problem warning
   function To_Ref is new Ada.Unchecked_Conversion
     (Cairo_User_Data, Cairo_Pattern_Ref);
   pragma Warnings (On);
   function To_User_Data is new Ada.Unchecked_Conversion
     (Cairo_Pattern_Ref, Cairo_User_Data);

   procedure Destroy_Pattern (Data : Cairo_User_Data);
   pragma Convention (C, Destroy_Pattern);

   ---------------------
   -- Destroy_Pattern --
   ---------------------

   procedure Destroy_Pattern (Data : Cairo_User_Data) is
      procedure Free is new Ada.Unchecked_Deallocation (
         Cairo_Pattern'Class,
         Cairo_Pattern_Ref);
      Ref : Cairo_Pattern_Ref := To_Ref (Data);
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Pattern.Destroy_Pattern Ref:" & Img
      --(Ref));
      Free (Ref);
   end Destroy_Pattern;

   -------------------
   -- Set_User_Data --
   -------------------

   procedure Set_User_Data
     (Pattern   : in out Cairo_Pattern'Class;
      Key       : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy   : Cairo_Destroy_Func;
      Status    : out Cairo_Status)
   is
   begin
      Status :=
         cairo_pattern_set_user_data (Pattern.Ptr, Key, User_Data, Destroy);
   end Set_User_Data;

   -------------------
   -- Get_User_Data --
   -------------------

   function Get_User_Data
     (Pattern : Cairo_Pattern'Class;
      Key     : Cairo_User_Data_Key)
      return Cairo_User_Data
   is
   begin
      return cairo_pattern_get_user_data (Pattern.Ptr, Key);
   end Get_User_Data;

   ----------------
   -- Set_Matrix --
   ----------------

   procedure Set_Matrix
     (Pattern : in out Cairo_Pattern'Class;
      Matrix  : Cairo_Matrix)
   is
   begin
      cairo_pattern_set_matrix (Pattern.Ptr, Matrix);
   end Set_Matrix;

   ----------------
   -- Get_Matrix --
   ----------------

   function Get_Matrix (Pattern : Cairo_Pattern'Class) return Cairo_Matrix is
      Matrix : Cairo_Matrix;
   begin
      cairo_pattern_get_matrix (Pattern.Ptr, Matrix);
      return Matrix;
   end Get_Matrix;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Pattern : Cairo_Pattern'Class) return Cairo_Status is
   begin
      return cairo_pattern_status (Pattern.Ptr);
   end Get_Status;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Pattern : Cairo_Pattern'Class)
      return Cairo_Pattern_Type
   is
   begin
      return cairo_pattern_get_type (Pattern.Ptr);
   end Get_Type;

   ---------
   -- Ref --
   ---------

   function Ref (Handle : Cairo_Pattern_Handle) return Cairo_Pattern_Ref is
   begin
      return Handle.Ref;
   end Ref;

   -----------
   -- Reset --
   -----------

   procedure Reset (Handle : in out Cairo_Pattern_Handle) is
   begin
      Handle := Cairo_Pattern_Null_Handle;
   end Reset;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Handle : Cairo_Pattern_Handle) return Boolean is
   begin
      return Handle /= Cairo_Pattern_Null_Handle;
   end Is_Set;

   ---------------
   -- To_Handle --
   ---------------

   function To_Handle
     (Ptr           : Pattern_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Pattern_Handle
   is
      Ref : Cairo_Pattern_Ref;
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Pattern.To_Handle Ptr:" & Img
      --(Ptr));
      if Ptr /= null then
         declare
            Data   : constant Cairo_User_Data :=
               cairo_pattern_get_user_data (Ptr, Ada_Pattern_Key'Access);
            Status : Cairo_Status;
         begin
            if Data /= null then
               -- An Ada pattern was previously created and attached.
               -- So we reuse it
               Ref := To_Ref (Data);
            else
               -- No Ada pattern was ever created and atached.
               -- So we do this.
               Ref    := Allocate_Ref (Ptr);
               Status :=
                  cairo_pattern_set_user_data
                    (Ptr,
                     Ada_Pattern_Key'Access,
                     To_User_Data (Ref),
                     Destroy_Pattern'Access);
               pragma Assert (Status = CAIRO_STATUS_SUCCESS);
            end if;
            pragma Assert (Ref /= null);
            if Is_Referenced then
               Ref.Ptr := Ptr;
            else
               Ref.Ptr := cairo_pattern_reference (Ptr);
            end if;
         end;
      end if;
      return Cairo_Pattern_Handle'(Ada.Finalization.Controlled with Ref =>
        Ref);
   end To_Handle;

   ---------
   -- Ptr --
   ---------

   function Ptr (Pattern : Cairo_Pattern'Class) return Pattern_Ptr is
   begin
      return Pattern.Ptr;
   end Ptr;

   ----------------
   -- Initialize --
   ----------------

   --     procedure Initialize (O : in out Cairo_Pattern_Handle) is
   --     begin
   --        Ada.Text_IO.Put_Line ("Cairo.Pattern.Initialize H:" & Img (O));
   --     end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Cairo_Pattern_Handle) is
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Pattern.Adjust H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         O.Ref.Ptr := cairo_pattern_reference (O.Ref.Ptr);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Pattern_Handle) is
   begin
      --      Ada.Text_IO.Put_Line ("Cairo.Pattern.Finalize H:" & Img (O));
      if O.Ref /= null then
         pragma Assert (O.Ref.Ptr /= null);
         cairo_pattern_destroy (O.Ref.Ptr);
      end if;
   end Finalize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Pattern_Type : Cairo_Pattern_Type;
      Allocator    : Pattern_Allocator)
   is
   begin
      pragma Assert (Pattern_Allocators (Pattern_Type) = null);
      Pattern_Allocators (Pattern_Type) := Allocator;
   end Register;

   ------------------
   -- Allocate_Ref --
   ------------------

   function Allocate_Ref (Ptr : Pattern_Ptr) return Cairo_Pattern_Ref is
   begin
      if Ptr = null then
         return null;
      elsif Pattern_Allocators (cairo_pattern_get_type (Ptr)) /= null then
         return Pattern_Allocators (cairo_pattern_get_type (Ptr)).all;
      else
         return new Cairo_Pattern;
      end if;
   end Allocate_Ref;

end Cairo.Pattern;
