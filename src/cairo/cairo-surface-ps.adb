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

with Cairo.Support.PS; use Cairo.Support.PS; use Cairo.Support;
with Interfaces.C.Strings;
with Cairo.Surface; -- for pragma !!!
pragma Elaborate_All (Cairo.Surface);

package body Cairo.Surface.PS is

   function Allocate_PS_Surface return Cairo_Surface_Ref;

   --------------------
   -- New_PS_Surface --
   --------------------

   function New_PS_Surface
     (Filename         : String;
      Width_In_Points  : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle
   is
      C_Filename : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Filename, Append_Nul => True);
   begin
      return To_Handle
               (cairo_ps_surface_create
                   (To_Chars_Ptr (C_Filename'Address),
                    Width_In_Points,
                    Height_In_Points),
                Is_Referenced => True);
   end New_PS_Surface;

   -------------------------------
   -- New_PS_Surface_For_Stream --
   -------------------------------

   function New_PS_Surface_For_Stream
     (Stream           : access Ada.Streams.Root_Stream_Type'Class;
      Width_In_Points  : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle
   is
   begin
      return To_Handle
               (cairo_ps_surface_create_for_stream
                   (Stream_Read'Access,
                    To_Closure (Stream_Access (Stream)),
                    Width_In_Points,
                    Height_In_Points),
                Is_Referenced => True);
   end New_PS_Surface_For_Stream;

   -----------------------
   -- Restrict_To_Level --
   -----------------------

   procedure Restrict_To_Level
     (Surface : in out Cairo_PS_Surface'Class;
      Level   : PS_Level)
   is
   begin
      cairo_ps_surface_restrict_to_level (Surface.Ptr, Level);
   end Restrict_To_Level;

   -------------------
   -- Get_PS_Levels --
   -------------------

   function Get_PS_Levels return PS_Level_Array is
      Level_Ptr  : PS_Level_Ptr;
      Num_Levels : int;
   begin
      cairo_ps_get_levels (Level_Ptr, Num_Levels);
      declare
         Levels : PS_Level_Array (0 .. Integer (Num_Levels) - 1);
         for Levels'Address use To_Address (Level_Ptr);
      begin
         return Levels;
      end;
   end Get_PS_Levels;

   -------------
   -- Set_Eps --
   -------------

   procedure Set_Eps
     (Surface : in out Cairo_PS_Surface'Class;
      Eps     : Boolean)
   is
      C_Eps : constant Cairo_Bool := Cairo_Bool'Val (Boolean'Pos (Eps));
   begin
      cairo_ps_surface_set_eps (Surface.Ptr, C_Eps);
   end Set_Eps;

   -------------
   -- Get_Eps --
   -------------

   function Get_Eps (Surface : Cairo_PS_Surface'Class) return Boolean is
   begin
      return cairo_ps_surface_get_eps (Surface.Ptr) /= 0;
   end Get_Eps;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Surface          : in out Cairo_PS_Surface'Class;
      Width_In_Points  : double;
      Height_In_Points : double)
   is
   begin
      cairo_ps_surface_set_size
        (Surface.Ptr,
         Width_In_Points,
         Height_In_Points);
   end Set_Size;

   -----------------
   -- DSC_Comment --
   -----------------

   procedure DSC_Comment
     (Surface : in out Cairo_PS_Surface'Class;
      Comment : String)
   is
      C_Comment : aliased Interfaces.C.char_array :=
         Interfaces.C.To_C (Comment, Append_Nul => True);
   begin
      cairo_ps_surface_dsc_comment
        (Surface.Ptr,
         To_Chars_Ptr (C_Comment'Address));
   end DSC_Comment;

   ---------------------
   -- DSC_Begin_Setup --
   ---------------------

   procedure DSC_Begin_Setup (Surface : in out Cairo_PS_Surface'Class) is
   begin
      cairo_ps_surface_dsc_begin_setup (Surface.Ptr);
   end DSC_Begin_Setup;

   --------------------------
   -- DSC_Begin_Page_Setup --
   --------------------------

   procedure DSC_Begin_Page_Setup (Surface : in out Cairo_PS_Surface'Class) is
   begin
      cairo_ps_surface_dsc_begin_page_setup (Surface.Ptr);
   end DSC_Begin_Page_Setup;

   ---------------
   -- To_String --
   ---------------

   function To_String (Level : PS_Level) return String is
      C_Result : Interfaces.C.Strings.chars_ptr;
      use type Interfaces.C.Strings.chars_ptr;
   begin
      C_Result := cairo_ps_level_to_string (Level);
      if C_Result /= Interfaces.C.Strings.Null_Ptr then
         return Interfaces.C.Strings.Value (C_Result);
      else
         return "";
      end if;
   end To_String;

   -------------------------
   -- Allocate_PS_Surface --
   -------------------------

   function Allocate_PS_Surface return Cairo_Surface_Ref is
   begin
      return new Cairo_PS_Surface;
   end Allocate_PS_Surface;

begin
   Register (CAIRO_SURFACE_TYPE_PS, Allocate_PS_Surface'Access);
end Cairo.Surface.PS;
