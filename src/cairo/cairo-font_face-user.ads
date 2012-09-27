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
with Cairo.Context; use Cairo.Context;
with Cairo.Scaled_Font; use Cairo.Scaled_Font;
with Cairo.Glyph_List; use Cairo.Glyph_List;
with Cairo.Text_Cluster_List; use Cairo.Text_Cluster_List;

package Cairo.Font_Face.User is

   type Cairo_User_Scaled_Font_Data is
   abstract new Ada.Finalization.Limited_Controlled with private;

   type Cairo_User_Scaled_Font_Data_Ref is
     access all Cairo_User_Scaled_Font_Data'Class;

   procedure Init
     (Data : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Context : Cairo_Context_Handle;
      Extents : in out Cairo_Font_Extents;
      Status : out Cairo_Status);

   procedure Render_Glyph
     (Data : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Glyph : unsigned_long;
      Context : Cairo_Context_Handle;
      Extents : in out Cairo_Text_Extents;
      Status : out Cairo_Status)
   is abstract;

   procedure Text_To_Glyphs
     (Data : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      UTF8 : String;
      Glyphs : in out Cairo_Glyph_List;
      Glyphs_Last : out Integer;
      Compute_Clusters : Boolean;
      Clusters : in out Cairo_Text_Cluster_List;
      Clusters_Last : out Integer;
      Cluster_Flags : out Cairo_Text_Cluster_Flags;
      Status : out Cairo_Status);

   procedure Unicode_To_Glyph
     (Data : in out Cairo_User_Scaled_Font_Data;
      Scaled_Font : Cairo_Scaled_Font_Handle;
      Unicode : unsigned_long;
      Glyph_Index : out unsigned_long;
      Status : out Cairo_Status);

   type Cairo_User_Font_Face (<>) is new Cairo_Font_Face with private;
   type Cairo_User_Font_Face_Ref is access all Cairo_User_Font_Face'Class;

   ------------------
   -- Construction --
   ------------------

   function New_User_Font_Face return Cairo_Font_Face_Handle;
   --
   --  Creates a new user font-face.
   --
   --  Use the setter functions to associate callbacks with the returned
   --  user font.  The only mandatory callback is render_glyph.
   --
   --  After the font-face is created, the user can attach arbitrary data
   --  (the actual font data) to it using Font_Face_Set_User_Data
   --  and access it from the user-font callbacks by using
   --  Scaled_Font_Get_Font_Face followed by
   --  Font_Face_Get_User_Data.
   --
   --  Return value: a newly created Cairo_Font_Face. Free with
   --   Font_Face_Destroy when you are done using it.
   --
   --  Since: 1.8


   -------------
   -- Setters --
   -------------

   procedure Set_Data
     (Font_Face : in out Cairo_User_Font_Face'Class;
      Data : Cairo_User_Scaled_Font_Data_Ref;
      Auto_Destroy : Boolean := False);


   -------------
   -- Getters --
   -------------

   function Get_Data
     (Font_Face : Cairo_User_Font_Face'Class)
     return Cairo_User_Scaled_Font_Data_Ref;

private
   type Cairo_User_Scaled_Font_Data is
   abstract new Ada.Finalization.Limited_Controlled with null record;

   type Cairo_User_Font_Face is new Cairo_Font_Face with record
      Data : Cairo_User_Scaled_Font_Data_Ref;
--      Auto_Destroy : Boolean;
   end record;

end Cairo.Font_Face.User;
