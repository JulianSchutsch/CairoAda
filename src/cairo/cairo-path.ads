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

--  This package defines Path.
--  A Path is defined as a sequence of elementary geometric orders (sections).
--  There are 4 types of geometric orders:
--  - MOVE_TO : is characterized by 1 point
--  - LINE_TO : is caracterized by 1 point
--  - CURVE_TO : is characterized by 3 points
--  - CLOSE_PATH : is self characterized.
--  There is no direct way to create a Path: a Path can be obtained
--  by calling Copy_Path and Copy_Path_Flat.
--  A path can also be used as an input value for Append_Path.
--  This package provides means to iterate over a Path and extract its
--  definition.
--  Memory associated to a Path is handled automatically.
--
--  Normal usage to iterate over Path should look like this:
--
--  Path : Cairo_Path;
--  Cursor : Cairo_Path_Cursor;
--
--  <retrieve the path, by adequate means>
--
--  Cursor := To_Cursor (Path);
--  while Is_Valid (Cursor) loop
--     case Get_Type (Cursor) is
--     when CAIRO_PATH_MOVE_TO =>
--        -- One can call Get_Point (Cursor, 1), if wanted
--     when CAIRO_PATH_LINE_TO =>
--        -- One can call Get_Point (Cursor, 1), if wanted
--     when CAIRO_PATH_CURVE_TO =>
--        -- One can call Get_Point (Cursor, 1..3), if wanted
--     when CAIRO_PATH_CLOSE_PATH =>
--     end case;
--     Next (Cursor);
--  end loop;

with Ada.Finalization;

package Cairo.Path is

   pragma Elaborate_Body;

   type Cairo_Path is limited private;
   --  Type used to represent a Path.

   function Get_Status
     (Path : Cairo_Path)
      return Cairo_Status;
   --  Return the path status.

   function Get_Num_Data
     (Path : Cairo_Path)
      return int;
   --  Return the number of data in the underlying array.
   --  This should not be normally used.
   --  The number of data is dependent on the section type.

   type Cairo_Path_Cursor is private;
   --  Type used to iterate over sections of a (valid) path.

   function To_Cursor
     (Path : Cairo_Path)
      return Cairo_Path_Cursor;
   --  Return a Cursor corresponding to the first section of the path.

   function Is_Valid
     (Cursor : Cairo_Path_Cursor)
      return Boolean;
   --  Return whether Cursor is valid or not.

   function Get_Type
     (Cursor : Cairo_Path_Cursor)
      return Cairo_Path_Data_Type;
   --  Return the type of the current path section, if any.
   --  Cursor must be valid.

   function Get_Point
     (Cursor : Cairo_Path_Cursor;
      Index : Positive)
      return Cairo_Tuple;
   --  Return the index-th point of the current section.
   --  Cursor must be valid.
   --  Index validity is checked relativeley to the type of the current section.
   --  - It can only be 1 when type is CAIRO_PATH_MOVE_TO or CAIRO_PATH_LINE_TO
   --  - It can be 1, 2 or 3 when type is CAIRO_PATH_CURVE_TO
   --  - There is no valid index value when type is CAIRO_PATH_CLOSE_PATH

   function Next
     (Cursor : Cairo_Path_Cursor)
      return Cairo_Path_Cursor;
   --  Return the Cursor correponding to the following section, if any.
   --  Cursor must be valid.
   --  Returned Cursor may be invalid, indicating the end or the Path has
   --  been reached.

   procedure Next
     (Cursor : in out Cairo_Path_Cursor);
   --  Advance the Cursor to the following path section, if any.
   --  Cursor must be valid.
   --  After call, modified Cursor may be invalid, indicating the end
   --  of the Path has been reached.


   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function Ptr (Path : Cairo_Path) return Path_Ptr;
   --  Return the pointer to the allocated C structure (cairo_path_t*)

   procedure Set_Ptr (Path : in out Cairo_Path; Ptr : Path_Ptr);
   --  Set the pointer to the allocated C structure (cairo_path_t*).

private
   type Cairo_Path is new Ada.Finalization.Limited_Controlled with record
      Ptr : Path_Ptr;
      --  Pointer to the allocated C data structure (cairo_path*) containing
      --  the path data.
      --  See body for more explanations (Path_Ptr must be converted to the
      --  right structure internally).
   end record;
   procedure Finalize (O : in out Cairo_Path);
   type Cairo_Path_Access is access constant Cairo_Path;
   for Cairo_Path_Access'Storage_Size use 0;

   type Cairo_Path_Cursor is record
      Path : Cairo_Path_Access;
      --  Access to the iterated Path
      Index : ptrdiff_t := 0;
      --  Data offset of the cursor
   end record;

end Cairo.Path;
