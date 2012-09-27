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

with Cairo.Support; use Cairo.Support;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;

package body Cairo.Path is

   -- Externally, Path_Ptr is declared as an access to Void.
   -- In fact, it is an access to C_Path.
   -- So we declare exact types and conversion functions.
   type C_Path is record
      Status : Cairo_Status;
      Data : Void_Ptr;
      Num_Data : int;
   end record;
   pragma Convention (C, C_Path);
   type C_Path_Ptr is access all C_Path;
   function To_C_Path_Ptr is new Ada.Unchecked_Conversion (Path_Ptr, C_Path_Ptr);

   -- In C, Data is declared as a union that can not be directly declared in Ada.
   -- The union can be interpreted either as Header or Point

   -- Header
   type Header is record
      Kind : Cairo_Path_Data_Type;
      Length : unsigned;
      Unused : Interfaces.Unsigned_64 := 0;
      --  64 unused bits.
      --  Declared to avoid a compiler warning telling that 64 bits of Header
      --  are unused.
   end record;
   for Header'Size use 128;
   pragma Convention (C, Header);
   Default_Header : constant Header := (CAIRO_PATH_CLOSE_PATH, 0, 0);
   type Header_Array is array (unsigned range <>) of aliased Header;
   package Header_Pointers is new Interfaces.C.Pointers (unsigned, Header, Header_Array, Default_Header);
   function To_Header_Ptr is new Ada.Unchecked_Conversion (Void_Ptr, Header_Pointers.Pointer);

   -- Tuple
   Default_Tuple : constant Cairo_Tuple := (0.0, 0.0);
   type Tuple_Array is array (unsigned range <>) of aliased Cairo_Tuple;
   package Tuple_Pointers is new Interfaces.C.Pointers (unsigned, Cairo_Tuple, Tuple_Array, Default_Tuple);
   function To_Tuple_Ptr is new Ada.Unchecked_Conversion (Void_Ptr, Tuple_Pointers.Pointer);

   Max_Index : constant array (Cairo_Path_Data_Type) of Natural :=
      (CAIRO_PATH_MOVE_TO    => 1,
       CAIRO_PATH_LINE_TO    => 1,
       CAIRO_PATH_CURVE_TO   => 3,
       CAIRO_PATH_CLOSE_PATH => 0);

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Path : Cairo_Path) return Cairo_Status is
   begin
      if Path.Ptr = null then
         return CAIRO_STATUS_NO_MEMORY;
      else
         return To_C_Path_Ptr (Path.Ptr).Status;
      end if;
   end Get_Status;

   ------------------
   -- Get_Num_Data --
   ------------------

   function Get_Num_Data (Path : Cairo_Path) return int is
   begin
      if Path.Ptr = null then
         return 0;
      else
         return To_C_Path_Ptr (Path.Ptr).Num_Data;
      end if;
   end Get_Num_Data;

   ---------------
   -- To_Cursor --
   ---------------

   function To_Cursor (Path : Cairo_Path) return Cairo_Path_Cursor is
   begin
      return Cairo_Path_Cursor'(Path'Unchecked_Access, 0);
   end To_Cursor;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Cursor : Cairo_Path_Cursor) return Boolean is
   begin
      if Cursor.Path = null then
         return False;
      elsif To_C_Path_Ptr (Cursor.Path.Ptr).Num_Data <= 0 then
         return False;
      else
         return int (Cursor.Index) < To_C_Path_Ptr (Cursor.Path.Ptr).Num_Data;
      end if;
   end Is_Valid;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Cursor : Cairo_Path_Cursor)
      return Cairo_Path_Data_Type
   is
      use Header_Pointers;
      Header_Ptr : Header_Pointers.Pointer;
   begin
      pragma Assert (Is_Valid (Cursor));
      if Cursor.Path = null then
         return CAIRO_PATH_CLOSE_PATH;
      else
         Header_Ptr := To_Header_Ptr (To_C_Path_Ptr (Cursor.Path.Ptr).Data) + Cursor.Index;
         return Header_Ptr.Kind;
      end if;
   end Get_Type;

   ---------------
   -- Get_Point --
   ---------------

   function Get_Point
     (Cursor : Cairo_Path_Cursor;
      Index : Positive)
      return Cairo_Tuple
   is
      use Tuple_Pointers;
      Tuple_Ptr : Tuple_Pointers.Pointer;
   begin
      pragma Assert (Is_Valid (Cursor));
      pragma Assert (Index <= Max_Index (Get_Type (Cursor)));
      Tuple_Ptr := To_Tuple_Ptr (To_C_Path_Ptr (Cursor.Path.Ptr).Data) + Cursor.Index + ptrdiff_t (Index);
      return Tuple_Ptr.all;
   end Get_Point;

   ----------
   -- Next --
   ----------

   function Next (Cursor : Cairo_Path_Cursor) return Cairo_Path_Cursor is
      use Header_Pointers;
      Header_Ptr : Header_Pointers.Pointer;
   begin
      pragma Assert (Is_Valid (Cursor));
      Header_Ptr := To_Header_Ptr (To_C_Path_Ptr (Cursor.Path.Ptr).Data) + Cursor.Index;
      return Cairo_Path_Cursor'(Cursor.Path, Cursor.Index + ptrdiff_t (Header_Ptr.Length));
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next (Cursor : in out Cairo_Path_Cursor) is
      use Header_Pointers;
      Header_Ptr : Header_Pointers.Pointer;
   begin
      pragma Assert (Is_Valid (Cursor));
      Header_Ptr := To_Header_Ptr (To_C_Path_Ptr (Cursor.Path.Ptr).Data) + Cursor.Index;
      Cursor.Index := Cursor.Index + ptrdiff_t (Header_Ptr.Length);
   end Next;

   ---------
   -- Ptr --
   ---------

   function Ptr (Path : Cairo_Path) return Path_Ptr is
   begin
      return Path.Ptr;
   end Ptr;

   -------------
   -- Set_Ptr --
   -------------

   procedure Set_Ptr (Path : in out Cairo_Path; Ptr : Path_Ptr) is
   begin
      if Path.Ptr /= Ptr then
         Finalize (Path);
         Path.Ptr := Ptr;
      end if;
   end Set_Ptr;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Path) is
   begin
      if O.Ptr /= null then
         cairo_path_destroy (O.Ptr);
         O.Ptr := null;
      end if;
   end Finalize;

end Cairo.Path;
