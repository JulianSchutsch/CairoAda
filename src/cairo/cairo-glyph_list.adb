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

package body Cairo.Glyph_List is

   Default_Glyph : constant Cairo_Glyph := (0, 0.0, 0.0);

   package Glyph_Pointers is new Interfaces.C.Pointers
     (Natural,
      Cairo_Glyph,
      Cairo_Glyph_Array,
      Default_Glyph);

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Glyph_List : in out Cairo_Glyph_List;
      Length : in Natural)
   is
   begin
      Free (Glyph_List);
      if Length > 0 then
         Glyph_List.Ptr := cairo_glyph_allocate (int (Length));
         if Glyph_List.Ptr /= null then
            Glyph_List.Length := Length;
         end if;
         -- otherwise, Length is already set to 0
      else
         Glyph_List.Ptr := null;
         Glyph_List.Length := Length;
      end if;
   end Allocate;

   ----------
   -- Free --
   ----------

   procedure Free
     (Glyph_List : in out Cairo_Glyph_List)
   is
   begin
      if Glyph_List.Ptr /= null then
         cairo_glyph_free (Glyph_List.Ptr);
         Glyph_List.Ptr := null;
         Glyph_List.Length := 0;
      end if;
   end Free;

   ---------------
   -- Set_Glyph --
   ---------------

   procedure Set_Glyph
     (Glyph_List : in out Cairo_Glyph_List;
      Index : Natural;
      Glyph : Cairo_Glyph)
   is
   begin
      if Index < Glyph_List.Length then
         declare
            P : Glyph_Pointers.Pointer := Glyph_Pointers.Pointer (Glyph_List.Ptr);
            use Glyph_Pointers;
         begin
            P := P + Interfaces.C.ptrdiff_t (Index);
            P.all := Glyph;
         end;
      else
         raise Constraint_Error;
      end if;
   end Set_Glyph;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (Glyph_List : Cairo_Glyph_List) return Natural is
   begin
      return Glyph_List.Length;
   end Get_Length;

   ---------------
   -- Get_Glyph --
   ---------------

   function Get_Glyph
     (Glyph_List : Cairo_Glyph_List;
      Index : Natural)
      return Cairo_Glyph
   is
   begin
      if Index < Glyph_List.Length then
         declare
            P : Glyph_Pointers.Pointer := Glyph_Pointers.Pointer (Glyph_List.Ptr);
            use Glyph_Pointers;
         begin
            P := P + Interfaces.C.ptrdiff_t (Index);
            return P.all;
         end;
      else
         raise Constraint_Error;
      end if;
   end Get_Glyph;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Glyph_List : Cairo_Glyph_List)
      return Cairo_Glyph_Array
   is
   begin
      if Glyph_List.Ptr /= null then
         return Glyph_Pointers.Value
           (Glyph_Pointers.Pointer (Glyph_List.Ptr),
            Length => Interfaces.C.ptrdiff_t (Glyph_List.Length));
      else
         raise Constraint_Error;
      end if;
   end To_Array;

   -------------
   -- Set_Ptr --
   -------------

   procedure Set_Ptr
     (Glyph_List : in out Cairo_Glyph_List;
      Ptr : Glyph_Ptr;
      Length : Natural)
   is
   begin
      Glyph_List.Ptr := Ptr;
      Glyph_List.Length := Length;
   end Set_Ptr;

   ---------
   -- Ptr --
   ---------

   function Ptr
     (Glyph_List : Cairo_Glyph_List)
      return Glyph_Ptr
   is
   begin
      return Glyph_List.Ptr;
   end Ptr;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Cairo_Glyph_List) is
   begin
      Free (O);
   end Finalize;

end Cairo.Glyph_List;
