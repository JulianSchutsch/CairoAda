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

--  A Cairo_Pattern represents a source when drawing onto a
--  surface. There are different subtypes of Cairo_Pattern,
--  for different types of sources; for example,
--  New_RGB_Pattern creates a pattern for a solid
--  opaque color.
--
--  Other than various New_XXX_Pattern functions, some of the pattern types
--  can be implicitly created using various Set_Source_XXX functions;
--  for example Set_Source_RGB.
--
--  The type of a pattern can be queried with Get_Type.

with Ada.Finalization;

package Cairo.Pattern is

   pragma Elaborate_Body;

   type Cairo_Pattern (<>) is tagged limited private;
   type Cairo_Pattern_Ref is access all Cairo_Pattern'Class;
   type Cairo_Pattern_Handle is private;
   Cairo_Pattern_Null_Handle : constant Cairo_Pattern_Handle;

   ---------------
   -- User data --
   ---------------

   procedure Set_User_Data
     (Pattern : in out Cairo_Pattern'Class;
      Key : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy : Cairo_Destroy_Func;
      Status : out Cairo_Status);
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="key">the address of a Cairo_User_Data_Key to attach the user data to</parameter>
   --  <parameter name="user_data">the user data to attach to the Cairo_Pattern</parameter>
   --  <parameter name="destroy">a Cairo_Destroy_Func which will be called when the
   --  Cairo_Context is destroyed or when new user data is attached using the
   --  same key.</parameter>
   --
   --  Attach user data to pattern.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   function Get_User_Data
     (Pattern : Cairo_Pattern'Class;
      Key : Cairo_User_Data_Key)
      return Cairo_User_Data;
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="key">the address of the Cairo_User_Data_Key the user data was
   --  attached to</parameter>
   --
   --  Return user data previously attached to pattern using the
   --  specified key.  If no user data has been attached with the given
   --  key this function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4


   procedure Set_Matrix (Pattern : in out Cairo_Pattern'Class; Matrix : Cairo_Matrix);
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="matrix">a Cairo_Matrix</parameter>
   --
   --  Sets the pattern's transformation matrix to Matrix. This matrix is
   --  a transformation from user space to pattern space.
   --
   --  When a pattern is first created it always has the identity matrix
   --  for its transformation matrix, which means that pattern space is
   --  initially identical to user space.
   --
   --  Important: Please note that the direction of this transformation
   --  matrix is from user space to pattern space. This means that if you
   --  imagine the flow from a pattern to user space (and on to device
   --  space), then coordinates in that flow will be transformed by the
   --  inverse of the pattern matrix.
   --
   --  For example, if you want to make a pattern appear twice as large as
   --  it does by default the correct code to use is:
   --
   --  <informalexample><programlisting>
   --  Init_Scale (Matrix, 0.5, 0.5);
   --  Set_Matrix (Pattern, Matrix);
   --  </programlisting></informalexample>
   --
   --  Meanwhile, using values of 2.0 rather than 0.5 in the code above
   --  would cause the pattern to appear at half of its default size.
   --
   --  Also, please note the discussion of the user-space locking
   --  semantics of Set_Source.

   function Get_Matrix (Pattern : Cairo_Pattern'Class) return Cairo_Matrix;
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --
   --  Return the pattern's transformation matrix into Matrix.

   function Get_Status (Pattern : Cairo_Pattern'Class) return Cairo_Status;
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --
   --  Checks whether an error has previously occurred for this
   --  pattern.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, CAIRO_STATUS_NO_MEMORY, or
   --  CAIRO_STATUS_PATTERN_TYPE_MISMATCH.

   function Get_Type (Pattern : Cairo_Pattern'Class) return Cairo_Pattern_Type;
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --
   --  This function returns the type of pattern.
   --  See Cairo_Pattern_Type for available types.
   --
   --  Return value: The type of Pattern.
   --
   --  Since: 1.2


   ------------
   -- Handle --
   ------------

   function Ref (Handle : Cairo_Pattern_Handle) return Cairo_Pattern_Ref;
   -- Deprecated
   procedure Reset (Handle : in out Cairo_Pattern_Handle);
   function Is_Set (Handle : Cairo_Pattern_Handle) return Boolean;


   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function To_Handle
     (Ptr : Pattern_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Pattern_Handle;
   --  Create a Handle from a C allocated structure (cairo_pattern_t*).
   --  If Is_Referenced is False, then cairo_pattern_reference is called
   --  to increment the reference counter.

   function Ptr
     (Pattern : Cairo_Pattern'Class)
      return Pattern_Ptr;
   --  Return the pointer to the C allocated structure (cairo_pattern_t*).

private

   type Cairo_Pattern is tagged limited record
      Ptr : Pattern_Ptr;
   end record;

   type Cairo_Pattern_Handle is new Ada.Finalization.Controlled with record
      Ref : Cairo_Pattern_Ref;
   end record;
--   procedure Initialize (O : in out Cairo_Pattern_Handle);
   procedure Adjust (O : in out Cairo_Pattern_Handle);
   procedure Finalize (O : in out Cairo_Pattern_Handle);

   Cairo_Pattern_Null_Handle : constant Cairo_Pattern_Handle :=
     (Ada.Finalization.Controlled with Ref => null);

   -- Certain patterns have a corresponding Ada type derived from the root
   -- pattern (Cairo_Pattern).
   -- In order to build a correct Ada type from a C pointer, each specific Ada
   -- type must register an allocator.
   -- At most one allocator must be registered for each pattern type.
   type Pattern_Allocator is access function return Cairo_Pattern_Ref;
   procedure Register (Pattern_Type : Cairo_Pattern_Type; Allocator : Pattern_Allocator);

end Cairo.Pattern;
