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

--  A Cairo_Surface represents an image, either as the destination
--  of a drawing operation or as source when drawing onto another
--  surface. To draw to a Cairo_Surface, create a cairo context
--  with the surface as the target, using New_Context.
--
--  There are different subtypes of Cairo_Surface for
--  different drawing backends; for example, New_Image_Surface
--  creates a bitmap image in memory.
--  The type of a surface can be queried with Get_Type.

with Ada.Finalization;
with Ada.Streams;
with Cairo.Font_Options; use Cairo.Font_Options;

package Cairo.Surface is

   pragma Elaborate_Body;

   type Cairo_Surface (<>) is tagged limited private;
   --  Base class of all cairo surfaces.

   type Cairo_Surface_Ref is access all Cairo_Surface'Class;
   type Cairo_Surface_Handle is private;
   Cairo_Surface_Null_Handle : constant Cairo_Surface_Handle;

   function Create_Similar
     (Other : Cairo_Surface'Class;
      Content : Cairo_Content;
      Width : int;
      Height : int)
      return Cairo_Surface_Handle;
   --  <parameter name="other">an existing surface used to select the backend of the new surface</parameter>
   --  <parameter name="content">the content for the new surface</parameter>
   --  <parameter name="width">width of the new surface, (in device-space units)</parameter>
   --  <parameter name="height">height of the new surface (in device-space units)</parameter>
   --
   --  Create a new surface that is as compatible as possible with an
   --  existing surface. For example the new surface will have the same
   --  fallback resolution and font options as Other. Generally, the new
   --  surface will also use the same backend as Other, unless that is
   --  not possible for some reason. The type of the returned surface may
   --  be examined with Surface_Get_Type.
   --
   --  Initially the surface contents are all 0 (transparent if contents
   --  have transparency, black otherwise.)
   --
   --  Return value: a pointer to the newly allocated surface. The caller
   --  owns the surface and should call Surface_Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if Other is already in an error state
   --  or any other error occurs.


   ---------------
   -- User data --
   ---------------

   procedure Set_User_Data
     (Surface : in out Cairo_Surface'Class;
      Key : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy : Cairo_Destroy_Func;
      Status : out Cairo_Status);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="key">the address of a Cairo_User_Data_Key to attach the user data to</parameter>
   --  <parameter name="user_data">the user data to attach to the surface</parameter>
   --  <parameter name="destroy">a Cairo_Destroy_Func which will be called when the</parameter>
   --  surface is destroyed or when new user data is attached using the
   --  same key.
   --
   --  Attach user data to Surface.  To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for Data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.

   function Get_User_Data
     (Surface : Cairo_Surface'Class;
      Key : Cairo_User_Data_Key)
      return Cairo_User_Data;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="key">the address of the Cairo_User_Data_Key the user data was</parameter>
   --  attached to
   --
   --  Return user data previously attached to Surface using the specified
   --  key.  If no user data has been attached with the given key this
   --  function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.


   procedure Finish
     (Surface : in out Cairo_Surface'Class);
   --  <parameter name="surface">the Cairo_Surface to finish</parameter>
   --
   --  This function finishes the surface and drops all references to
   --  external resources.  For example, for the Xlib backend it means
   --  that cairo will no longer access the drawable, which can be freed.
   --  After calling Surface_Finish the only valid operations on a
   --  surface are getting and setting user, referencing and
   --  destroying, and flushing and finishing it.
   --  Further drawing to the surface will not affect the
   --  surface but will instead trigger a CAIRO_STATUS_SURFACE_FINISHED
   --  error.
   --
   --  When the last call to Surface_Destroy decreases the
   --  reference count to zero, cairo will call Surface_Finish if
   --  it hasn't been called already, before freeing the resources
   --  associated with the surface.

   procedure Flush
     (Surface : in out Cairo_Surface'Class);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  Do any pending drawing for the surface and also restore any
   --  temporary modification's cairo has made to the surface's
   --  state. This function must be called before switching from
   --  drawing on the surface with cairo to drawing on it directly
   --  with native APIs. If the surface doesn't support direct access,
   --  then this function does nothing.

   procedure Mark_Dirty
     (Surface : in out Cairo_Surface'Class);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  Tells cairo that drawing has been done to surface using means other
   --  than cairo, and that cairo should reread any cached areas. Note
   --  that you must call Surface_Flush before doing such drawing.

   procedure Mark_Dirty_Rectangle
     (Surface : in out Cairo_Surface'Class;
      X, Y : int;
      Width, Height : int);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="x">X coordinate of dirty rectangle</parameter>
   --  <parameter name="y">Y coordinate of dirty rectangle</parameter>
   --  <parameter name="width">width of dirty rectangle</parameter>
   --  <parameter name="height">height of dirty rectangle</parameter>
   --
   --  Like Surface_Mark_Dirty, but drawing has been done only to
   --  the specified rectangle, so that cairo can retain cached contents
   --  for other parts of the surface.
   --
   --  Any cached clip set on the surface will be reset by this function,
   --  to make sure that future cairo calls have the clip set that they
   --  expect.

   procedure Set_Device_Offset
     (Surface : in out Cairo_Surface'Class;
      Offset : Cairo_Tuple);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="x_offset">the offset in the X direction, in device units</parameter>
   --  <parameter name="y_offset">the offset in the Y direction, in device units</parameter>
   --
   --  Sets an offset that is added to the device coordinates determined
   --  by the CTM when drawing to Surface. One use case for this function
   --  is when we want to create a Cairo_Surface that redirects drawing
   --  for a portion of an onscreen surface to an offscreen surface in a
   --  way that is completely invisible to the user of the cairo
   --  API. Setting a transformation via Translate isn't
   --  sufficient to do this, since functions like
   --  Device_To_User will expose the hidden offset.
   --
   --  Note that the offset affects drawing to the surface as well as
   --  using the surface in a source pattern.

   procedure Set_Fallback_Resolution
     (Surface : in out Cairo_Surface'Class;
      X_Pixels_Per_Inch : double;
      Y_Pixels_Per_Inch : double);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="x_pixels_per_inch">horizontal setting for pixels per inch</parameter>
   --  <parameter name="y_pixels_per_inch">vertical setting for pixels per inch</parameter>
   --
   --  Set the horizontal and vertical resolution for image fallbacks.
   --
   --  When certain operations aren't supported natively by a backend,
   --  cairo will fallback by rendering operations to an image and then
   --  overlaying that image onto the output. For backends that are
   --  natively vector-oriented, this function can be used to set the
   --  resolution used for these image fallbacks, (larger values will
   --  result in more detailed images, but also larger file sizes).
   --
   --  Some examples of natively vector-oriented backends are the ps, pdf,
   --  and svg backends.
   --
   --  For backends that are natively raster-oriented, image fallbacks are
   --  still possible, but they are always performed at the native
   --  device resolution. So this function has no effect on those
   --  backends.
   --
   --  Note: The fallback resolution only takes effect at the time of
   --  completing a page (with Show_Page or Copy_Page) so
   --  there is currently no way to have more than one fallback resolution
   --  in effect on a single page.
   --
   --  The default fallback resoultion is 300 pixels per inch in both
   --  dimensions.
   --
   --  Since: 1.2

   procedure Copy_Page
     (Surface : in out Cairo_Surface'Class);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  Emits the current page for backends that support multiple pages,
   --  but doesn't clear it, so that the contents of the current page will
   --  be retained for the next page.  Use Surface_Show_Page if you
   --  want to get an empty page after the emission.
   --
   --  There is a convenience function for this that takes a Cairo_Context,
   --  namely Copy_Page.
   --
   --  Since: 1.6

   procedure Show_Page
     (Surface : in out Cairo_Surface'Class);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  Emits and clears the current page for backends that support multiple
   --  pages.  Use Surface_Copy_Page if you don't want to clear the page.
   --
   --  There is a convenience function for this that takes a Cairo_Context,
   --  namely Show_Page.
   --
   --  Since: 1.6



   procedure Write_To_PNG
     (Surface : Cairo_Surface'Class;
      Filename : String;
      Status : out Cairo_Status);
   --  <parameter name="surface">a Cairo_Surface with pixel contents</parameter>
   --  <parameter name="filename">the name of a file to write to</parameter>
   --
   --  Writes the contents of Surface to a new file Filename as a PNG
   --  image.
   --
   --  Return value: CAIRO_STATUS_SUCCESS if the PNG file was written
   --  successfully. Otherwise, CAIRO_STATUS_NO_MEMORY if memory could not
   --  be allocated for the operation or
   --  CAIRO_STATUS_SURFACE_TYPE_MISMATCH if the surface does not have
   --  pixel contents, or CAIRO_STATUS_WRITE_ERROR if an I/O error occurs
   --  while attempting to write the file.

   procedure Write_To_PNG_Stream
     (Surface : Cairo_Surface'Class;
      Stream : access Ada.Streams.Root_Stream_Type'Class;
      Status : out Cairo_Status);
   --  <parameter name="surface">a Cairo_Surface with pixel contents</parameter>
   --  <parameter name="write_func">a Cairo_Write_Func</parameter>
   --  <parameter name="closure">closure data for the write function</parameter>
   --
   --  Writes the image surface to the write function.
   --
   --  Return value: CAIRO_STATUS_SUCCESS if the PNG file was written
   --  successfully.  Otherwise, CAIRO_STATUS_NO_MEMORY is returned if
   --  memory could not be allocated for the operation,
   --  CAIRO_STATUS_SURFACE_TYPE_MISMATCH if the surface does not have
   --  pixel contents.
   -- WARNING: PNG functions don't seem to be supported on all cairo targets


   -------------
   -- Getters --
   -------------

   function Get_Status
     (Surface : Cairo_Surface'Class)
      return Cairo_Status;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  Checks whether an error has previously occurred for this
   --  surface.
   --
   --  Return value: CAIRO_STATUS_SUCCESS, CAIRO_STATUS_NULL_POINTER,
   --  CAIRO_STATUS_NO_MEMORY, CAIRO_STATUS_READ_ERROR,
   --  CAIRO_STATUS_INVALID_CONTENT, CAIRO_STATUS_INVALID_FORMAT, or
   --  CAIRO_STATUS_INVALID_VISUAL.

   function Get_Type
     (Surface : Cairo_Surface'Class)
      return Cairo_Surface_Type;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  This function returns the type of the backend used to create
   --  a surface. See Cairo_Surface_Type for available types.
   --
   --  Return value: The type of Surface.
   --
   --  Since: 1.2

   function Get_Content
     (Surface : Cairo_Surface'Class)
      return Cairo_Content;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  This function returns the content type of Surface which indicates
   --  whether the surface contains color and/or alpha information. See
   --  Cairo_Content.
   --
   --  Return value: The content type of Surface.
   --
   --  Since: 1.2

   function Get_Font_Options
     (Surface : Cairo_Surface'Class)
      return Cairo_Font_Options;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="options">a Cairo_Font_Options object into which to store</parameter>
   --    the retrieved options. All existing values are overwritten
   --
   --  Retrieves the default font rendering options for the surface.
   --  This allows display surfaces to report the correct subpixel order
   --  for rendering on them, print surfaces to disable hinting of
   --  metrics and so forth. The result can then be used with
   --  Scaled_Font_Create.

   function Get_Device_Offset
     (Surface : Cairo_Surface'Class)
      return Cairo_Tuple;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="x_offset">the offset in the X direction, in device units</parameter>
   --  <parameter name="y_offset">the offset in the Y direction, in device units</parameter>
   --
   --  This function returns the previous device offset set by
   --  Surface_Set_Device_Offset.
   --
   --  Since: 1.2
   -- Create 2 functions ?

   procedure Get_Fallback_Resolution
     (Surface : Cairo_Surface'Class;
      X_Pixels_Per_Inch : out double;
      Y_Pixels_Per_Inch : out double);
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="x_pixels_per_inch">horizontal pixels per inch</parameter>
   --  <parameter name="y_pixels_per_inch">vertical pixels per inch</parameter>
   --
   --  This function returns the previous fallback resolution set by
   --  Surface_Set_Fallback_Resolution, or default fallback
   --  resolution if never set.
   --
   --  Since: 1.8

   function Has_Show_Text_Glyphs
     (Surface : Cairo_Surface'Class)
      return Boolean;
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --
   --  Returns whether the surface supports
   --  sophisticated Show_Text_Glyphs operations.  That is,
   --  whether it actually uses the provided text and cluster data
   --  to a Show_Text_Glyphs call.
   --
   --  Note: Even if this function returns FALSE, a
   --  Show_Text_Glyphs operation targeted at Surface will
   --  still succeed.  It just will
   --  act like a Show_Glyphs operation.  Users can use this
   --  function to avoid computing UTF-8 text and cluster mapping if the
   --  target surface does not use it.
   --
   --  There is a convenience function for this that takes a Cairo_Context,
   --  namely Has_Show_Text_Glyphs.
   --
   --  Return value: TRUE if Surface supports
   --                Show_Text_Glyphs, FALSE otherwise
   --
   --  Since: 1.8


   ------------
   -- Handle --
   ------------

   function Ref (Handle : Cairo_Surface_Handle) return Cairo_Surface_Ref;
   -- Deprecated
   procedure Reset (Handle : in out Cairo_Surface_Handle);
   function Is_Set (Handle : Cairo_Surface_Handle) return Boolean;


   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function To_Handle
     (Ptr : Surface_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Surface_Handle;
   --  Create a Handle from a C allocated structure (cairo_surface_t*).
   --  If Is_Referenced is False, then cairo_surfce_reference is called
   --  to increment the reference counter.

   function Ptr
     (Surface : Cairo_Surface'Class)
      return Surface_Ptr;
   --  Return the pointer to the C allocated structure (cairo_surface_t*).

private

   type Cairo_Surface is tagged limited record
      Ptr : Surface_Ptr;
   end record;

   type Cairo_Surface_Handle is new Ada.Finalization.Controlled with record
      Ref : Cairo_Surface_Ref;
   end record;
--   procedure Initialize (O : in out Cairo_Surface_Handle);
   procedure Adjust (O : in out Cairo_Surface_Handle);
   procedure Finalize (O : in out Cairo_Surface_Handle);

   Cairo_Surface_Null_Handle : constant Cairo_Surface_Handle :=
     (Ada.Finalization.Controlled with Ref => null);

   --  Certain surfaces have a corresponding Ada type derived from the root
   --  surface (Cairo_Surface).
   --  In order to build a correct Ada type from a C pointer, each specific Ada
   --  type must register an allocator.
   --  At most one allocator must be registered for each surface type.
   type Surface_Allocator is access function return Cairo_Surface_Ref;
   procedure Register (Surface_Type : Cairo_Surface_Type; Allocator : Surface_Allocator);

end Cairo.Surface;
