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

--  Cairo_Context contains the current state of the rendering device,
--  including coordinates of yet to be drawn shapes.
--
--  Cairo_Contexts are central to cairo and all drawing with cairo
--  is always done to a Cairto_Context object.
--
--  A user can not directly declare a Cairo_Context. One needs to declare
--  a Cairo_Context_Handle and call New�_Context.
--  A Cairo_Context can then be retrieved from the Handle.
--  Memory management is automatically handled, as for all other
--  classes in this binding.
--
--  As all function have a Cairo_Context('Class) as parameters, there are two
--  possible solutions:
--  1) Dereference systematically a Handle (with Ref) before calling
--     the functions. An example would be:
--
--     procedure Example1 is
--        Context_Handle : Cairo_Context_Handle;
--        ...
--     begin
--        Context_Handle := New_Context (...);
--        ...
--        Save (Ref (Context_Handle).all);
--        ...
--        New_Path (Ref (Context_Handle).all);
--        ...
--     end Example1;
--
--  2) Create a function/procedure that has a Cairo_Context('Class) as parameter
--     and Dereference the Handle once. Previous exampel would become:
--
--     procedure Example2 is
--        Context_Handle : Cairo_Context_Handle;
--        ...
--
--        procedure Draw (Context : in out Cairo_Context'Class) is
--        begin
--           ...
--           Save (Context);
--           ...
--           New_Path (Context);
--           ...
--        end Draw;
--     begin
--        Context_Handle := New_Context (...);
--        Draw (Ref (Context_Handle).all);
--     end Example2;
--
--  Solution 2 is the preferred one. It is simpler to write, and should be more
--  efficient, as there is no need to systematically check that the access
--  returned by Ref is valid (not null).

with Ada.Finalization;
with Cairo.Path; use Cairo.Path;
with Cairo.Rectangle_List; use Cairo.Rectangle_List;
with Cairo.Surface; use Cairo.Surface;
with Cairo.Font_Options; use Cairo.Font_Options;
with Cairo.Font_Face; use Cairo.Font_Face;
with Cairo.Scaled_Font; use Cairo.Scaled_Font;
with Cairo.Pattern; use Cairo.Pattern;

package Cairo.Context is

   pragma Elaborate_Body;

   type Cairo_Context (<>) is tagged limited private;
   --  Type representing a cairo context
   type Cairo_Context_Ref is access all Cairo_Context'Class;
   type Cairo_Context_Handle is private;
   --  Handle that gives access to a Cairo_Context and that takes care of
   --  memory management.
   Cairo_Context_Null_Handle : constant Cairo_Context_Handle;

   ------------------
   -- Construction --
   ------------------

   function New_Context
     (Target : access Cairo_Surface'Class)
      return Cairo_Context_Handle;
   --  function New_Context (Target : Cairo_Surface_Handle) return Cairo_Context_Handle;
   --  <parameter name="target">target surface for the context</parameter>
   --
   --  Creates a new Cairo_Context with all graphics state parameters set to
   --  default values and with Target as a target surface. The target
   --  surface should be constructed with a backend-specific function such
   --  as Image_Surface_Create (or any other New_XXX_Surface variant.
   --
   --  This function references Target, so you can immediately
   --  call Surface_Destroy on it if you don't need to
   --  maintain a separate reference to it.
   --
   --  Return value: a newly allocated Cairo_Context with a reference
   --  count of 1. The initial reference count should be released
   --  with Destroy when you are done using the Cairo_Context.
   --  This function never returns NULL. If memory cannot be
   --  allocated, a special Cairo_Context object will be returned on
   --  which Status returns CAIRO_STATUS_NO_MEMORY.
   --  You can use this object normally, but no drawing will
   --  be done.


   ---------------
   -- User data --
   ---------------

   procedure Set_User_Data
     (Context : in out Cairo_Context'Class;
      Key : Cairo_User_Data_Key;
      User_Data : Cairo_User_Data;
      Destroy : Cairo_Destroy_Func;
      Status : out Cairo_Status);
   --  <parameter name="Context">A Cairo_Context</parameter>
   --  <parameter name="key">The address of a Cairo_User_Data_Key to attach
   --  the user data to</parameter>
   --  <parameter name="user_data">The user data to attach to the
   --  Cairo_Context</parameter>
   --  <parameter name="destroy">a Cairo_Destroy_Func which will be called
   --  when the Cairo_Context is destroyed or when new user data is
   --  attached using the same key.</parameter>
   --
   --  Attach user data to Context. To remove user data from a surface,
   --  call this function with the key that was used to set it and NULL
   --  for Data.
   --
   --  Return value: CAIRO_STATUS_SUCCESS or CAIRO_STATUS_NO_MEMORY if a
   --  slot could not be allocated for the user data.
   --
   --  Since: 1.4

   function Get_User_Data
     (Context : Cairo_Context'Class;
      Key : Cairo_User_Data_Key)
      return Cairo_User_Data;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="key">the address of the Cairo_User_Data_Key the user
   --  data was attached to</parameter>
   --
   --  Return user data previously attached to Context using the specified
   --  key. If no user data has been attached with the given key this
   --  function returns NULL.
   --
   --  Return value: the user data previously attached or NULL.
   --
   --  Since: 1.4


   -----------
   -- State --
   -----------

   procedure Save (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --
   --  Makes a copy of the current state of Context and saves it
   --  on an internal stack of saved states for Context. When
   --  Restore is called, Context will be restored to
   --  the saved state. Multiple calls to Save and
   --  Restore can be nested; each call to Restore
   --  restores the state from the matching paired Save.
   --
   --  It isn't necessary to clear all saved states before
   --  a Cairo_Context is freed. If the reference count of a Cairo_Context
   --  drops to zero in response to a call to Destroy,
   --  any saved states will be freed along with the Cairo_Context.

   procedure Restore (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --
   --  Restores Context to the state saved by a preceding call to
   --  Save and removes that state from the stack of
   --  saved states.

   procedure Push_Group (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Temporarily redirects drawing to an intermediate surface known as a
   --  group. The redirection lasts until the group is completed by a call
   --  to Pop_Group or Pop_Group_To_Source. These calls
   --  provide the result of any drawing to the group as a pattern,
   --  (either as an explicit object, or set as the source pattern).
   --
   --  This group functionality can be convenient for performing
   --  intermediate compositing. One common use of a group is to render
   --  objects as opaque within the group, (so that they occlude each
   --  other), and then blend the result with translucence onto the
   --  destination.
   --
   --  Groups can be nested arbitrarily deep by making balanced calls to
   --  Push_Group/Pop_Group. Each call pushes/pops the new
   --  target group onto/from a stack.
   --
   --  The Push_Group function calls Save so that any
   --  changes to the graphics state will not be visible outside the
   --  group, (the pop_group functions call Restore).
   --
   --  By default the intermediate group will have a content type of
   --  CAIRO_CONTENT_COLOR_ALPHA. Other content types can be chosen for
   --  the group by using Push_Group_With_Content instead.
   --
   --  As an example, here is how one might fill and stroke a path with
   --  translucence, but without any portion of the fill being visible
   --  under the stroke:
   --
   --  <informalexample><programlisting>
   --  cairo_push_group (Context);
   --  cairo_set_source (Context, fill_pattern);
   --  cairo_fill_preserve (Context);
   --  cairo_set_source (Context, stroke_pattern);
   --  cairo_stroke (Context);
   --  cairo_pop_group_to_source (Context);
   --  cairo_paint_with_alpha (Context, alpha);
   --  </programlisting></informalexample>
   --
   --  Since: 1.2

   procedure Push_Group_With_Content
     (Context : in out Cairo_Context'Class;
      Content : Cairo_Content);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="content">a Cairo_Content indicating the type of
   --  group that will be created</parameter>
   --
   --  Temporarily redirects drawing to an intermediate surface known as a
   --  group. The redirection lasts until the group is completed by a call
   --  to Pop_Group or Pop_Group_To_Source. These calls
   --  provide the result of any drawing to the group as a pattern,
   --  (either as an explicit object, or set as the source pattern).
   --
   --  The group will have a content type of Content. The ability to
   --  control this content type is the only distinction between this
   --  function and Push_Group which you should see for a more
   --  detailed description of group rendering.
   --
   --  Since: 1.2

   procedure Pop_Group
     (Context : in out Cairo_Context'Class;
      Pattern : out Cairo_Pattern_Handle);
   --  <parameter name="Context">A cairo context</parameter>
   --  <parameter name="Pattern">A newly created (surface) pattern containing
   --  the results of all drawing operations performed to the group. The
   --  caller owns the returned object and should call
   --  Pattern_Destroy when finished with it.</parameter>
   --
   --  Terminates the redirection begun by a call to Push_Group or
   --  Push_Group_With_Content and returns a new pattern
   --  containing the results of all drawing operations performed to the
   --  group.
   --
   --  The Pop_Group procedure calls Restore, (balancing a
   --  call to Save by the push_group function), so that any
   --  changes to the graphics state will not be visible outside the
   --  group.
   --
   --
   --  Since: 1.2

   procedure Pop_Group_To_Source (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Terminates the redirection begun by a call to Push_Group or
   --  Push_Group_With_Content and installs the resulting pattern
   --  as the source pattern in the given cairo context.
   --
   --  The behavior of this function is equivalent to the sequence of
   --  operations:
   --
   --  <informalexample><programlisting>
   --  Cairo_Pattern *group = cairo_pop_group (Context);
   --  cairo_set_source (Context, group);
   --  cairo_pattern_destroy (group);
   --  </programlisting></informalexample>
   --
   --  but is more convenient as their is no need for a variable to store
   --  the short-lived pointer to the pattern.
   --
   --  The Pop_Group function calls Restore, (balancing a
   --  call to Save by the push_group function), so that any
   --  changes to the graphics state will not be visible outside the
   --  group.
   --
   --  Since: 1.2

   procedure Set_Operator
     (Context : in out Cairo_Context'Class;
      Operator : Cairo_Operator);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="op">a compositing operator, specified as a Cairo_Operator</parameter>
   --
   --  Sets the compositing operator to be used for all drawing
   --  operations. See Cairo_Operator for details on the semantics of
   --  each available compositing operator.
   --
   --  The default operator is CAIRO_OPERATOR_OVER.

   procedure Set_Source
     (Context : in out Cairo_Context'Class;
      Source : access Cairo_Pattern'Class);
   procedure Set_Source_RGB
     (Context : in out Cairo_Context'Class;
      Red, Green, Blue : double);
   procedure Set_Source_RGBA
     (Context : in out Cairo_Context'Class;
      Red, Green, Blue, Alpha : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="red">red component of color</parameter>
   --  <parameter name="green">green component of color</parameter>
   --  <parameter name="blue">blue component of color</parameter>
   --  <parameter name="alpha">alpha component of color</parameter>
   --
   --  Sets the source pattern within Context to a translucent color. This
   --  color will then be used for any subsequent drawing operation until
   --  a new source pattern is set.
   --
   --  The color and alpha components are floating point numbers in the
   --  range 0 to 1. If the values passed in are outside that range, they
   --  will be clamped.
   --
   --  The default source pattern is opaque black, (that is, it is
   --  equivalent to cairo_set_source_rgba(Context, 0.0, 0.0, 0.0, 1.0)).

   procedure Set_Source_Surface
     (Context : in out Cairo_Context'Class;
      Surface : access Cairo_Surface'Class;
      X, Y : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="surface">a surface to be used to set the source pattern</parameter>
   --  <parameter name="x">User-space X coordinate for surface origin</parameter>
   --  <parameter name="y">User-space Y coordinate for surface origin</parameter>
   --
   --  This is a convenience function for creating a pattern from Surface
   --  and setting it as the source in Context with Set_Source.
   --
   --  The X and Y parameters give the user-space coordinate at which
   --  the surface origin should appear. (The surface origin is its
   --  upper-left corner before any transformation has been applied.) The
   --  X and Y patterns are negated and then set as translation values
   --  in the pattern matrix.
   --
   --  Other than the initial translation pattern matrix, as described
   --  above, all other pattern attributes, (such as its extend mode), are
   --  set to the default values as in Pattern_Create_For_Surface.
   --  The resulting pattern can be queried with Get_Source so
   --  that these attributes can be modified if desired, (eg. to create a
   --  repeating pattern with Pattern_Set_Extend).

   procedure Set_Tolerance
     (Context : in out Cairo_Context'Class;
      Tolerance : double);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="tolerance">the tolerance, in device units (typically pixels)</parameter>
   --
   --  Sets the tolerance used when converting paths into trapezoids.
   --  Curved segments of the path will be subdivided until the maximum
   --  deviation between the original path and the polygonal approximation
   --  is less than Tolerance. The default value is 0.1. A larger
   --  value will give better performance, a smaller value, better
   --  appearance. (Reducing the value from the default value of 0.1
   --  is unlikely to improve appearance significantly.)

   procedure Set_Antialias
     (Context : in out Cairo_Context'Class;
      Antialias : Cairo_Antialias);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="antialias">the new antialiasing mode</parameter>
   --
   --  Set the antialiasing mode of the rasterizer used for drawing shapes.
   --  This value is a hint, and a particular backend may or may not support
   --  a particular value.  At the current time, no backend supports
   --  CAIRO_ANTIALIAS_SUBPIXEL when drawing shapes.
   --
   --  Note that this option does not affect text rendering, instead see
   --  Font_Options_Set_Antialias.

   procedure Set_Fill_Rule
     (Context : in out Cairo_Context'Class;
      Fill_Rule : Cairo_Fill_Rule);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="fill_rule">a fill rule, specified as a Cairo_Fill_Rule</parameter>
   --
   --  Set the current fill rule within the cairo context. The fill rule
   --  is used to determine which regions are inside or outside a complex
   --  (potentially self-intersecting) path. The current fill rule affects
   --  both Fill and Clip. See Cairo_Fill_Rule for details
   --  on the semantics of each available fill rule.
   --
   --  The default fill rule is CAIRO_FILL_RULE_WINDING.

   procedure Set_Line_Width
     (Context : in out Cairo_Context'Class;
      Width : double);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="width">a line width</parameter>
   --
   --  Sets the current line width within the cairo context. The line
   --  width value specifies the diameter of a pen that is circular in
   --  user space, (though device-space pen may be an ellipse in general
   --  due to scaling/shear/rotation of the CTM).
   --
   --  Note: When the description above refers to user space and CTM it
   --  refers to the user space and CTM in effect at the time of the
   --  stroking operation, not the user space and CTM in effect at the
   --  time of the call to Set_Line_Width. The simplest usage
   --  makes both of these spaces identical. That is, if there is no
   --  change to the CTM between a call to Set_Line_Width and the
   --  stroking operation, then one can just pass user-space values to
   --  Set_Line_Width and ignore this note.
   --
   --  As with the other stroke parameters, the current line width is
   --  examined by Stroke, Stroke_Extents, and
   --  Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default line width value is 2.0.

   procedure Set_Line_Cap
     (Context : in out Cairo_Context'Class;
      Line_Cap : Cairo_Line_Cap);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="line_cap">a line cap style</parameter>
   --
   --  Sets the current line cap style within the cairo context. See
   --  Cairo_Line_Cap for details about how the available line cap
   --  styles are drawn.
   --
   --  As with the other stroke parameters, the current line cap style is
   --  examined by Stroke, Stroke_Extents, and
   --  Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default line cap style is CAIRO_LINE_CAP_BUTT.

   procedure Set_Line_Join
     (Context : in out Cairo_Context'Class;
      Line_Join : Cairo_Line_Join);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="line_join">a line join style</parameter>
   --
   --  Sets the current line join style within the cairo context. See
   --  Cairo_Line_Join for details about how the available line join
   --  styles are drawn.
   --
   --  As with the other stroke parameters, the current line join style is
   --  examined by Stroke, Stroke_Extents, and
   --  Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default line join style is CAIRO_LINE_JOIN_MITER.

   procedure Set_Dash
     (Context : in out Cairo_Context'Class;
      Dashes : double_Array;
      Offset : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="dashes">an array specifying alternate lengths of on and off stroke portions</parameter>
   --  <parameter name="num_dashes">the length of the dashes array</parameter>
   --  <parameter name="offset">an offset into the dash pattern at which the stroke should start</parameter>
   --
   --  Sets the dash pattern to be used by Stroke. A dash pattern
   --  is specified by Dashes, an array of positive values. Each value
   --  provides the length of alternate "on" and "off" portions of the
   --  stroke. The Offset specifies an offset into the pattern at which
   --  the stroke begins.
   --
   --  Each "on" segment will have caps applied as if the segment were a
   --  separate sub-path. In particular, it is valid to use an "on" length
   --  of 0.0 with CAIRO_LINE_CAP_ROUND or CAIRO_LINE_CAP_SQUARE in order
   --  to distributed dots or squares along a path.
   --
   --  Note: The length values are in user-space units as evaluated at the
   --  time of stroking. This is not necessarily the same as the user
   --  space at the time of Set_Dash.
   --
   --  If Num_dashes is 0 dashing is disabled.
   --
   --  If Num_dashes is 1 a symmetric pattern is assumed with alternating
   --  on and off portions of the size specified by the single value in
   --  Dashes.
   --
   --  If any value in Dashes is negative, or if all values are 0, then
   --  Context will be put into an error state with a status of
   --  CAIRO_STATUS_INVALID_DASH.

   procedure Set_Miter_Limit
     (Context : in out Cairo_Context'Class;
      Limit : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="limit">miter limit to set</parameter>
   --
   --  Sets the current miter limit within the cairo context.
   --
   --  If the current line join style is set to CAIRO_LINE_JOIN_MITER
   --  (see Set_Line_Join), the miter limit is used to determine
   --  whether the lines should be joined with a bevel instead of a miter.
   --  Cairo divides the length of the miter by the line width.
   --  If the result is greater than the miter limit, the style is
   --  converted to a bevel.
   --
   --  As with the other stroke parameters, the current line miter limit is
   --  examined by Stroke, Stroke_Extents, and
   --  Stroke_To_Path, but does not have any effect during path
   --  construction.
   --
   --  The default miter limit value is 10.0, which will convert joins
   --  with interior angles less than 11 degrees to bevels instead of
   --  miters. For reference, a miter limit of 2.0 makes the miter cutoff
   --  at 60 degrees, and a miter limit of 1.414 makes the cutoff at 90
   --  degrees.
   --
   --  A miter limit for a desired angle can be computed as: miter limit =
   --  1/sin(angle/2)

   ---------------------
   -- Transformations --
   ---------------------

   procedure Translate
     (Context : in out Cairo_Context'Class;
      T : Cairo_Tuple);
   procedure Translate
     (Context : in out Cairo_Context'Class;
      TX, TY : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="tx">amount to translate in the X direction</parameter>
   --  <parameter name="ty">amount to translate in the Y direction</parameter>
   --
   --  Modifies the current transformation matrix (CTM) by translating the
   --  user-space origin by (Tx, Ty). This offset is interpreted as a
   --  user-space coordinate according to the CTM in place before the new
   --  call to Translate. In other words, the translation of the
   --  user-space origin takes place after any existing transformation.

   procedure Scale
     (Context : in out Cairo_Context'Class;
      SX, SY : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="sx">scale factor for the X dimension</parameter>
   --  <parameter name="sy">scale factor for the Y dimension</parameter>
   --
   --  Modifies the current transformation matrix (CTM) by scaling the X
   --  and Y user-space axes by Sx and Sy respectively. The scaling of
   --  the axes takes place after any existing transformation of user
   --  space.

   procedure Rotate
     (Context : in out Cairo_Context'Class;
      Angle : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="angle">angle (in radians) by which the user-space axes will be</parameter>
   --  rotated
   --
   --  Modifies the current transformation matrix (CTM) by rotating the
   --  user-space axes by Angle radians. The rotation of the axes takes
   --  places after any existing transformation of user space. The
   --  rotation direction for positive angles is from the positive X axis
   --  toward the positive Y axis.

   procedure Transform
     (Context : in out Cairo_Context'Class;
      Matrix : Cairo_Matrix);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="matrix">a transformation to be applied to the user-space axes</parameter>
   --
   --  Modifies the current transformation matrix (CTM) by applying
   --  Matrix as an additional transformation. The new transformation of
   --  user space takes place after any existing transformation.

   procedure Set_Matrix
     (Context : in out Cairo_Context'Class;
      Matrix : Cairo_Matrix);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="matrix">a transformation matrix from user space to device space</parameter>
   --
   --  Modifies the current transformation matrix (CTM) by setting it
   --  equal to Matrix.

   procedure Set_Identity_Matrix (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Resets the current transformation matrix (CTM) by setting it equal
   --  to the identity matrix. That is, the user-space and device-space
   --  axes will be aligned and one user-space unit will transform to one
   --  device-space unit.

   ----------
   -- Path --
   ----------

   procedure New_Path (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Clears the current path. After this call there will be no path and
   --  no current point.

   procedure Move_To
     (Context : in out Cairo_Context'Class;
      Point : Cairo_Tuple);
   procedure Move_To
     (Context : in out Cairo_Context'Class;
      X, Y : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">the X coordinate of the new position</parameter>
   --  <parameter name="y">the Y coordinate of the new position</parameter>
   --
   --  Begin a new sub-path. After this call the current point will be (X, Y).

   procedure New_Sub_Path (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Begin a new sub-path. Note that the existing path is not
   --  affected. After this call there will be no current point.
   --
   --  In many cases, this call is not needed since new sub-paths are
   --  frequently started with Move_To.
   --
   --  A call to New_Sub_Path is particularly useful when
   --  beginning a new sub-path with one of the Arc calls. This
   --  makes things easier as it is no longer necessary to manually
   --  compute the arc's initial coordinates for a call to
   --  Move_To.
   --
   --  Since: 1.2

   procedure Line_To
     (Context : in out Cairo_Context'Class;
      Point : Cairo_Tuple);
   procedure Line_To
     (Context : in out Cairo_Context'Class;
      X, Y : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">the X coordinate of the end of the new line</parameter>
   --  <parameter name="y">the Y coordinate of the end of the new line</parameter>
   --
   --  Adds a line to the path from the current point to position (X, Y)
   --  in user-space coordinates. After this call the current point
   --  will be (X, Y).
   --
   --  If there is no current point before the call to Line_To
   --  this function will behave as cairo_move_to(Context, X, Y).

   procedure Curve_To
     (Context : in out Cairo_Context'Class;
      Point1, Point2, Point3 : Cairo_Tuple);
   procedure Curve_To
     (Context : in out Cairo_Context'Class;
      X1, Y1, X2, Y2, X3, Y3 : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x1">the X coordinate of the first control point</parameter>
   --  <parameter name="y1">the Y coordinate of the first control point</parameter>
   --  <parameter name="x2">the X coordinate of the second control point</parameter>
   --  <parameter name="y2">the Y coordinate of the second control point</parameter>
   --  <parameter name="x3">the X coordinate of the end of the curve</parameter>
   --  <parameter name="y3">the Y coordinate of the end of the curve</parameter>
   --
   --  Adds a cubic B�zier spline to the path from the current point to
   --  position (X3, Y3) in user-space coordinates, using (X1, Y1) and
   --  (X2, Y2) as the control points. After this call the current point
   --  will be (X3, Y3).
   --
   --  If there is no current point before the call to Curve_To
   --  this function will behave as if preceded by a call to
   --  cairo_move_to(Context, X1, Y1).

   procedure Arc
     (Context : in out Cairo_Context'Class;
      Center : Cairo_Tuple;
      Radius : double;
      Angle1, Angle2 : double);
   procedure Arc
     (Context : in out Cairo_Context'Class;
      Center_X, Center_Y : double;
      Radius : double;
      Angle1, Angle2 : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="Center_X">X position of the center of the arc</parameter>
   --  <parameter name="Center_Y">Y position of the center of the arc</parameter>
   --  <parameter name="radius">the radius of the arc</parameter>
   --  <parameter name="angle1">the start angle, in radians</parameter>
   --  <parameter name="angle2">the end angle, in radians</parameter>
   --
   --  Adds a circular arc of the given Radius to the current path.  The
   --  arc is centered at (Center_X, Center_Y), begins at Angle1 and proceeds in
   --  the direction of increasing angles to end at Angle2. If Angle2 is
   --  less than Angle1 it will be progressively increased by 2*M_PI
   --  until it is greater than Angle1.
   --
   --  If there is a current point, an initial line segment will be added
   --  to the path to connect the current point to the beginning of the
   --  arc. If this initial line is undesired, it can be avoided by
   --  calling New_Sub_Path before calling Arc.
   --
   --  Angles are measured in radians. An angle of 0.0 is in the direction
   --  of the positive X axis (in user space). An angle of M_PI/2.0 radians
   --  (90 degrees) is in the direction of the positive Y axis (in
   --  user space). Angles increase in the direction from the positive X
   --  axis toward the positive Y axis. So with the default transformation
   --  matrix, angles increase in a clockwise direction.
   --
   --  (To convert from degrees to radians, use <literal>degrees * (M_PI /
   --  180.)</literal>.)
   --
   --  This function gives the arc in the direction of increasing angles;
   --  see Arc_Negative to get the arc in the direction of
   --  decreasing angles.
   --
   --  The arc is circular in user space. To achieve an elliptical arc,
   --  you can scale the current transformation matrix by different
   --  amounts in the X and Y directions. For example, to draw an ellipse
   --  in the box given by X, Y, Width, Height:
   --
   --  <informalexample><programlisting>
   --  Save (Context);
   --  Translate (Context, x + width / 2., y + height / 2.);
   --  Scale (Context, width / 2., height / 2.);
   --  Arc (Context, 0., 0., 1., 0., 2 * M_PI);
   --  Restore (Context);
   --  </programlisting></informalexample>

   procedure Arc_Negative
     (Context : in out Cairo_Context'Class;
      Center : Cairo_Tuple;
      Radius : double;
      Angle1, Angle2 : double);
   procedure Arc_Negative
     (Context : in out Cairo_Context'Class;
      Center_X, Center_Y : double;
      Radius : double;
      Angle1, Angle2 : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="Center_X">X position of the center of the arc</parameter>
   --  <parameter name="Center_Y">Y position of the center of the arc</parameter>
   --  <parameter name="radius">the radius of the arc</parameter>
   --  <parameter name="angle1">the start angle, in radians</parameter>
   --  <parameter name="angle2">the end angle, in radians</parameter>
   --
   --  Adds a circular arc of the given Radius to the current path.  The
   --  arc is centered at (Center_X, Center_Y), begins at Angle1 and proceeds in
   --  the direction of decreasing angles to end at Angle2. If Angle2 is
   --  greater than Angle1 it will be progressively decreased by 2*M_PI
   --  until it is less than Angle1.
   --
   --  See Arc for more details. This function differs only in the
   --  direction of the arc between the two angles.

   procedure Rel_Move_To
     (Context : in out Cairo_Context'Class;
      Offset : Cairo_Tuple);
   procedure Rel_Move_To
     (Context : in out Cairo_Context'Class;
      Offset_X, Offset_Y : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="Offset_X">the X offset</parameter>
   --  <parameter name="Offset_Y">the Y offset</parameter>
   --
   --  Begin a new sub-path. After this call the current point will offset
   --  by (Offset_X, Offset_Y).
   --
   --  Given a current point of (X, Y), Rel_Move_To(Context, DX, DY)
   --  is logically equivalent to Move_to(Context, X + DX, Y + DY).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause Context to shutdown with a status of
   --  CAIRO_STATUS_NO_CURRENT_POINT.

   procedure Rel_Line_To
     (Context : in out Cairo_Context'Class;
      Offset : Cairo_Tuple);
   procedure Rel_Line_To
     (Context : in out Cairo_Context'Class;
      Offset_X, Offset_Y : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="Offset_X">the X offset to the end of the new line</parameter>
   --  <parameter name="Offset_Y">the Y offset to the end of the new line</parameter>
   --
   --  Relative-coordinate version of Line_To. Adds a line to the
   --  path from the current point to a point that is offset from the
   --  current point by (Offset_X, Offset_Y) in user space. After this call the
   --  current point will be offset by (Offset_X, Offset_Y).
   --
   --  Given a current point of (X, Y), Rel_Line_To(Context, DX, DY)
   --  is logically equivalent to Line_To(Context, X + DX, Y + DY).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause Context to shutdown with a status of
   --  CAIRO_STATUS_NO_CURRENT_POINT.

   procedure Rel_Curve_To
     (Context : in out Cairo_Context'Class;
      Offset1, Offset2, Offset3 : Cairo_Tuple);
   procedure Rel_Curve_To
     (Context : in out Cairo_Context'Class;
      Offset1_X, Offset1_Y, Offset2_X, Offset2_Y, Offset3_X, Offset3_Y : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="dx1">the X offset to the first control point</parameter>
   --  <parameter name="dy1">the Y offset to the first control point</parameter>
   --  <parameter name="dx2">the X offset to the second control point</parameter>
   --  <parameter name="dy2">the Y offset to the second control point</parameter>
   --  <parameter name="dx3">the X offset to the end of the curve</parameter>
   --  <parameter name="dy3">the Y offset to the end of the curve</parameter>
   --
   --  Relative-coordinate version of Curve_To. All offsets are
   --  relative to the current point. Adds a cubic B�zier spline to the
   --  path from the current point to a point offset from the current
   --  point by (Dx3, Dy3), using points offset by (Dx1, Dy1) and
   --  (Dx2, Dy2) as the control points. After this call the current
   --  point will be offset by (Dx3, Dy3).
   --
   --  Given a current point of (x, y), cairo_rel_curve_to(Context, Dx1,
   --  Dy1, Dx2, Dy2, Dx3, Dy3) is logically equivalent to
   --  cairo_curve_to(Context, x+Dx1, y+Dy1, x+Dx2, y+Dy2, x+Dx3, y+Dy3).
   --
   --  It is an error to call this function with no current point. Doing
   --  so will cause Context to shutdown with a status of
   --  CAIRO_STATUS_NO_CURRENT_POINT.

   procedure Rectangle
     (Context : in out Cairo_Context'Class;
      Point : Cairo_Tuple;
      Width, Height : double);
   procedure Rectangle
     (Context : in out Cairo_Context'Class;
      X, Y : double;
      Width, Height : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">the X coordinate of the top left corner of the rectangle</parameter>
   --  <parameter name="y">the Y coordinate to the top left corner of the rectangle</parameter>
   --  <parameter name="width">the width of the rectangle</parameter>
   --  <parameter name="height">the height of the rectangle</parameter>
   --
   --  Adds a closed sub-path rectangle of the given size to the current
   --  path at position (X, Y) in user-space coordinates.
   --
   --  This function is logically equivalent to:
   --  <informalexample><programlisting>
   --  Move_to (Context, x, y);
   --  Rel_Line_To (Context, width, 0);
   --  Rel_Line_To (Context, 0, height);
   --  Rel_Line_To (Context, -width, 0);
   --  Close_Path (Context);
   --  </programlisting></informalexample>

   procedure Close_Path (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Adds a line segment to the path from the current point to the
   --  beginning of the current sub-path, (the most recent point passed to
   --  Move_To), and closes this sub-path. After this call the
   --  current point will be at the joined endpoint of the sub-path.
   --
   --  The behavior of Close_Path is distinct from simply calling
   --  Line_To with the equivalent coordinate in the case of
   --  stroking. When a closed sub-path is stroked, there are no caps on
   --  the ends of the sub-path. Instead, there is a line join connecting
   --  the final and initial segments of the sub-path.
   --
   --  If there is no current point before the call to Close_Path,
   --  this function will have no effect.
   --
   --  Note: As of cairo version 1.2.4 any call to Close_Path will
   --  place an explicit MOVE_TO element into the path immediately after
   --  the CLOSE_PATH element, (which can be seen in Copy_Path for
   --  example). This can simplify path processing in some cases as it may
   --  not be necessary to save the "last move_to point" during processing
   --  as the MOVE_TO immediately after the CLOSE_PATH will provide that
   --  point.

   procedure Append_Path
     (Context : in out Cairo_Context'Class;
      Path : Cairo_Path);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="path">path to be appended</parameter>
   --
   --  Append the Path onto the current path. The Path may be either the
   --  return value from one of Copy_Path or
   --  Copy_Path_Flat or it may be constructed manually.  See
   --  Cairo_Path for details on how the path data structure should be
   --  initialized, and note that <literal>path->status</literal> must be
   --  initialized to CAIRO_STATUS_SUCCESS.
   -- NYI: Arc_To
   -- NYI: Stroke_To_Path

   --------------
   -- Painting --
   --------------

   procedure Paint (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  A drawing operator that paints the current source everywhere within
   --  the current clip region.

   procedure Paint_With_Alpha
     (Context : in out Cairo_Context'Class;
      Alpha : double);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="alpha">alpha value, between 0 (transparent) and 1 (opaque)</parameter>
   --
   --  A drawing operator that paints the current source everywhere within
   --  the current clip region using a mask of constant alpha value
   --  Alpha. The effect is similar to Paint, but the drawing
   --  is faded out using the alpha value.

   procedure Mask
     (Context : in out Cairo_Context'Class;
      Pattern : access Cairo_Pattern'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --
   --  A drawing operator that paints the current source
   --  using the alpha channel of Pattern as a mask. (Opaque
   --  areas of Pattern are painted with the source, transparent
   --  areas are not painted.)

   procedure Mask_Surface
     (Context : in out Cairo_Context'Class;
      Surface : access Cairo_Surface'Class; Surface_Origin : Cairo_Tuple);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="surface">a Cairo_Surface</parameter>
   --  <parameter name="surface_x">X coordinate at which to place the origin of Surface</parameter>
   --  <parameter name="surface_y">Y coordinate at which to place the origin of Surface</parameter>
   --
   --  A drawing operator that paints the current source
   --  using the alpha channel of Surface as a mask. (Opaque
   --  areas of Surface are painted with the source, transparent
   --  areas are not painted.)

   procedure Stroke (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  A drawing operator that strokes the current path according to the
   --  current line width, line join, line cap, and dash settings. After
   --  Stroke, the current path will be cleared from the cairo
   --  context. See Set_Line_Width, Set_Line_Join,
   --  Set_Line_Cap, Set_Dash, and
   --  Stroke_Preserve.
   --
   --  Note: Degenerate segments and sub-paths are treated specially and
   --  provide a useful result. These can result in two different
   --  situations:
   --
   --  1. Zero-length "on" segments set in Set_Dash. If the cap
   --  style is CAIRO_LINE_CAP_ROUND or CAIRO_LINE_CAP_SQUARE then these
   --  segments will be drawn as circular dots or squares respectively. In
   --  the case of CAIRO_LINE_CAP_SQUARE, the orientation of the squares
   --  is determined by the direction of the underlying path.
   --
   --  2. A sub-path created by Move_To followed by either a
   --  Close_Path or one or more calls to Line_To to the
   --  same coordinate as the Move_To. If the cap style is
   --  CAIRO_LINE_CAP_ROUND then these sub-paths will be drawn as circular
   --  dots. Note that in the case of CAIRO_LINE_CAP_SQUARE a degenerate
   --  sub-path will not be drawn at all, (since the correct orientation
   --  is indeterminate).
   --
   --  In no case will a cap style of CAIRO_LINE_CAP_BUTT cause anything
   --  to be drawn in the case of either degenerate segments or sub-paths.

   procedure Stroke_Preserve (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  A drawing operator that strokes the current path according to the
   --  current line width, line join, line cap, and dash settings. Unlike
   --  Stroke, Stroke_Preserve preserves the path within the
   --  cairo context.
   --
   --  See Set_Line_Width, Set_Line_Join,
   --  Set_Line_Cap, Set_Dash, and
   --  Stroke_Preserve.

   procedure Fill (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  A drawing operator that fills the current path according to the
   --  current fill rule, (each sub-path is implicitly closed before being
   --  filled). After Fill, the current path will be cleared from
   --  the cairo context. See Set_Fill_Rule and
   --  Fill_Preserve.

   procedure Fill_Preserve (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  A drawing operator that fills the current path according to the
   --  current fill rule, (each sub-path is implicitly closed before being
   --  filled). Unlike Fill, Fill_Preserve preserves the
   --  path within the cairo context.
   --
   --  See Set_Fill_Rule and Fill.

   procedure Copy_Page (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Emits the current page for backends that support multiple pages, but
   --  doesn't clear it, so, the contents of the current page will be retained
   --  for the next page too.  Use Show_Page if you want to get an
   --  empty page after the emission.
   --
   --  This is a convenience function that simply calls
   --  Surface_Copy_Page on Context's target.

   procedure Show_Page (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Emits and clears the current page for backends that support multiple
   --  pages.  Use Copy_Page if you don't want to clear the page.
   --
   --  This is a convenience function that simply calls
   --  Surface_Show_Page on Context's target.

   --------------
   -- Clipping --
   --------------

   procedure Reset_Clip (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Reset the current clip region to its original, unrestricted
   --  state. That is, set the clip region to an infinitely large shape
   --  containing the target surface. Equivalently, if infinity is too
   --  hard to grasp, one can imagine the clip region being reset to the
   --  exact bounds of the target surface.
   --
   --  Note that code meant to be reusable should not call
   --  Reset_Clip as it will cause results unexpected by
   --  higher-level code which calls Clip. Consider using
   --  Save and Restore around Clip as a more
   --  robust means of temporarily restricting the clip region.

   procedure Clip (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Establishes a new clip region by intersecting the current clip
   --  region with the current path as it would be filled by Fill
   --  and according to the current fill rule (see Set_Fill_Rule).
   --
   --  After Clip, the current path will be cleared from the cairo
   --  context.
   --
   --  The current clip region affects all drawing operations by
   --  effectively masking out any changes to the surface that are outside
   --  the current clip region.
   --
   --  Calling Clip can only make the clip region smaller, never
   --  larger. But the current clip is part of the graphics state, so a
   --  temporary restriction of the clip region can be achieved by
   --  calling Clip within a Save/Restore
   --  pair. The only other means of increasing the size of the clip
   --  region is Reset_Clip.

   procedure Clip_Preserve (Context : in out Cairo_Context'Class);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Establishes a new clip region by intersecting the current clip
   --  region with the current path as it would be filled by Fill
   --  and according to the current fill rule (see Set_Fill_Rule).
   --
   --  Unlike Clip, Clip_Preserve preserves the path within
   --  the cairo context.
   --
   --  The current clip region affects all drawing operations by
   --  effectively masking out any changes to the surface that are outside
   --  the current clip region.
   --
   --  Calling Clip can only make the clip region smaller, never
   --  larger. But the current clip is part of the graphics state, so a
   --  temporary restriction of the clip region can be achieved by
   --  calling Clip within a Save/Restore
   --  pair. The only other means of increasing the size of the clip
   --  region is Reset_Clip.

   ----------
   -- Text --
   ----------

   procedure Select_Font_Face
     (Context : in out Cairo_Context'Class;
      Family : String;
      Slant : Cairo_Font_Slant;
      Weight : Cairo_Font_Weight);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="family">a font family name, encoded in UTF-8</parameter>
   --  <parameter name="slant">the slant for the font</parameter>
   --  <parameter name="weight">the weight for the font</parameter>
   --
   --  Note: The Select_Font_Face function call is part of what
   --  the cairo designers call the "toy" text API. It is convenient for
   --  short demos and simple programs, but it is not expected to be
   --  adequate for serious text-using applications.
   --
   --  Selects a family and style of font from a simplified description as
   --  a family name, slant and weight. Cairo provides no operation to
   --  list available family names on the system (this is a "toy",
   --  remember), but the standard CSS2 generic family names, ("serif",
   --  "sans-serif", "cursive", "fantasy", "monospace"), are likely to
   --  work as expected.
   --
   --  For "real" font selection, see the font-backend-specific
   --  font_face_create functions for the font backend you are using. (For
   --  example, if you are using the freetype-based cairo-ft font backend,
   --  see Ft_Font_Face_Create_For_Ft_Face or
   --  Ft_Font_Face_Create_For_Pattern.) The resulting font face
   --  could then be used with Scaled_Font_Create and
   --  Set_Scaled_Font.
   --
   --  Similarly, when using the "real" font support, you can call
   --  directly into the underlying font system, (such as fontconfig or
   --  freetype), for operations such as listing available fonts, etc.
   --
   --  It is expected that most applications will need to use a more
   --  comprehensive font handling and text layout library, (for example,
   --  pango), in conjunction with cairo.
   --
   --  If text is drawn without a call to Select_Font_Face, (nor
   --  Set_Font_Face nor Set_Scaled_Font), the default
   --  family is platform-specific, but is essentially "sans-serif".
   --  Default slant is CAIRO_FONT_SLANT_NORMAL, and default weight is
   --  CAIRO_FONT_WEIGHT_NORMAL.
   --
   --  This function is equivalent to a call to New_Toy_Font_Face
   --  followed by Set_Font_Face.

   procedure Set_Font_Size
     (Context : in out Cairo_Context'Class;
      Size : double);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="size">the new font size, in user space units</parameter>
   --
   --  Sets the current font matrix to a scale by a factor of Size, replacing
   --  any font matrix previously set with Set_Font_Size or
   --  Set_Font_Matrix. This results in a font size of Size user space
   --  units. (More precisely, this matrix will result in the font's
   --  em-square being a Size by Size square in user space.)
   --
   --  If text is drawn without a call to Set_Font_Size, (nor
   --  Set_Font_Matrix nor Set_Scaled_Font), the default
   --  font size is 10.0.

   procedure Set_Font_Matrix
     (Context : in out Cairo_Context'Class;
      Matrix : Cairo_Matrix);

   procedure Set_Font_Options
     (Context : in out Cairo_Context'Class;
      Options : Cairo_Font_Options);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="options">font options to use</parameter>
   --
   --  Sets a set of custom font rendering options for the Cairo_Context.
   --  Rendering options are derived by merging these options with the
   --  options derived from underlying surface; if the value in Options
   --  has a default value (like CAIRO_ANTIALIAS_DEFAULT), then the value
   --  from the surface is used.

   procedure Set_Scaled_Font
     (Context : in out Cairo_Context'Class;
      Scaled_Font : access Cairo_Scaled_Font'Class);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="scaled_font">a Cairo_Scaled_Font</parameter>
   --
   --  Replaces the current font face, font matrix, and font options in
   --  the Cairo_Context with those of the Cairo_Scaled_Font.  Except for
   --  some translation, the current CTM of the Cairo_Context should be the
   --  same as that of the Cairo_Scaled_Font, which can be accessed
   --  using Scaled_Font_Get_Ctm.
   --
   --  Since: 1.2

   procedure Show_Text
     (Context : in out Cairo_Context'Class;
      UTF8 : String);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="utf8">a NUL-terminated string of text encoded in UTF-8, or NULL</parameter>
   --
   --  A drawing operator that generates the shape from a string of UTF-8
   --  characters, rendered according to the current font_face, font_size
   --  (font_matrix), and font_options.
   --
   --  This function first computes a set of glyphs for the string of
   --  text. The first glyph is placed so that its origin is at the
   --  current point. The origin of each subsequent glyph is offset from
   --  that of the previous glyph by the advance values of the previous
   --  glyph.
   --
   --  After this call the current point is moved to the origin of where
   --  the next glyph would be placed in this same progression. That is,
   --  the current point will be at the origin of the final glyph offset
   --  by its advance values. This allows for easy display of a single
   --  logical string with multiple calls to Show_Text.
   --
   --  Note: The Show_Text function call is part of what the cairo
   --  designers call the "toy" text API. It is convenient for short demos
   --  and simple programs, but it is not expected to be adequate for
   --  serious text-using applications. See Show_Glyphs for the
   --  "real" text display API in cairo.

   procedure Show_Glyphs
     (Context : in out Cairo_Context'Class;
      Glyphs : Cairo_Glyph_Array);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="glyphs">array of glyphs to show</parameter>
   --  <parameter name="num_glyphs">number of glyphs to show</parameter>
   --
   --  A drawing operator that generates the shape from an array of glyphs,
   --  rendered according to the current font face, font size
   --  (font matrix), and font options.

   procedure Show_Text_Glyphs
     (Context : in out Cairo_Context'Class;
      UTF8 : String;
      Glyphs : Cairo_Glyph_Array;
      Clusters : Cairo_Text_Cluster_Array;
      Cluster_Flags : Cairo_Text_Cluster_Flags);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="utf8">a string of text encoded in UTF-8</parameter>
   --  <parameter name="utf8_len">length of UTF8 in bytes, or -1 if it is NUL-terminated</parameter>
   --  <parameter name="glyphs">array of glyphs to show</parameter>
   --  <parameter name="num_glyphs">number of glyphs to show</parameter>
   --  <parameter name="clusters">array of cluster mapping information</parameter>
   --  <parameter name="num_clusters">number of clusters in the mapping</parameter>
   --  <parameter name="cluster_flags">cluster mapping flags</parameter>
   --
   --  This operation has rendering effects similar to Show_Glyphs
   --  but, if the target surface supports it, uses the provided text and
   --  cluster mapping to embed the text for the glyphs shown in the output.
   --  The Has_Show_Text_Glyphs function can be used to query that.
   --  If the target does not support it, this function acts like
   --  Show_Glyphs.
   --
   --  The mapping between UTF8 and Glyphs is provided by an array of
   --  <firstterm>clusters</firstterm>.  Each cluster covers a number of
   --  text bytes and glyphs, and neighboring clusters cover neighboring
   --  areas of UTF8 and Glyphs.  The clusters should collectively cover UTF8
   --  and Glyphs in entirety.
   --
   --  The first cluster always covers bytes from the beginning of UTF8.
   --  If Cluster_flags do not have the CAIRO_TEXT_CLUSTER_FLAG_BACKWARD
   --  set, the first cluster also covers the beginning
   --  of Glyphs, otherwise it covers the end of the Glyphs array and
   --  following clusters move backward.
   --
   --  See Cairo_Text_Cluster for constraints on valid clusters.
   --
   --  Since: 1.8

   procedure Set_Font_Face
     (Context : in out Cairo_Context'Class;
      Font_Face : access Cairo_Font_Face'Class);
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="font_face">a Cairo_Font_Face, or NULL to restore to the default font</parameter>
   --
   --  Replaces the current Cairo_Font_Face object in the Cairo_Context with
   --  Font_face. The replaced font face in the Cairo_Context will be
   --  destroyed if there are no other references to it.

   procedure Text_Path
     (Context : in out Cairo_Context'Class;
      UTF8 : String);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="utf8">a NUL-terminated string of text encoded in UTF-8, or NULL</parameter>
   --
   --  Adds closed paths for text to the current path.  The generated
   --  path if filled, achieves an effect similar to that of
   --  Show_Text.
   --
   --  Text conversion and positioning is done similar to Show_Text.
   --
   --  Like Show_Text, After this call the current point is
   --  moved to the origin of where the next glyph would be placed in
   --  this same progression.  That is, the current point will be at
   --  the origin of the final glyph offset by its advance values.
   --  This allows for chaining multiple calls to to Text_Path
   --  without having to set current point in between.
   --
   --  Note: The Text_Path function call is part of what the cairo
   --  designers call the "toy" text API. It is convenient for short demos
   --  and simple programs, but it is not expected to be adequate for
   --  serious text-using applications. See Glyph_Path for the
   --  "real" text path API in cairo.

   procedure Glyph_Path
     (Context : in out Cairo_Context'Class;
      Glyphs : Cairo_Glyph_Array);
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="glyphs">array of glyphs to show</parameter>
   --  <parameter name="num_glyphs">number of glyphs to show</parameter>
   --
   --  Adds closed paths for the glyphs to the current path.  The generated
   --  path if filled, achieves an effect similar to that of
   --  Show_Glyphs.


   function Get_Status (Context : Cairo_Context'Class) return Cairo_Status;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Checks whether an error has previously occurred for this context.
   --
   --  Returns: the current status of this context, see Cairo_Status

   ------------------------
   -- Insideness testing --
   ------------------------

   function In_Stroke
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Boolean;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">X coordinate of the point to test</parameter>
   --  <parameter name="y">Y coordinate of the point to test</parameter>
   --
   --  Tests whether the given point is inside the area that would be
   --  affected by a Stroke operation given the current path and
   --  stroking parameters. Surface dimensions and clipping are not taken
   --  into account.
   --
   --  See Stroke, Set_Line_Width, Set_Line_Join,
   --  Set_Line_Cap, Set_Dash, and
   --  Stroke_Preserve.
   --
   --  Return value: A non-zero value if the point is inside, or zero if
   --  outside.

   function In_Fill
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Boolean;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">X coordinate of the point to test</parameter>
   --  <parameter name="y">Y coordinate of the point to test</parameter>
   --
   --  Tests whether the given point is inside the area that would be
   --  affected by a Fill operation given the current path and
   --  filling parameters. Surface dimensions and clipping are not taken
   --  into account.
   --
   --  See Fill, Set_Fill_Rule and Fill_Preserve.
   --
   --  Return value: A non-zero value if the point is inside, or zero if
   --  outside.

   ----------
   -- Path --
   ----------

   procedure Copy_Path
     (Context : Cairo_Context'Class;
      Path : in out Cairo_Path);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Creates a copy of the current path and returns it to the user as a
   --  Cairo_Path. See Cairo_Path_Data for hints on how to iterate
   --  over the returned data structure.
   --
   --  This function will always return a valid pointer, but the result
   --  will have no data (<literal>data==NULL</literal> and
   --  <literal>num_data==0</literal>), if either of the following
   --  conditions hold:
   --
   --  <orderedlist>
   --  <listitem>If there is insufficient memory to copy the path. In this
   --      case <literal>path->status</literal> will be set to
   --      CAIRO_STATUS_NO_MEMORY.</listitem>
   --  <listitem>If Context is already in an error state. In this case
   --     <literal>path->status</literal> will contain the same status that
   --     would be returned by Status.</listitem>
   --  </orderedlist>
   --
   --  In either case, <literal>path->status</literal> will be set to
   --  CAIRO_STATUS_NO_MEMORY (regardless of what the error status in
   --  Context might have been).
   --
   --  Return value: the copy of the current path. The caller owns the
   --  returned object and should call Path_Destroy when finished
   --  with it.

   procedure Copy_Path_Flat
     (Context : Cairo_Context'Class;
      Path : in out Cairo_Path);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets a flattened copy of the current path and returns it to the
   --  user as a Cairo_Path. See Cairo_Path_Data for hints on
   --  how to iterate over the returned data structure.
   --
   --  This function is like Copy_Path except that any curves
   --  in the path will be approximated with piecewise-linear
   --  approximations, (accurate to within the current tolerance
   --  value). That is, the result is guaranteed to not have any elements
   --  of type CAIRO_PATH_CURVE_TO which will instead be replaced by a
   --  series of CAIRO_PATH_LINE_TO elements.
   --
   --  This function will always return a valid pointer, but the result
   --  will have no data (<literal>data==NULL</literal> and
   --  <literal>num_data==0</literal>), if either of the following
   --  conditions hold:
   --
   --  <orderedlist>
   --  <listitem>If there is insufficient memory to copy the path. In this
   --      case <literal>path->status</literal> will be set to
   --      CAIRO_STATUS_NO_MEMORY.</listitem>
   --  <listitem>If Context is already in an error state. In this case
   --     <literal>path->status</literal> will contain the same status that
   --     would be returned by Status.</listitem>
   --  </orderedlist>
   --
   --  Return value: the copy of the current path. The caller owns the
   --  returned object and should call Path_Destroy when finished
   --  with it.

   -------------
   -- Extents --
   -------------

   function Get_Font_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Font_Extents;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="extents">a Cairo_Font_Extents object into which the results</parameter>
   --  will be stored.
   --
   --  Gets the font extents for the currently selected font.

   function Get_Stroke_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x1">left of the resulting extents</parameter>
   --  <parameter name="y1">top of the resulting extents</parameter>
   --  <parameter name="x2">right of the resulting extents</parameter>
   --  <parameter name="y2">bottom of the resulting extents</parameter>
   --
   --  Computes a bounding box in user coordinates covering the area that
   --  would be affected, (the "inked" area), by a Stroke
   --  operation operation given the current path and stroke
   --  parameters. If the current path is empty, returns an empty
   --  rectangle ((0,0), (0,0)). Surface dimensions and clipping are not
   --  taken into account.
   --
   --  Note that if the line width is set to exactly zero, then
   --  Stroke_Extents will return an empty rectangle. Contrast with
   --  Path_Extents which can be used to compute the non-empty
   --  bounds as the line width approaches zero.
   --
   --  Note that Stroke_Extents must necessarily do more work to
   --  compute the precise inked areas in light of the stroke parameters,
   --  so Path_Extents may be more desirable for sake of
   --  performance if non-inked path extents are desired.
   --
   --  See Stroke, Set_Line_Width, Set_Line_Join,
   --  Set_Line_Cap, Set_Dash, and
   --  Stroke_Preserve.

   function Get_Fill_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x1">left of the resulting extents</parameter>
   --  <parameter name="y1">top of the resulting extents</parameter>
   --  <parameter name="x2">right of the resulting extents</parameter>
   --  <parameter name="y2">bottom of the resulting extents</parameter>
   --
   --  Computes a bounding box in user coordinates covering the area that
   --  would be affected, (the "inked" area), by a Fill operation
   --  given the current path and fill parameters. If the current path is
   --  empty, returns an empty rectangle ((0,0), (0,0)). Surface
   --  dimensions and clipping are not taken into account.
   --
   --  Contrast with Path_Extents, which is similar, but returns
   --  non-zero extents for some paths with no inked area, (such as a
   --  simple line segment).
   --
   --  Note that Fill_Extents must necessarily do more work to
   --  compute the precise inked areas in light of the fill rule, so
   --  Path_Extents may be more desirable for sake of performance
   --  if the non-inked path extents are desired.
   --
   --  See Fill, Set_Fill_Rule and Fill_Preserve.

   function Get_Path_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x1">left of the resulting extents</parameter>
   --  <parameter name="y1">top of the resulting extents</parameter>
   --  <parameter name="x2">right of the resulting extents</parameter>
   --  <parameter name="y2">bottom of the resulting extents</parameter>
   --
   --  Computes a bounding box in user-space coordinates covering the
   --  points on the current path. If the current path is empty, returns
   --  an empty rectangle ((0,0), (0,0)). Stroke parameters, fill rule,
   --  surface dimensions and clipping are not taken into account.
   --
   --  Contrast with Fill_Extents and Stroke_Extents which
   --  return the extents of only the area that would be "inked" by
   --  the corresponding drawing operations.
   --
   --  The result of Path_Extents is defined as equivalent to the
   --  limit of Stroke_Extents with CAIRO_LINE_CAP_ROUND as the
   --  line width approaches 0.0, (but never reaching the empty-rectangle
   --  returned by Stroke_Extents for a line width of 0.0).
   --
   --  Specifically, this means that zero-area sub-paths such as
   --  Move_To;Line_To segments, (even degenerate cases
   --  where the coordinates to both calls are identical), will be
   --  considered as contributing to the extents. However, a lone
   --  Move_To will not contribute to the results of
   --  Path_Extents.
   --
   --  Since: 1.6

   function Get_Text_Extents
     (Context : Cairo_Context'Class;
      UTF8 : String)
      return Cairo_Text_Extents;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="utf8">a NUL-terminated string of text encoded in UTF-8, or NULL</parameter>
   --  Return a Cairo_Text_Extents object
   --
   --  Gets the extents for a string of text. The extents describe a
   --  user-space rectangle that encloses the "inked" portion of the text,
   --  (as it would be drawn by Show_Text). Additionally, the
   --  x_advance and y_advance values indicate the amount by which the
   --  current point would be advanced by Show_Text.
   --
   --  Note that whitespace characters do not directly contribute to the
   --  size of the rectangle (extents.width and extents.height). They do
   --  contribute indirectly by changing the position of non-whitespace
   --  characters. In particular, trailing whitespace characters are
   --  likely to not affect the size of the rectangle, though they will
   --  affect the x_advance and y_advance values.

   function Get_Glyph_Extents
     (Context : Cairo_Context'Class;
      Glyphs : Cairo_Glyph_Array)
      return Cairo_Text_Extents;

   function Get_Font_Options
     (Context : Cairo_Context'Class)
      return Cairo_Font_Options;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --  <parameter name="options">a Cairo_Font_Options object into which to store</parameter>
   --    the retrieved options. All existing values are overwritten
   --
   --  Retrieves font rendering options set via cairo_set_font_options.
   --  Note that the returned options do not include any options derived
   --  from the underlying surface; they are literally the options
   --  passed to Set_Font_Options.

   function Get_Font_Face
     (Context : Cairo_Context'Class)
      return Cairo_Font_Face_Handle;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --
   --  Gets the current font face for a Cairo_Context.
   --
   --  Return value: the current font face.  This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Font_Face_Reference.
   --
   --  This function never returns NULL. If memory cannot be allocated, a
   --  special "nil" Cairo_Font_Face object will be returned on which
   --  Font_Face_Status returns CAIRO_STATUS_NO_MEMORY. Using
   --  this nil object will cause its error state to propagate to other
   --  objects it is passed to, (for example, calling
   --  Set_Font_Face with a nil font will trigger an error that
   --  will shutdown the Cairo_Context object).

   function Get_Font_Matrix
     (Context : Cairo_Context'Class)
      return Cairo_Matrix;

   -----------
   -- State --
   -----------

   function Get_Operator
     (Context : Cairo_Context'Class)
      return Cairo_Operator;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current compositing operator for a cairo context.
   --
   --  Return value: the current compositing operator.

   function Get_Source
     (Context : Cairo_Context'Class)
      return Cairo_Pattern_Handle;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current source pattern for Context.
   --
   --  Return value: the current source pattern. This object is owned by
   --  cairo. To keep a reference to it, you must call
   --  Pattern_Reference.

   function Get_Tolerance
     (Context : Cairo_Context'Class)
      return double;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current tolerance value, as set by Set_Tolerance.
   --
   --  Return value: the current tolerance value.

   function Get_Antialias
     (Context : Cairo_Context'Class)
      return Cairo_Antialias;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current shape antialiasing mode, as set by Set_Antialias.
   --
   --  Return value: the current shape antialiasing mode.

   function Has_Current_Point
     (Context : Cairo_Context'Class)
      return Boolean;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Returns whether a current point is defined on the current path.
   --  See Get_Current_Point for details on the current point.
   --
   --  Return value: whether a current point is defined.
   --
   --  Since: 1.6

   function Get_Current_Point
     (Context : Cairo_Context'Class)
      return Cairo_Tuple;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">return value for X coordinate of the current point</parameter>
   --  <parameter name="y">return value for Y coordinate of the current point</parameter>
   --
   --  Gets the current point of the current path, which is
   --  conceptually the final point reached by the path so far.
   --
   --  The current point is returned in the user-space coordinate
   --  system. If there is no defined current point or if Context is in an
   --  error status, X and Y will both be set to 0.0. It is possible to
   --  check this in advance with Has_Current_Point.
   --
   --  Most path construction functions alter the current point. See the
   --  following for details on how they affect the current point:
   --  New_Path, New_Sub_Path,
   --  Append_Path, Close_Path,
   --  Move_To, Line_To, Curve_To,
   --  Rel_Move_To, Rel_Line_To, Rel_Curve_To,
   --  Arc, Arc_Negative, Rectangle,
   --  Text_Path, Glyph_Path, Stroke_To_Path.
   --
   --  Some functions use and alter the current point but do not
   --  otherwise change current path:
   --  Show_Text.
   --
   --  Some functions unset the current path and as a result, current point:
   --  Fill, Stroke.

   function Get_Fill_Rule
     (Context : Cairo_Context'Class)
      return Cairo_Fill_Rule;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current fill rule, as set by Set_Fill_Rule.
   --
   --  Return value: the current fill rule.

   function Get_Line_Width
     (Context : Cairo_Context'Class)
      return double;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  This function returns the current line width value exactly as set by
   --  Set_Line_Width. Note that the value is unchanged even if
   --  the CTM has changed between the calls to Set_Line_Width and
   --  Get_Line_Width.
   --
   --  Return value: the current line width.

   function Get_Line_Cap
     (Context : Cairo_Context'Class)
      return Cairo_Line_Cap;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current line cap style, as set by Set_Line_Cap.
   --
   --  Return value: the current line cap style.

   function Get_Line_Join
     (Context : Cairo_Context'Class)
      return Cairo_Line_Join;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current line join style, as set by Set_Line_Join.
   --
   --  Return value: the current line join style.

   function Get_Miter_Limit
     (Context : Cairo_Context'Class)
      return double;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current miter limit, as set by Set_Miter_Limit.
   --
   --  Return value: the current miter limit.

   function Get_Dash_Count
     (Context : Cairo_Context'Class)
      return Natural;
   -- <parameter name="Context">a #Cairo_Context</parameter>
   --
   --  This function returns the length of the dash array in Context
   --  (0 if dashing is not currently in effect).
   --
   --  See also Set_Dash, Get_Dashes and Get_Dash_Offset.
   --
   --  Return value: the length of the dash array, or 0 if no dash array set.
   --
   --  Since: 1.4

   function Get_Dashes
     (Context : Cairo_Context'Class)
      return double_Array;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --
   --  Gets the current dash array.
   --
   --  Since: 1.4

   function Get_Dash_Offset
     (Context : Cairo_Context'Class)
      return double;
   --  <parameter name="Context">a Cairo_Context</parameter>
   --
   --  Gets the current dash offset.
   --
   --  Since: 1.4

   function Get_Matrix
     (Context : Cairo_Context'Class)
      return Cairo_Matrix;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="matrix">return value for the matrix</parameter>
   --
   --  Stores the current transformation matrix (CTM) into Matrix.

   function Get_Target
     (Context : Cairo_Context'Class)
      return Cairo_Surface_Handle;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the target surface for the cairo context as passed to
   --  Create.
   --
   --  This function will always return a valid pointer, but the result
   --  can be a "nil" surface if Context is already in an error state,
   --  (ie. Status <literal>!=</literal> CAIRO_STATUS_SUCCESS).
   --  A nil surface is indicated by Surface_Status
   --  <literal>!=</literal> CAIRO_STATUS_SUCCESS.
   --
   --  Return value: the target surface. This object is owned by cairo. To
   --  keep a reference to it, you must call Surface_Reference.

   function Get_Group_Target
     (Context : Cairo_Context'Class)
      return Cairo_Surface_Handle;
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current destination surface for the context. This is either
   --  the original target surface as passed to Create or the target
   --  surface for the current group as started by the most recent call to
   --  Push_Group or Push_Group_With_Content.
   --
   --  This function will always return a valid pointer, but the result
   --  can be a "nil" surface if Context is already in an error state,
   --  (ie. Status <literal>!=</literal> CAIRO_STATUS_SUCCESS).
   --  A nil surface is indicated by Surface_Status
   --  <literal>!=</literal> CAIRO_STATUS_SUCCESS.
   --
   --  Return value: the target surface. This object is owned by cairo. To
   --  keep a reference to it, you must call Surface_Reference.
   --
   --  Since: 1.2

   ---------------------
   -- Transformations --
   ---------------------

   function User_To_Device
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Cairo_Tuple;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x">X value of coordinate (in/out parameter)</parameter>
   --  <parameter name="y">Y value of coordinate (in/out parameter)</parameter>
   --
   --  Transform a coordinate from user space to device space by
   --  multiplying the given point by the current transformation matrix
   --  (CTM).

   function User_To_Device_Distance
     (Context : Cairo_Context'Class;
      Vector : Cairo_Tuple)
      return Cairo_Tuple;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="dx">X component of a distance vector (in/out parameter)</parameter>
   --  <parameter name="dy">Y component of a distance vector (in/out parameter)</parameter>
   --
   --  Transform a distance vector from user space to device space. This
   --  function is similar to User_To_Device except that the
   --  translation components of the CTM will be ignored when transforming
   --  (Dx,Dy).

   function Device_To_User
     (Context : Cairo_Context'Class;
      Point : Cairo_Tuple)
      return Cairo_Tuple;
   --  <parameter name="Context">a cairo</parameter>
   --  <parameter name="x">X value of coordinate (in/out parameter)</parameter>
   --  <parameter name="y">Y value of coordinate (in/out parameter)</parameter>
   --
   --  Transform a coordinate from device space to user space by
   --  multiplying the given point by the inverse of the current
   --  transformation matrix (CTM).

   function Device_To_User_Distance
     (Context : Cairo_Context'Class;
      Vector : Cairo_Tuple)
      return Cairo_Tuple;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="dx">X component of a distance vector (in/out parameter)</parameter>
   --  <parameter name="dy">Y component of a distance vector (in/out parameter)</parameter>
   --
   --  Transform a distance vector from device space to user space. This
   --  function is similar to Device_To_User except that the
   --  translation components of the inverse CTM will be ignored when
   --  transforming (Dx,Dy).

   --------------
   -- Clipping --
   --------------

   function Get_Clip_Extents
     (Context : Cairo_Context'Class)
      return Cairo_Box;
   --  <parameter name="Context">a cairo context</parameter>
   --  <parameter name="x1">left of the resulting extents</parameter>
   --  <parameter name="y1">top of the resulting extents</parameter>
   --  <parameter name="x2">right of the resulting extents</parameter>
   --  <parameter name="y2">bottom of the resulting extents</parameter>
   --
   --  Computes a bounding box in user coordinates covering the area inside the
   --  current clip.
   --
   --  Since: 1.4

   procedure Copy_Clip_Rectangle_List
     (Context : Cairo_Context'Class;
      Rectangles : in out Cairo_Rectangle_List);
   --  <parameter name="Context">a cairo context</parameter>
   --
   --  Gets the current clip region as a list of rectangles in user coordinates.
   --  Never returns NULL.
   --
   --  The status in the list may be CAIRO_STATUS_CLIP_NOT_REPRESENTABLE to
   --  indicate that the clip region cannot be represented as a list of
   --  user-space rectangles. The status may have other values to indicate
   --  other errors.
   --
   --  Returns: the current clip region as a list of rectangles in user coordinates,
   --  which should be destroyed using Rectangle_List_Destroy.
   --
   --  Since: 1.4


   ------------
   -- Handle --
   ------------

   function Ref (Handle : Cairo_Context_Handle) return Cairo_Context_Ref;
   -- Deprecated
   procedure Reset (Handle : in out Cairo_Context_Handle);
   function Is_Set (Handle : Cairo_Context_Handle) return Boolean;

   -----------------------------
   -- Binding internal stuffs --
   -----------------------------

   --  Those functions give direct access to C cairo structures and are reserved
   --  to binding writers.

   function To_Handle
     (Ptr : Context_Ptr;
      Is_Referenced : Boolean)
      return Cairo_Context_Handle;
   --  Create a Handle from a C allocated structure (cairo_t*).
   --  If Is_Referenced is False, then cairo_reference is called to increment
   --  the reference counter.

   function Ptr
     (Context : Cairo_Context'Class)
      return Context_Ptr;
   --  Return the pointer to the C allocated structure (cairo_t*).

private

   type Cairo_Context is tagged limited record
      Ptr : Context_Ptr;
   end record;

   type Cairo_Context_Handle is new Ada.Finalization.Controlled with record
      Ref : Cairo_Context_Ref;
   end record;
--   procedure Initialize (O : in out Cairo_Context_Handle);
   procedure Adjust (O : in out Cairo_Context_Handle);
   procedure Finalize (O : in out Cairo_Context_Handle);

   Cairo_Context_Null_Handle : constant Cairo_Context_Handle :=
     (Ada.Finalization.Controlled with Ref => null);

end Cairo.Context;
