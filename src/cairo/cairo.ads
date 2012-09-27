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

--  Root package of the cairo Ada binding.
--  This package contains:
--  - Declarations of structures that can directly be mapped from C to
--    their Ada equivalent.
--  - Declarations of enumerated types.
--  - Declarations of simple Ada specific types that don't exist in the C
--    version but simplify writting of certain functions or procedures.
--  - Some internal stuff, that must be visible to several Cairo packages.
--    Those types are only useful for the writting of this binding, and
--    for writting bindings to other libraries that need access to cairo
--    C machinery.

with Interfaces.C; use Interfaces.C;

package Cairo is


   -- Create cairo specific types instead of direct reuse of Interfaces.C types ?


   type double_Array is array (Natural range <>) of aliased double;

   -----------------------------
   -- Enumerated & mask types --
   -----------------------------

   ------------
   -- Status --
   ------------

   --  Cairo_Status is used to indicate errors that can occur when
   --  using Cairo. In some cases it is returned directly by functions.
   --  but when using Context, the last error, if any, is stored in
   --  the context and can be retrieved with Get_Status.
   --
   --  New entries may be added in future versions. Use To_String
   --  to get a human-readable representation of an error message.

   type Cairo_Status is
     (CAIRO_STATUS_SUCCESS,
      --  No error has occurred
      CAIRO_STATUS_NO_MEMORY,
      --  Out of memory
      CAIRO_STATUS_INVALID_RESTORE,
      --  Restore called without matching Save
      CAIRO_STATUS_INVALID_POP_GROUP,
      --  No saved group to pop
      CAIRO_STATUS_NO_CURRENT_POINT,
      --  No current point defined
      CAIRO_STATUS_INVALID_MATRIX,
      --  Invalid matrix (not invertible)
      CAIRO_STATUS_INVALID_STATUS,
      --  Invalid value for an input Cairo_Status
      CAIRO_STATUS_NULL_POINTER,
      --  NULL pointer
      CAIRO_STATUS_INVALID_STRING,
      --  Input string not valid UTF-8
      CAIRO_STATUS_INVALID_PATH_DATA,
      --  Input path data not valid
      CAIRO_STATUS_READ_ERROR,
      --  Error while reading from input stream
      CAIRO_STATUS_WRITE_ERROR,
      --  Error while writing to output stream
      CAIRO_STATUS_SURFACE_FINISHED,
      --  Target surface has been finished
      CAIRO_STATUS_SURFACE_TYPE_MISMATCH,
      --  The surface type is not appropriate for the operation
      CAIRO_STATUS_PATTERN_TYPE_MISMATCH,
      --  The pattern type is not appropriate for the operation
      CAIRO_STATUS_INVALID_CONTENT,
      --  Invalid value for an input Cairo_Content
      CAIRO_STATUS_INVALID_FORMAT,
      --  Invalid value for an input Cairo_Format
      CAIRO_STATUS_INVALID_VISUAL,
      --  Invalid value for an input Visual*
      CAIRO_STATUS_FILE_NOT_FOUND,
      --  File not found
      CAIRO_STATUS_INVALID_DASH,
      --  Invalid value for a dash setting
      CAIRO_STATUS_INVALID_DSC_COMMENT,
      --  Invalid value for a DSC comment (Since 1.2)
      CAIRO_STATUS_INVALID_INDEX,
      --  Invalid index passed to getter (Since 1.4)
      CAIRO_STATUS_CLIP_NOT_REPRESENTABLE,
      --  Clip region not representable in desired format (Since 1.4)
      CAIRO_STATUS_TEMP_FILE_ERROR,
      --  Error creating or writing to a temporary file (Since 1.6)
      CAIRO_STATUS_INVALID_STRIDE,
      --  Invalid value for stride (Since 1.6)
      CAIRO_STATUS_FONT_TYPE_MISMATCH,
      --  The font type is not appropriate for the operation (Since 1.8)
      CAIRO_STATUS_USER_FONT_IMMUTABLE,
      --  The user-font is immutable (Since 1.8)
      CAIRO_STATUS_USER_FONT_ERROR,
      --  Error occurred in a user-font callback function (Since 1.8)
      CAIRO_STATUS_NEGATIVE_COUNT,
      --  Negative number used where it is not allowed (Since 1.8)
      CAIRO_STATUS_INVALID_CLUSTERS,
      --  Input clusters do not represent the accompanying text and glyph array (Since 1.8)
      CAIRO_STATUS_INVALID_SLANT,
      --  Invalid value for an input Cairo_Font_Slant (Since 1.8)
      CAIRO_STATUS_INVALID_WEIGHT
      --  Invalid value for an input Cairo_Font_Weight (Since 1.8)
      );
   pragma Convention (C, Cairo_Status);

   function To_String (Status : Cairo_Status) return String;


   -------------
   -- Content --
   -------------

   --  Cairo_Content is used to describe the content that a surface will
   --  contain, whether color information, alpha information (translucence
   --  vs. opacity), or both.
   --
   --  Note: The large values here are designed to keep Cairo_Content
   --  values distinct from Cairo_Format values so that the
   --  implementation can detect the error if users confuse the two types.

   type Cairo_Content is new unsigned;
   CAIRO_CONTENT_COLOR       : constant Cairo_Content := 16#1000#;
   --  The surface will hold color content only.
   CAIRO_CONTENT_ALPHA       : constant Cairo_Content := 16#2000#;
   --  ohe surface will hold alpha content only.
   CAIRO_CONTENT_COLOR_ALPHA : constant Cairo_Content := 16#3000#;
   --  The surface will hold color and alpha content.


   --------------
   -- Operator --
   --------------
   --  Cairo_Operator is used to set the compositing operator for all cairo
   --  drawing operations.
   --
   --  The default operator is CAIRO_OPERATOR_OVER.
   --
   --  The operators marked as <firstterm>unbounded</firstterm> modify their
   --  destination even outside of the mask layer (that is, their effect is not
   --  bound by the mask layer).  However, their effect can still be limited by
   --  way of clipping.
   --
   --  To keep things simple, the operator descriptions here
   --  document the behavior for when both source and destination are either fully
   --  transparent or fully opaque.  The actual implementation works for
   --  translucent layers too.
   --  For a more detailed explanation of the effects of each operator, including
   --  the mathematical definitions, see
   --  <ulink url="http://cairographics.org/operators/">http://cairographics.org/operators/</ulink>.

   type Cairo_Operator is
     (CAIRO_OPERATOR_CLEAR,
      --  Clear destination layer (bounded)

      CAIRO_OPERATOR_SOURCE,
      --  Replace destination layer (bounded)
      CAIRO_OPERATOR_OVER,
      --  Draw source layer on top of destination layer (bounded)
      CAIRO_OPERATOR_IN,
      --  Draw source where there was destination content (unbounded)
      CAIRO_OPERATOR_OUT,
      --  Draw source where there was no destination content (unbounded)
      CAIRO_OPERATOR_ATOP,
      --  Draw source on top of destination content and only there

      CAIRO_OPERATOR_DEST,
      --  Ignore the source
      CAIRO_OPERATOR_DEST_OVER,
      --  Draw destination on top of source
      CAIRO_OPERATOR_DEST_IN,
      --  Leave destination only where there was source content (unbounded)
      CAIRO_OPERATOR_DEST_OUT,
      --  Leave destination only where there was no source content
      CAIRO_OPERATOR_DEST_ATOP,
      --  Leave destination on top of source content and only there (unbounded)

      CAIRO_OPERATOR_XOR,
      --  Source and destination are shown where there is only one of them
      CAIRO_OPERATOR_ADD,
      --  Source and destination layers are accumulated
      CAIRO_OPERATOR_SATURATE
      --  Like over, but assuming source and dest are disjoint geometries
      );
   pragma Convention (C, Cairo_Operator);


   ---------------
   -- Antialias --
   ---------------

   --  Specifies the type of antialiasing to do when rendering text or shapes.

   type Cairo_Antialias is
     (CAIRO_ANTIALIAS_DEFAULT,
      --  Use the default antialiasing for the subsystem and target device
      CAIRO_ANTIALIAS_NONE,
      --  Use a bilevel alpha mask
      CAIRO_ANTIALIAS_GRAY,
      --  Perform single-color antialiasing (using shades of gray
      --  for black text on a white background, for example).
      CAIRO_ANTIALIAS_SUBPIXEL
      --  Perform antialiasing by taking advantage of the order of
      --  subpixel elements on devices such as LCD panels
      );
   pragma Convention (C, Cairo_Antialias);


   ---------------
   -- Fill_Rule --
   ---------------

   --  Cairo_Fill_Rule is used to select how paths are filled. For both
   --  fill rules, whether or not a point is included in the fill is
   --  determined by taking a ray from that point to infinity and looking
   --  at intersections with the path. The ray can be in any direction,
   --  as long as it doesn't pass through the end point of a segment
   --  or have a tricky intersection sadded in future versions.
   --
   --  The default fill rule is CAIRO_FILL_RULE_WINDING.
   --
   --  New entries may be added in future versions.

   type Cairo_Fill_Rule is
     (CAIRO_FILL_RULE_WINDING,
      --  If the path crosses the ray from  left-to-right, counts +1.
      --  If the path crosses the ray  from right to left, counts -1.
      --  (Left and right are determined from the perspective of looking
      --  along the ray from the starting  point)
      --  If the total count is non-zero, the point will be filled.
      CAIRO_FILL_RULE_EVEN_ODD
      --  Counts the total number of intersections, without regard to
      --  the orientation of the contour. If the total number of
      --  intersections is odd, the point will be filled.
      );
   pragma Convention (C, Cairo_Fill_Rule);


   --------------
   -- Line_Cap --
   --------------

   --  Specifies how to render the endpoints of the path when stroking.
   --
   --  The default line cap style is CAIRO_LINE_CAP_BUTT.

   type Cairo_Line_Cap is
     (CAIRO_LINE_CAP_BUTT,
      --  Start (stop) the line exactly at the start(end) point
      CAIRO_LINE_CAP_ROUND,
      --  Use a round ending, the center of the circle is the end point
      CAIRO_LINE_CAP_SQUARE
      --  Use squared ending, the center of the square is the end point
      );
   pragma Convention (C, Cairo_Line_Cap);


   ---------------
   -- Line_Join --
   ---------------

   --  Specifies how to render the junction of two lines when stroking.
   --
   --  The default line join style is CAIRO_LINE_JOIN_MITER.

   type Cairo_Line_Join is
     (CAIRO_LINE_JOIN_MITER,
      --  Use a sharp (angled) corner, see Set_Miter_Limit
      CAIRO_LINE_JOIN_ROUND,
      --  Use a rounded join, the center of the circle is the joint point
      CAIRO_LINE_JOIN_BEVEL
      --  Use a cut-off join, the join is cut off at half
      --  the line width from the joint point
      );
   pragma Convention (C, Cairo_Line_Join);


   ----------------
   -- Font_Slant --
   ----------------

   --  Specifies variants of a font face based on their slant.

   type Cairo_Font_Slant is
     (CAIRO_FONT_SLANT_NORMAL,
      --  Upright font style
      CAIRO_FONT_SLANT_ITALIC,
      --  Italic font style
      CAIRO_FONT_SLANT_OBLIQUE
      --  Oblique font style
      );
   pragma Convention (C, Cairo_Font_Slant);


   -----------------
   -- Font_Weight --
   -----------------

   --  Specifies variants of a font face based on their weight.

   type Cairo_Font_Weight is
     (CAIRO_FONT_WEIGHT_NORMAL,
      --  Normal font weight
      CAIRO_FONT_WEIGHT_BOLD
      --  Bold font weight
      );
   pragma Convention (C, Cairo_Font_Weight);


   --------------------
   -- Subpixel_Order --
   --------------------

   --  The subpixel order specifies the order of color elements within
   --  each pixel on the display device when rendering with an
   --  antialiasing mode of CAIRO_ANTIALIAS_SUBPIXEL.

   type Cairo_Subpixel_Order is
     (CAIRO_SUBPIXEL_ORDER_DEFAULT,
      --  Use the default subpixel order for the target device
      CAIRO_SUBPIXEL_ORDER_RGB,
      --  Subpixel elements are arranged horizontally with red at the left
      CAIRO_SUBPIXEL_ORDER_BGR,
      --  Subpixel elements are arranged horizontally with blue at the left
      CAIRO_SUBPIXEL_ORDER_VRGB,
      --  Subpixel elements are arranged vertically with red at the top
      CAIRO_SUBPIXEL_ORDER_VBGR
      --  Subpixel elements are arranged vertically with blue at the top
      );
   pragma Convention (C, Cairo_Subpixel_Order);


   ----------------
   -- Hint_Style --
   ----------------

   --  Specifies the type of hinting to do on font outlines. Hinting
   --  is the process of fitting outlines to the pixel grid in order
   --  to improve the appearance of the result. Since hinting outlines
   --  involves distorting them, it also reduces the faithfulness
   --  to the original outline shapes. Not all of the outline hinting
   --  styles are supported by all font backends.
   --
   --  New entries may be added in future versions.

   type Cairo_Hint_Style is
     (CAIRO_HINT_STYLE_DEFAULT,
      --  Use the default hint style for font backend and target device
      CAIRO_HINT_STYLE_NONE,
      --  Do not hint outlines
      CAIRO_HINT_STYLE_SLIGHT,
      --  Hint outlines slightly to improve contrast while retaining
      --  good fidelity to the original shapes.
      CAIRO_HINT_STYLE_MEDIUM,
      --  Hint outlines with medium strength giving a compromise
      --  between fidelity to the original shapes and contrast
      CAIRO_HINT_STYLE_FULL
      --  Hint outlines to maximize contrast
      );
   pragma Convention (C, Cairo_Hint_Style);


   ------------------
   -- Hint_Metrics --
   ------------------

   --  Specifies whether to hint font metrics; hinting font metrics
   --  means quantizing them so that they are integer values in
   --  device space. Doing this improves the consistency of
   --  letter and line spacing, however it also means that text
   --  will be laid out differently at different zoom factors.

   type Cairo_Hint_Metrics is
     (CAIRO_HINT_METRICS_DEFAULT,
      --  Hint metrics in the default manner for the font
      --  backend and target device
      CAIRO_HINT_METRICS_OFF,
      --  Do not hint font metrics
      CAIRO_HINT_METRICS_ON
      --  Hint font metrics
      );
   pragma Convention (C, Cairo_Hint_Metrics);


   --------------------
   -- Path_Data_Type --
   --------------------

   --  Cairo_Path_Data_Type is used to describe the type of one portion
   --  of a path when represented as a Cairo_Path.
   --  See Cairo.Path for details.

   type Cairo_Path_Data_Type is
     (CAIRO_PATH_MOVE_TO,
      --  A move-to operation
      CAIRO_PATH_LINE_TO,
      --  A line-to operation
      CAIRO_PATH_CURVE_TO,
      --  A curve-to operation
      CAIRO_PATH_CLOSE_PATH
      --  A close-path operation
      );
   pragma Convention (C, Cairo_Path_Data_Type);
   for Cairo_Path_Data_Type'Size use int'Size;


   ---------------
   -- Font_Type --
   ---------------

   --  Cairo_Font_Type is used to describe the type of a given font
   --  face or scaled font. The font types are also known as "font
   --  backends" within cairo.
   --
   --  The type of a font face is determined by the function used to
   --  create it, which will generally be of the form New_XXX_Font_Face.
   --  The font face type can be queried with Get_Type.
   --
   --  The various Cairo.Font_Face functions can be used with a font face
   --  of any type.
   --
   --  The type of a scaled font is determined by the type of the font
   --  face passed to New_Scaled_Font. The scaled font type can
   --  be queried with Get_Type
   --
   --  The various Cairo_Scaled_Font functions can be used with scaled
   --  fonts of any type, but some font backends also provide
   --  type-specific functions that must only be called with a scaled font
   --  of the appropriate type.
   --
   --  The behavior of calling a type-specific function with a scaled font
   --  of the wrong type is undefined.
   --
   --  New entries may be added in future versions.
   --
   --  Since: 1.2

   type Cairo_Font_Type is
     (CAIRO_FONT_TYPE_TOY,
      --  The font was created using cairo's toy font api
      CAIRO_FONT_TYPE_FT,
      --  The font is of type FreeType
      CAIRO_FONT_TYPE_WIN32,
      --  The font is of type Win32
      CAIRO_FONT_TYPE_QUARTZ,
      --  The font is of type Quartz (Since: 1.6)
      CAIRO_FONT_TYPE_USER
      --  The font was create using cairo's user font api (Since: 1.8)
      );
   pragma Convention (C, Cairo_Font_Type);


   ------------------
   -- Surface_Type --
   ------------------

   --  Cairo_Surface_Type is used to describe the type of a given
   --  surface. The surface types are also known as "backends" or "surface
   --  backends" within cairo.
   --
   --  The type of a surface is determined by the function used to create
   --  it, which will generally be of the form New_<emphasis>Type</emphasis>_Surface,
   --  (though see Create_Similar as well).
   --
   --  The surface type can be queried with Cairo.Surface.Get_Type
   --
   --  The various Cairo_Surface functions can be used with surfaces of
   --  any type, but some backends also provide type-specific functions
   --  that must only be called with a surface of the appropriate
   --  type. These functions have names that begin with
   --  cairo_<emphasis>type</emphasis>_surface<!-- --> such as Cairo.Surface.Image.Get_Width.
   --
   --  The behavior of calling a type-specific function with a surface of
   --  the wrong type is undefined.
   --
   --  New entries may be added in future versions.
   --
   --  Since: 1.2

   type Cairo_Surface_Type is
     (CAIRO_SURFACE_TYPE_IMAGE,
      --  The surface is of type image
      CAIRO_SURFACE_TYPE_PDF,
      --  The surface is of type pdf
      CAIRO_SURFACE_TYPE_PS,
      --  The surface is of type ps
      CAIRO_SURFACE_TYPE_XLIB,
      --  The surface is of type xlib
      CAIRO_SURFACE_TYPE_XCB,
      --  The surface is of type xcb
      CAIRO_SURFACE_TYPE_GLITZ,
      --  The surface is of type glitz
      CAIRO_SURFACE_TYPE_QUARTZ,
      --  The surface is of type quartz
      CAIRO_SURFACE_TYPE_WIN32,
      --  The surface is of type win32
      CAIRO_SURFACE_TYPE_BEOS,
      --  The surface is of type beos
      CAIRO_SURFACE_TYPE_DIRECTFB,
      --  The surface is of type directfb
      CAIRO_SURFACE_TYPE_SVG,
      --  The surface is of type svg
      CAIRO_SURFACE_TYPE_OS2,
      --  The surface is of type os2
      CAIRO_SURFACE_TYPE_WIN32_PRINTING,
      --  The surface is a win32 printing surface
      CAIRO_SURFACE_TYPE_QUARTZ_IMAGE
      --  The surface is of type quartz_image
      );
   pragma Convention (C, Cairo_Surface_Type);


   ------------
   -- Format --
   ------------

   --  Cairo_Format is used to identify the memory format of
   --  image data.
   --
   --  New entries may be added in future versions.

   type Cairo_Format is
     (CAIRO_FORMAT_ARGB32,
      --  Each pixel is a 32-bit quantity, with alpha in the upper
      --  8 bits, then red, then green, then blue.
      --  The 32-bit quantities are stored native-endian. Pre-multiplied
      --  alpha is used. (That is, 50% transparent red is 0x80800000,
      --  not 0x80ff0000.)
      CAIRO_FORMAT_RGB24,
      --  Each pixel is a 32-bit quantity, with the upper 8 bits
      --  unused. Red, Green, and Blue are stored in the remaining
      --  24 bits in that order.
      CAIRO_FORMAT_A8,
      --  Each pixel is a 8-bit quantity holding an alpha value.
      CAIRO_FORMAT_A1
      --  Each pixel is a 1-bit quantity holding an alpha value.
      --  Pixels are packed together into 32-bit quantities.
      --  The ordering of the bits matches the endianess of the platform.
      --  On a big-endian machine, the first pixel is in the uppermost
      --  bit, on a little-endian machine the first pixel is in the
      --  least-significant bit.

      -- CAIRO_FORMAT_RGB16_565
      --  This format value is deprecated. It has
      --    never been properly implemented in cairo and should not be used
      --    by applications. (since 1.2)
      -- The value of 4 is reserved by a deprecated enum value.
      -- The next format added must have an explicit value of 5.
      -- When a new value will be added, then a representation clause should be added
      -- or a deprecated value should be inserted for 4
      );
   pragma Convention (C, Cairo_Format);


   ------------------
   -- Pattern_Type --
   ------------------

   --  Cairo_Pattern_Type is used to describe the type of a given pattern.
   --
   --  The type of a pattern is determined by the function used to create
   --  it. The New_RGB_Pattern and New_RGBA_Pattern
   --  functions create SOLID patterns. The remaining
   --  New_XXX_Pattern functions map to pattern types in obvious ways.
   --
   --  The pattern type can be queried with Cairo.Pattern.Get_Type
   --
   --  Most Cairo.Pattern functions can be called with a pattern of any
   --  type, (though trying to change the extend or filter for a solid
   --  pattern will have no effect). A notable exception is
   --  Add_Color_Stop_RGB and
   --  Add_Color_Stop_RGBA (which must only be called with
   --  gradient patterns (either LINEAR or RADIAL). Otherwise the pattern
   --  will be shutdown and put into an error state.
   --
   --  New entries may be added in future versions.
   --
   --  Since: 1.2

   type Cairo_Pattern_Type is
     (CAIRO_PATTERN_TYPE_SOLID,
      --  The pattern is a solid (uniform) color. It may be opaque or translucent.
      CAIRO_PATTERN_TYPE_SURFACE,
      --  The pattern is a based on a surface (an image).
      CAIRO_PATTERN_TYPE_LINEAR,
      --  The pattern is a linear gradient.
      CAIRO_PATTERN_TYPE_RADIAL
      --  The pattern is a radial gradient.
      );
   pragma Convention (C, Cairo_Pattern_Type);


   ------------
   -- Extend --
   ------------

   --  Cairo_Extend is used to describe how pattern color/alpha will be
   --  determined for areas "outside" the pattern's natural area, (for
   --  example, outside the surface bounds or outside the gradient
   --  geometry).
   --
   --  The default extend mode is CAIRO_EXTEND_NONE for surface patterns
   --  and CAIRO_EXTEND_PAD for gradient patterns.
   --
   --  New entries may be added in future

   type Cairo_Extend is
     (CAIRO_EXTEND_NONE,
      --  Pixels outside of the source pattern are fully transparent
      CAIRO_EXTEND_REPEAT,
      --  The pattern is tiled by repeating
      CAIRO_EXTEND_REFLECT,
      --  The pattern is tiled by reflecting at the edges (Implemented
      --  for surface patterns since 1.6)
      CAIRO_EXTEND_PAD
      --  Pixels outside of the pattern copy the closest pixel from
      --  the source (Since 1.2; but only implemented for surface patterns since 1.6)
      );
   pragma Convention (C, Cairo_Extend);


   ------------
   -- Filter --
   ------------

   --  Cairo_Filter is used to indicate what filtering should be
   --  applied when reading pixel values from patterns. See
   --  Cairo.Pattern.Set_Source for indicating the desired filter to be
   --  used with a particular pattern.

   type Cairo_Filter is
     (CAIRO_FILTER_FAST,
      --  A high-performance filter, with quality similar to CAIRO_FILTER_NEAREST
      CAIRO_FILTER_GOOD,
      --  A reasonable-performance filter, with quality similar to CAIRO_FILTER_BILINEAR
      CAIRO_FILTER_BEST,
      --  The highest-quality available, performance may not be suitable for interactive use.
      CAIRO_FILTER_NEAREST,
      --  Nearest-neighbor filtering
      CAIRO_FILTER_BILINEAR,
      --  Linear interpolation in two dimensions
      CAIRO_FILTER_GAUSSIAN
      --  This filter value is currently unimplemented, and should not be used in current code.
      );
   pragma Convention (C, Cairo_Filter);


   ------------------------
   -- Text_Cluster_Flags --
   ------------------------

   --  Specifies properties of a text cluster mapping.
   --
   --  Since: 1.8

   type Cairo_Text_Cluster_Flags is new unsigned;
   CAIRO_TEXT_CLUSTER_FLAG_BACKWARD : constant Cairo_Text_Cluster_Flags := 16#00000001#;
   --  The clusters in the cluster array map to glyphs in the glyph array from end to start.


   --  CairoAda specific type, normally used by the thin binding.

   type Void is null record;
   type Void_Ptr is access all Void;
   pragma Convention (C, Void_Ptr);


   ---------------
   -- User_Data --
   ---------------

   type Cairo_User_Data_Key is new Void_Ptr;
   type Cairo_User_Data is new Void_Ptr; -- use a real tagged record ???
   pragma No_Strict_Aliasing (Cairo_User_Data);
   type Cairo_Destroy_Func is access procedure (Data : Cairo_User_Data);
   pragma Convention (C, Cairo_Destroy_Func);


   -----------
   -- Tuple --
   -----------

   --  CairoAda specific type used to store an (X, Y) pair.

   type Cairo_Tuple is record
      X : double;
      Y : double;
   end record;


   ---------
   -- Box --
   ---------

   --  CairoAda specific type used to store an (X1, Y1, X2, Y2) 4-uple.

   type Cairo_Box is record
      X1 : double;
      Y1 : double;
      X2 : double;
      Y2 : double;
   end record;


   ---------------
   -- Rectangle --
   ---------------

   --  A data structure for holding a rectangle.

   type Cairo_Rectangle is record
      X : double;
      --  X coordinate of the left side of the rectangle
      Y : double;
      --  Y coordinate of the the top side of the rectangle
      Width : double;
      --  Width of the rectangle
      Height : double;
      --  Height of the rectangle
   end record;
   pragma Convention (C, Cairo_Rectangle);

   type Cairo_Rectangle_Array is array (Natural range <>) of aliased Cairo_Rectangle;


   -----------
   -- Glyph --
   -----------

   --  The Cairo_Glyph structure holds information about a single glyph
   --  when drawing or measuring text. A font is (in simple terms) a
   --  collection of shapes used to draw text. A glyph is one of these
   --  shapes. There can be multiple glyphs for a single character
   --  (alternates to be used in different contexts, for example), or a
   --  glyph can be a <firstterm>ligature</firstterm> of multiple
   --  characters. Cairo doesn't expose any way of converting input text
   --  into glyphs, so in order to use the Cairo interfaces that take
   --  arrays of glyphs, you must directly access the appropriate
   --  underlying font system.
   --
   --  Note that the offsets given by X and Y are not cumulative. When
   --  drawing or measuring text, each glyph is individually positioned
   --  with respect to the overall origin

   type Cairo_Glyph is record
      Index : unsigned_long;
      --  Glyph index in the font. The exact interpretation of the
      --  glyph index depends on the font technology being used.
      X : double;
      --  The offset in the X direction between the origin used for
      --  drawing or measuring the string and the origin of this glyph.
      Y : double;
      --  The offset in the Y direction between the origin used for
      --  drawing or measuring the string and the origin of this glyph.
   end record;
   pragma Convention (C, Cairo_Glyph);

   type Cairo_Glyph_Array is array (Natural range <>) of aliased Cairo_Glyph;


   ------------------
   -- Text_Cluster --
   ------------------

   --  The Cairo_Text_Cluster structure holds information about a single
   --  <firstterm>text cluster</firstterm>. A text cluster is a minimal
   --  mapping of some glyphs corresponding to some UTF-8 text.
   --
   --  For a cluster to be valid, both Num_Bytes and Num_Glyphs should
   --  be non-negative, and at least one should be non-zero.
   --  Note that clusters with zero glyphs are not as well supported as
   --  normal clusters.  For example, PDF rendering applications typically
   --  ignore those clusters when PDF text is being selected.
   --
   --  See Show_Text_Glyphs for how clusters are used in advanced
   --  text operations.
   --
   --  Since: 1.8

   type Cairo_Text_Cluster is record
      Num_Bytes : int;
      --  The number of bytes of UTF-8 text covered by cluster
      Num_Glyphs : int;
      --  The number of glyphs covered by cluster
   end record;
   pragma Convention (C, Cairo_Text_Cluster);

   type Cairo_Text_Cluster_Array is array (Natural range <>) of aliased Cairo_Text_Cluster;


   ------------------
   -- Text_Extents --
   ------------------

   --  The Cairo_Text_Extents structure stores the extents of a single
   --  glyph or a string of glyphs in user-space coordinates. Because text
   --  extents are in user-space coordinates, they are mostly, but not
   --  entirely, independent of the current transformation matrix. If you call
   --  <literal>Scale (Context, 2.0, 2.0)</literal>, text will
   --  be drawn twice as big, but the reported text extents will not be
   --  doubled. They will change slightly due to hinting (so you can't
   --  assume that metrics are independent of the transformation matrix),
   --  but otherwise will remain unchanged.

   type Cairo_Text_Extents is record
      X_Bearing : double;
      --  The horizontal distance from the origin to the
      --  leftmost part of the glyphs as drawn. Positive if the
      --  glyphs lie entirely to the right of the origin.

      Y_Bearing : double;
      --  The vertical distance from the origin to the
      --  topmost part of the glyphs as drawn. Positive only if the
      --  glyphs lie completely below the origin; will usually be
      --  negative.

      Width : double;
      --  Width of the glyphs as drawn

      Height : double;
      --  Height of the glyphs as drawn

      X_Advance : double;
      --  Distance to advance in the X direction
      --  after drawing these glyphs

      Y_Advance : double;
      --  Distance to advance in the Y direction
      --  after drawing these glyphs. Will typically be zero except
      --  for vertical text layout as found in East-Asian languages.
   end record;
   pragma Convention (C, Cairo_Text_Extents);


   ------------------
   -- Font_Extents --
   ------------------

   --  The Cairo_Font_Extents structure stores metric information for
   --  a font. Values are given in the current user-space coordinate
   --  system.
   --
   --  Because font metrics are in user-space coordinates, they are
   --  mostly, but not entirely, independent of the current transformation
   --  matrix. If you call <literal>Scale (Context, 2.0, 2.0)</literal>,
   --  text will be drawn twice as big, but the reported text extents will
   --  not be doubled. They will change slightly due to hinting (so you
   --  can't assume that metrics are independent of the transformation
   --  matrix), but otherwise will remain unchanged.

   type Cairo_Font_Extents is record
      Ascent : double;
      --  The distance that the font extends above the baseline.
      --  Note that this is not always exactly equal to the maximum
      --  of the extents of all the glyphs in the font, but rather
      --  is picked to express the font designer's intent as to
      --  how the font should align with elements above it.

      Descent : double;
      --  RTe distance that the font extends below the baseline.
      --  This value is positive for typical fonts that include
      --  portions below the baseline. Note that this is not always
      --  exactly equal to the maximum of the extents of all the
      --  glyphs in the font, but rather is picked to express the
      --  font designer's intent as to how the the font should
      --  align with elements below it.

      Height : double;
      --  The recommended vertical distance between baselines when
      --  setting consecutive lines of text with the font. This
      --  is greater than Ascent + Descent by a
      --  quantity known as the <firstterm>line spacing</firstterm>
      --  or <firstterm>external leading</firstterm>. When space
      --  is at a premium, most fonts can be set with only
      --  a distance of Ascent + Descent between lines.

      Max_X_Advance : double;
      --  The maximum distance in the X direction that
      --  the the origin is advanced for any glyph in the font.

      Max_Y_Advance : double;
      --  The maximum distance in the Y direction that
      --  the the origin is advanced for any glyph in the font.
      --  this will be zero for normal fonts used for horizontal
      --  writing. (The scripts of East Asia are sometimes written
      --  vertically.)
   end record;
   pragma Convention (C, Cairo_Font_Extents);


   ------------
   -- Matrix --
   ------------

   --  A Cairo_Matrix holds an affine transformation, such as a scale,
   --  rotation, shear, or a combination of those. The transformation of
   --  a point (x, y) is given by:
   --  <programlisting>
   --      x_new := xx * x + xy * y + x0;
   --      y_new := yx * x + yy * y + y0;
   --  </programlisting>

   type Cairo_Matrix is record
      XX : double; YX : double;
      XY : double; YY : double;
      X0 : double; Y0 : double;
   end record;
   pragma Convention (C, Cairo_Matrix);

   procedure Init
     (Matrix : out Cairo_Matrix;
      XX, YX, XY, YY, X0, Y0 : double);
   --  Sets Matrix to be the affine transformation given by
   --  XX, YX, XY, YY, X0, Y0. The transformation is given by:
   --  <programlisting>
   --   X_New := XX * X + XY * Y + X0;
   --   Y_New := YX * X + YY * Y + Y0;
   --  </programlisting>

   procedure Init_Identity
     (Matrix : out Cairo_Matrix);
   --  Modifies Matrix to be an identity transformation.

   procedure Init_Translate
     (Matrix : out Cairo_Matrix;
      T : Cairo_Tuple);
   -- Initializes Matrix to a transformation that translates by T.

   procedure Init_Scale
     (Matrix : out Cairo_Matrix;
      SX, SY : double);
   --  Initializes Matrix to a transformation that scales by SX and SY
   --  in the X and Y dimensions, respectively.

   procedure Init_Rotate
     (Matrix : out Cairo_Matrix;
      Radians : double);
   --  Initialized Matrix to a transformation that rotates by Radians.
   --  The direction of rotation is defined such that positive angles
   --  rotate in the direction from the positive X axis toward the positive
   --  Y axis. With the default axis orientation of cairo, positive angles
   --  rotate in a clockwise direction.

   procedure Translate
     (Matrix : in out Cairo_Matrix;
      T : Cairo_Tuple);
   --  Applies a translation by T (X, Y) to the transformation in
   --  Matrix. The effect of the new transformation is to first translate
   --  the coordinates by T.X and T.Y, then apply the original transformation
   --  to the coordinates.

   procedure Scale
     (Matrix : in out Cairo_Matrix;
      SX, SY : double);
   --  Applies scaling by SX, SY to the transformation in Matrix. The
   --  effect of the new transformation is to first scale the coordinates
   --  by SX and SY, then apply the original transformation to the coordinates.

   procedure Rotate
     (Matrix : in out Cairo_Matrix;
      Radians : double);
   --  Applies rotation by Radians to the transformation in
   --  Matrix. The effect of the new transformation is to first rotate the
   --  coordinates by Radians, then apply the original transformation
   --  to the coordinates.

   procedure Invert
     (Matrix : in out Cairo_Matrix;
      Status : out Cairo_Status);
   --  Changes Matrix to be the inverse of it's original value. Not
   --  all transformation matrices have inverses; if the matrix
   --  collapses points together (it is <firstterm>degenerate</firstterm>),
   --  then it has no inverse and this function will fail.
   --
   --  If Matrix has an inverse, modifies Matrix to
   --  be the inverse matrix and set Status to CAIRO_STATUS_SUCCESS.
   --  Otherwise, set Status to CAIRO_STATUS_INVALID_MATRIX.

   procedure Multiply
     (Result : out Cairo_Matrix;
      A, B : Cairo_Matrix);
   --  Multiplies the affine transformations in A and B together
   --  and stores the result in Result. The effect of the resulting
   --  transformation is to first apply the transformation in A to the
   --  coordinates and then apply the transformation in B to the
   --  coordinates.
   --
   --  It is allowable for Result to be identical to either A or B.

   procedure Transform_Distance
     (Matrix : Cairo_Matrix;
      Vector : in out Cairo_Tuple);
   --  Transforms the distance Vector (DX, DY) by Matrix. This is
   --  similar to Transform_Point except that the translation
   --  components of the transformation are ignored. The calculation of
   --  the returned vector is as follows:
   --
   --  <programlisting>
   --  dx2 := dx1 * a + dy1 * c;
   --  dy2 := dx1 * b + dy1 * d;
   --  </programlisting>
   --
   --  Affine transformations are position invariant, so the same vector
   --  always transforms to the same vector. If (x1, y1) transforms
   --  to (x2, y2) then (x1+dx1, y1+dy1) will transform to
   --  (x1+dx2,y1+dy2) for all values of x1 and x2.

   procedure Transform_Point
     (Matrix : Cairo_Matrix;
      Point : in out Cairo_Tuple);
   --  Transforms the Point (X, Y) by Matrix.


   ----------
   -- Misc --
   ----------

   function Version return int;
   --  Returns the version of the cairo library encoded in a single
   --  integer as per CAIRO_VERSION_ENCODE. The encoding ensures that
   --  later versions compare greater than earlier versions.

   function Version_String return String;
   --  Returns the version of the cairo library as a human-readable string
   --  of the form "X.Y.Z".

   procedure Debug_Reset_Static_Data;
   --  Resets all static data within cairo to its original state,
   --  (ie. identical to the state at the time of program invocation). For
   --  example, all caches within cairo will be flushed empty.
   --
   --  This function is intended to be useful when using memory-checking
   --  tools such as valgrind. When valgrind's memcheck analyzes a
   --  cairo-using program without a call to Debug_Reset_Static_Data,
   --  it will report all data reachable via cairo's static objects as
   --  "still reachable". Calling Debug_Reset_Static_Data just prior
   --  to program termination will make it easier to get squeaky clean
   --  reports from valgrind.
   --
   --  WARNING: It is only safe to call this function when there are no
   --  active cairo objects remaining, (ie. the appropriate destroy
   --  functions have been called as necessary). If there are active cairo
   --  objects, this call is likely to cause a crash, (eg. an assertion
   --  failure due to a hash table being destroyed when non-empty).

   -----------------------------
   --  Binding internal stuff --
   -----------------------------

   --  The following types are intended for internal use only.
   --  They correspond to a direct mapping of C types.
   --  They can also be used to write other bindings that depend on cairo
   --  and need to access C values.

   type Font_Options_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_font_options_t*
   type Font_Face_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_font_face_t*
   type Scaled_Font_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_scaled_font_t*
   type Pattern_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_pattern_t*
   type Surface_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_surface_t*
   type Context_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_t*
   type Path_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_path_t*
   type Rectangle_List_Ptr is new Void_Ptr;
   --  Type corresponding to cairo_rectangle_list_t*
   type Glyph_Ptr is access all Cairo_Glyph;
   --  Type corresponding to cairo_glyph_t*
   pragma Convention (C, Glyph_Ptr);
   pragma No_Strict_Aliasing (Glyph_Ptr);
   type Text_Cluster_Ptr is access all Cairo_Text_Cluster;
   --  Type corresponding to cairo_text_cluster_t*
   pragma Convention (C, Text_Cluster_Ptr);
   pragma No_Strict_Aliasing (Text_Cluster_Ptr);

end Cairo;
