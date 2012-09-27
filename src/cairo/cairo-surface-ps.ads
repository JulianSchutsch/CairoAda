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

with Ada.Streams;

package Cairo.Surface.PS is

   type PS_Level is
     (PS_LEVEL_2,
      PS_LEVEL_3);
   pragma Convention (C, PS_Level);
   for PS_Level'Size use 32;

   type PS_Level_Array is array (Natural range <>) of PS_Level;
   pragma Convention (C, PS_Level_Array);

   type Cairo_PS_Surface (<>) is new Cairo_Surface with private;
   type Cairo_PS_Surface_Ref is access all Cairo_PS_Surface'Class;

   -- Construction
   function New_PS_Surface
     (Filename : String;
      Width_In_Points : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle;
   --  <parameter name="level">a level id</parameter>
   --
   --  Get the string representation of the given Level id. This function
   --  will return %NULL if Level id isn't valid. See Ps_Get_Levels
   --  for a way to get the list of valid level ids.
   --
   --  Return value: the string associated to given level.
   --
   --  Since: 1.6

   function New_PS_Surface_For_Stream
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Width_In_Points : double;
      Height_In_Points : double)
      return Cairo_Surface_Handle;
   --  <parameter name="write_func">a Cairo_Write_Func to accept the output data</parameter>
   --  <parameter name="closure">the closure argument for Write_Func</parameter>
   --  <parameter name="width_in_points">width of the surface, in points (1 point = 1/72.0 inch)</parameter>
   --  <parameter name="height_in_points">height of the surface, in points (1 point = 1/72.0 inch)</parameter>
   --
   --  Creates a PostScript surface of the specified size in points to be
   --  written incrementally to the stream represented by Write_Func and
   --  Closure. See Ps_Surface_Create for a more convenient way
   --  to simply direct the PostScript output to a named file.
   --
   --  Note that the size of individual pages of the PostScript
   --  output can vary. See Ps_Surface_Set_Size.
   --
   --  Return value: a pointer to the newly created surface. The caller
   --  owns the surface and should call Surface_Destroy when done
   --  with it.
   --
   --  This function always returns a valid pointer, but it will return a
   --  pointer to a "nil" surface if an error such as out of memory
   --  occurs. You can use Surface_Status to check for this.
   --
   --  Since: 1.2

   procedure Restrict_To_Level
     (Surface : in out Cairo_PS_Surface'Class;
      Level : PS_Level);
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --  <parameter name="level">PostScript level</parameter>
   --
   --  Restricts the generated PostSript file to Level. See
   --  Ps_Get_Levels for a list of available level values that
   --  can be used here.
   --
   --  This function should only be called before any drawing operations
   --  have been performed on the given surface. The simplest way to do
   --  this is to call this function immediately after creating the
   --  surface.
   --
   --  Since: 1.6

   function Get_PS_Levels return PS_Level_Array;

   procedure Set_Eps
     (Surface : in out Cairo_PS_Surface'Class;
      Eps : Boolean);
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --  <parameter name="eps">TRUE to output EPS format PostScript</parameter>
   --
   --  If Eps is TRUE, the PostScript surface will output Encapsulated
   --  PostScript.
   --
   --  This function should only be called before any drawing operations
   --  have been performed on the current page. The simplest way to do
   --  this is to call this function immediately after creating the
   --  surface. An Encapsulated PostScript file should never contain more
   --  than one page.
   --
   --  Since: 1.6

   function Get_Eps
     (Surface : Cairo_PS_Surface'Class)
      return Boolean;
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --
   --  Check whether the PostScript surface will output Encapsulated PostScript.
   --
   --  Return value: TRUE if the surface will output Encapsulated PostScript.
   --
   --  Since: 1.6

   procedure Set_Size
     (Surface : in out Cairo_PS_Surface'Class;
      Width_In_Points : double;
      Height_In_Points : double);
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --  <parameter name="width_in_points">new surface width, in points (1 point == 1/72.0 inch)</parameter>
   --  <parameter name="height_in_points">new surface height, in points (1 point == 1/72.0 inch)</parameter>
   --
   --  Changes the size of a PostScript surface for the current (and
   --  subsequent) pages.
   --
   --  This function should only be called before any drawing operations
   --  have been performed on the current page. The simplest way to do
   --  this is to call this function immediately after creating the
   --  surface or immediately after completing a page with either
   --  Show_Page or Copy_Page.
   --
   --  Since: 1.2

   procedure DSC_Comment
     (Surface : in out Cairo_PS_Surface'Class;
      Comment : String);
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --  <parameter name="comment">a comment string to be emitted into the PostScript output</parameter>
   --
   --  Emit a comment into the PostScript output for the given surface.
   --
   --  The comment is expected to conform to the PostScript Language
   --  Document Structuring Conventions (DSC). Please see that manual for
   --  details on the available comments and their meanings. In
   --  particular, the %%IncludeFeature comment allows a
   --  device-independent means of controlling printer device features. So
   --  the PostScript Printer Description Files Specification will also be
   --  a useful reference.
   --
   --  The comment string must begin with a percent character (%) and the
   --  total length of the string (including any initial percent
   --  characters) must not exceed 255 characters. Violating either of
   --  these conditions will place Surface into an error state. But
   --  beyond these two conditions, this function will not enforce
   --  conformance of the comment with any particular specification.
   --
   --  The comment string should not have a trailing newline.
   --
   --  The DSC specifies different sections in which particular comments
   --  can appear. This function provides for comments to be emitted
   --  within three sections: the header, the Setup section, and the
   --  PageSetup section.  Comments appearing in the first two sections
   --  apply to the entire document while comments in the BeginPageSetup
   --  section apply only to a single page.
   --
   --  For comments to appear in the header section, this function should
   --  be called after the surface is created, but before a call to
   --  DSC_Begin_Setup.
   --
   --  For comments to appear in the Setup section, this function should
   --  be called after a call to DSC_Begin_Setup but before
   --  a call to Begin_Page_Setup.
   --
   --  For comments to appear in the PageSetup section, this function
   --  should be called after a call to Begin_Page_Setup.
   --
   --  Note that it is only necessary to call Begin_Page_Setup
   --  for the first page of any surface. After a call to
   --  Show_Page or Copy_Page comments are unambiguously
   --  directed to the PageSetup section of the current page. But it
   --  doesn't hurt to call this function at the beginning of every page
   --  as that consistency may make the calling code simpler.
   --
   --  As a final note, cairo automatically generates several comments on
   --  its own. As such, applications must not manually generate any of
   --  the following comments:
   --
   --  Header section: %!PS-Adobe-3.0, %%Creator, %%CreationDate, %%Pages,
   --  %%BoundingBox, %%DocumentData, %%LanguageLevel, %%EndComments.
   --
   --  Setup section: %%BeginSetup, %%EndSetup
   --
   --  PageSetup section: %%BeginPageSetup, %%PageBoundingBox,
   --  %%EndPageSetup.
   --
   --  Other sections: %%BeginProlog, %%EndProlog, %%Page, %%Trailer, %%EOF
   --
   --  Here is an example sequence showing how this function might be used:
   --
   --  <informalexample><programlisting>
   --  Cairo_Surface_Handle Surface_Handle := New_PS_Surface (filename, width, height);
   --  ...
   --  Surface : Cairo_PS_Surface => Cairo_PS_Surface'Class (Ref (Surface_Handle).all)
   --  ...
   --  DSC_Comment (Surface, "%%Title: My excellent document");
   --  DSC_Comment (Surface, "%%Copyright: Copyright (C) 2006 Cairo Lover")
   --  ...
   --  DSC_Begin_Setup (Surface);
   --  DSC_Comment (Surface, "%%IncludeFeature: *MediaColor White");
   --  ...
   --  Begin_Page_Setup (Surface);
   --  DSC_Comment (Surface, "%%IncludeFeature: *PageSize A3");
   --  DSC_Comment (Surface, "%%IncludeFeature: *InputSlot LargeCapacity");
   --  DSC_Comment (Surface, "%%IncludeFeature: *MediaType Glossy");
   --  DSC_Comment (Surface, "%%IncludeFeature: *MediaColor Blue");
   --  ... draw to first page here ..
   --  Show_Page (Context);
   --  ...
   --  DSC_Comment (Surface, "%%IncludeFeature: *PageSize A5");
   --  ...
   --  </programlisting></informalexample>
   --
   --  Since: 1.2

   procedure DSC_Begin_Setup
     (Surface : in out Cairo_PS_Surface'Class);
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --
   --  This function indicates that subsequent calls to
   --  Ps_Surface_Dsc_Comment should direct comments to the Setup
   --  section of the PostScript output.
   --
   --  This function should be called at most once per surface, and must
   --  be called before any call to Ps_Surface_Dsc_Begin_Page_Setup
   --  and before any drawing is performed to the surface.
   --
   --  See Ps_Surface_Dsc_Comment for more details.
   --
   --  Since: 1.2

   procedure DSC_Begin_Page_Setup
     (Surface : in out Cairo_PS_Surface'Class);
   --  <parameter name="surface">a PostScript Cairo_Surface</parameter>
   --
   --  This function indicates that subsequent calls to
   --  Ps_Surface_Dsc_Comment should direct comments to the
   --  PageSetup section of the PostScript output.
   --
   --  This function call is only needed for the first page of a
   --  surface. It should be called after any call to
   --  Ps_Surface_Dsc_Begin_Setup and before any drawing is
   --  performed to the surface.
   --
   --  See Ps_Surface_Dsc_Comment for more details.
   --
   --  Since: 1.2

   function To_String (Level : PS_Level) return String;
   --  <parameter name="level">a level id</parameter>
   --
   --  Get the string representation of the given Level id. This function
   --  will return NULL if Level id isn't valid. See Ps_Get_Levels
   --  for a way to get the list of valid level ids.
   --
   --  Return value: the string associated to given level.
   --
   --  Since: 1.6

private

   type Cairo_PS_Surface is new Cairo_Surface with null record;

end Cairo.Surface.PS;
