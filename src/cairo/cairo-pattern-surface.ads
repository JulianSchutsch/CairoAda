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

with Cairo.Surface; use Cairo.Surface;

package Cairo.Pattern.Surface is

   pragma Elaborate_Body;

   type Cairo_Surface_Pattern (<>) is new Cairo_Pattern with private;
   type Cairo_Surface_Pattern_Ref is access all Cairo_Surface_Pattern'Class;

   ------------------
   -- Construction --
   ------------------

   function New_Surface_Pattern
     (Surface : access Cairo_Surface'Class)
      return Cairo_Pattern_Handle;
   --  <parameter name="surface">the surface</parameter>
   --
   --  Create a new Cairo_Pattern for the given surface.
   --
   --  Return value: the newly created Cairo_Pattern if successful, or
   --  an error pattern in case of no memory.  The caller owns the
   --  returned object and should call Pattern_Destroy when
   --  finished with it.
   --
   --  This function will always return a valid pointer, but if an error
   --  occurred the pattern status will be set to an error.  To inspect
   --  the status of a pattern use Pattern_Status.


   ------------------
   -- Modification --
   ------------------

   procedure Set_Extend
     (Pattern : in out Cairo_Surface_Pattern'Class;
      Extend : Cairo_Extend);
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="extend">a Cairo_Extend describing how the area outside of the</parameter>
   --  pattern will be drawn
   --
   --  Sets the mode to be used for drawing outside the area of a pattern.
   --  See Cairo_Extend for details on the semantics of each extend
   --  strategy.
   --
   --  The default extend mode is CAIRO_EXTEND_NONE for surface patterns
   --  and CAIRO_EXTEND_PAD for gradient patterns.

   procedure Set_Filter
     (Pattern : in out Cairo_Surface_Pattern'Class;
      Filter : Cairo_Filter);
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --  <parameter name="filter">a Cairo_Filter describing the filter to use for resizing</parameter>
   --  the pattern
   --
   --  Sets the filter to be used for resizing when using this pattern.
   --  See Cairo_Filter for details on each filter.
   --
   --  * Note that you might want to control filtering even when you do not
   --  have an explicit Cairo_Pattern object, (for example when using
   --  Set_Source_Surface). In these cases, it is convenient to
   --  use Get_Source to get access to the pattern that cairo
   --  creates implicitly. For example:
   --
   --  <informalexample><programlisting>
   --  cairo_set_source_surface (cr, image, x, y);
   --  cairo_pattern_set_filter (cairo_get_source (cr), CAIRO_FILTER_NEAREST);
   --  </programlisting></informalexample>


   -------------
   -- Getters --
   -------------

   function Get_Extend
     (Pattern : Cairo_Surface_Pattern'Class)
      return Cairo_Extend;
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --
   --  Gets the current extend mode for a pattern.  See Cairo_Extend
   --  for details on the semantics of each extend strategy.
   --
   --  Return value: the current extend strategy used for drawing the
   --  pattern.

   function Get_Filter
     (Pattern : Cairo_Surface_Pattern'Class)
      return Cairo_Filter;
   --  <parameter name="pattern">a Cairo_Pattern</parameter>
   --
   --  Gets the current filter for a pattern.  See Cairo_Filter
   --  for details on each filter.
   --
   --  Return value: the current filter used for resizing the pattern.

   procedure Get_Surface
     (Pattern : Cairo_Surface_Pattern'Class;
      Surface : out Cairo_Surface_Handle;
      Success : out Cairo_Status);

private

   type Cairo_Surface_Pattern is new Cairo_Pattern with null record;

end Cairo.Pattern.Surface;
