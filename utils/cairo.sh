#/bin/sh

warn="\033[31m"
warn2="\033[32m"
reset="\033[0m"

hdir="/tools/soft/cairo-1.8/include/cairo"
input="$hdir/cairo.h"
input="cairo-1.8.0.h"
todo="todo.txt"
if [ $# -ge 1 ]
then
   input=$1
fi

# Change 1st letter of each group of letters into capital
to_capital()
{
   {
   for item in $*
   do
      echo -n " "`echo "${item:0:1}" | tr [a-z] [A-Z]`"${item:1}"
   done
   echo
   } | sed -e "s/^ //"
}

# Change 1st letter of each word into capital
to_mixed()
{
   for mitem in $*
   do
      to_capital `echo $mitem | sed -e "s/_/ /g"` | sed -e "s/ /_/g"
   done
}


list_func_names()
{
   grep "^cairo_" $input | sed -e "/^cairo_public/d" -e "s/ .*$//"
}

gen_imports()
{
   for f in `list_func_names | sort`
   do
      echo "   pragma Import (C, $f, \"$f\");"
   done
}

AWK="
BEGIN {
   IN_FUNC=0;
}
{
   # hyp: no tab, no duplicate space
   # remove all starting blanks
   sub(\"^[ ]*\",\"\");

   # Recognize functions with lines that start by 'cairo_public'
   if (match(\$0,\"^cairo_public\"))
   {
      IN_FUNC=1;
      print \"-----------------------------------------------------\";
      sub(\"cairo_public *\",\"\");
      print \$0;
   }
   else if (IN_FUNC!=0)
   {
      if (match(\$0,\"[,)]\"))
      {
         gsub(\"[(,]\",\"\n\");
         sub(\"[,]\",\"\");
         #sub(\"[;]\",\"\");
         print \$0;
      }
      if (match(\$0,\");\"))
      {
         IN_FUNC=0;
      }
   }
}
END {
   print \"-----------------------------------------------------\";
}
"

map_c_type()
{
   case "$*" in
      #  Special cases
      void) echo -n "void";;
      int) echo -n "int";;
      unsigned\ int) echo "unsigned";;
      unsigned\ long) echo -n "unsigned_long";;
      double) echo -n "double";;

      cairo_t\ \*) echo -n "Context_Ptr";;
      const\ cairo_t\ \*) echo -n "Context_Ptr";;

      void\ \*) echo -n "System.Address";;
      const\ char\ \*) echo -n "Interfaces.C.Strings.Chars_Ptr";;
      double\ \*) echo -n "[in] out double";;
      const\ double\ \*) echo "access double";; # array ?
      unsigned\ char\ \*) echo "System.Address";;
      int\ \*) echo  -n "[in] out int";; # array ?

      cairo_matrix_t\ \*) echo -n "[in] out Cairo.Cairo_Matrix";;
      cairo_text_extents_t\ \*) echo -n "[in] out Cairo.Cairo_Text_Extents";;
      cairo_font_extents_t\ \*) echo -n "[in] out Cairo.Cairo_Font_Extents";;
      cairo_glyph_t\ \*) echo -n "access Cairo.Glyph";; # ???
      cairo_text_cluster_t\ \*) echo "[in] out Cairo.Cairo_Text_Cluster";;
      cairo_text_cluster_flags_t\ \*) echo "[in] out Cairo.Cairo_Text_Cluster_Flags";;

      cairo_glyph_t\ \*\ \*) echo -n "access Cairo_Glyph_Ptr";; # ???
      cairo_text_cluster_t\ \*\ \*) echo "access Cairo_Text_Cluster_Ptr";;
      cairo_surface_t\ \*\ \*) echo " access Surface_Ptr";;

      const\ cairo_matrix_t\ \*) echo -n "Cairo.Cairo_Matrix";;
      const\ cairo_text_extents_t\ \*) echo -n "Cairo.Cairo_Text_Extents";;
      const\ cairo_font_extents_t\ \*) echo -n "Cairo.Cairo_Font_Extents";;
      const\ cairo_user_data_key_t\ \*) echo -n "Cairo.Cairo_User_Data_Key";;
      const\ cairo_glyph_t\ \*) echo "Cairo.Cairo_Glyph_Ptr";;
      const\ cairo_text_cluster_t\ \*) echo "Cairo.Cairo_Text_Cluster";;


      # Rule 1 (Mixed case without _t)
      cairo_bool_t |\
      cairo_write_func_t |\
      cairo_read_func_t |\
      cairo_user_scaled_font_init_func_t |\
      cairo_user_scaled_font_render_glyph_func_t |\
      cairo_user_scaled_font_text_to_glyphs_func_t |\
      cairo_user_scaled_font_unicode_to_glyph_func_t)
         to_mixed `echo "$*" | sed -e "s/_t$//"`
	 ;;

      # Rule 2 Cairo. + Mixed case without _t)
      cairo_hint_style_t |\
      cairo_status_t |\
      cairo_extend_t |\
      cairo_operator_t |\
      cairo_font_type_t |\
      cairo_filter_t |\
      cairo_format_t |\
      cairo_antialias_t |\
      cairo_line_cap_t |\
      cairo_subpixel_order_t |\
      cairo_surface_type_t |\
      cairo_hint_metrics_t |\
      cairo_font_slant_t |\
      cairo_font_weight_t |\
      cairo_line_join_t |\
      cairo_fill_rule_t |\
      cairo_content_t |\
      cairo_pattern_type_t |\
      cairo_text_cluster_flags_t |\
      cairo_destroy_func_t)
         echo -n "Cairo."
         to_mixed `echo "$*" | sed -e "s/_t$//"`
	 ;;

      # Rule 3 : Ptr)
      cairo_font_face_t\ \* |\
      cairo_font_options_t\ \* |\
      cairo_path_t\ \* |\
      cairo_pattern_t\ \* |\
      cairo_rectangle_list_t\ \* |\
      cairo_scaled_font_t\ \* |\
      cairo_surface_t\ \*)
         to_mixed `echo "$*" | sed -e "s/ .*$//" -e "s/^cairo_//" -e "s/_t$/_ptr/"`
	 ;;

      # Rule 4 : Ptr)
      const\ cairo_font_face_t\ \* |\
      const\ cairo_font_options_t\ \* |\
      const\ cairo_path_t\ \* |\
      const\ cairo_pattern_t\ \* |\
      const\ cairo_scaled_font_t\ \* |\
      const\ cairo_surface_t\ \*)
         to_mixed `echo "$1" | sed -e "s/^const cairo_//" -e "s/ .*$//" -e "s/_t$/_ptr/"`
	 ;;

       #cairo_text_cluster_t\ *\
       #cairo_text_cluster_t\ *\ \*\
       #const\ cairo_text_cluster_t\ *\
       #cairo_text_cluster_flags_t\
       #cairo_text_cluster_flags_t\ \*\
       #cairo_glyph_t\ \*\ \*\
       #cairo_surface_t\ \*\ \*\

      *) echo -n -e "${warn}$*${reset}"
         echo "Type: $*" >> $todo
         ;;
   esac
}

map_c_name()
{
   case "$*" in
      # Specific cases
      void) echo "void";;
      cr) echo "Context";;
      op) echo "Operator";;
      dx*|dy*|cx*|cy*|sx|sy|tx|ty|xc|yc|xx|xy|yx|yy|ctm|utf8|r0|r1) echo "$*" | tr [a-z] [A-Z];;
      # Common rule
      *) to_mixed "$*";;
   esac
}

process_arg()
{
    arg_name=`echo "$*" | sed -e "s/^.* //"`
    arg_type=`echo "$*" | sed -e "s/ $arg_name$//"`
    arg_name=`map_c_name $arg_name`
    echo -n "$arg_name : `map_c_type "$arg_type"`"
}

process_c_func()
{
   count=0
   func_names=
   while read line
   do
      case "$line" in
         ----*)
            if [ $count -ne 0 ]
            then
               echo -n ")";
               if [ "$ret" != "" ]
               then
                  echo -n " return `map_c_type "$ret"`";
               fi
               echo -n ";";
            fi
            count=0
            echo
            #echo "   pragma Import (C, ${func_name}, \"${func_name}\");"
            ;;
         *)
            count=`expr $count + 1`;
            case $count in
               1)
                  case "$line" in
                     void)
                        echo -n "   procedure ";
                        ret=""
                        ;;
                     *)
                        echo -n "   function ";
                        ret="$line";
                        ;;
                  esac
                  ;;
               2)
                  func_name="$line"
                  func_names="$func_names $func_name"
                  echo -n "$line ("
                  ;;
               *)
                  #echo "$count:$line"
                  if [ $count -ge 4 ]
                  then
                          echo -n "; "
                  fi
                  process_arg "$line"
                  ;;
            esac
            ;;
      esac
   done
   echo
   for f in $func_names
   do
      echo "   pragma Import (C, $f, \"$f\");"
   done
}

extract_func()
{
   # 1) replace tabs by space and multi space by 1 space
   # 2) recognize functions
   # 3) clean things
   cat $input | sed -e "s/	/ /g" -e "s/  */ /g" \
              | gawk "$AWK" \
              | sed -e "s/[);]//g" -e "s/^  *//" -e "s/\*/ \* /" -e "/^$/d"  -e "s/  */ /g"\
              | process_c_func \
              | sed -e "s/(void : void) //"
}

list_func()
{
   while read line
   do
      echo "$line"
      case "$line" in
         cairo_public*)
            ret=`echo "$line" | sed -e "s/cairo_public *//"`
            if [ "$ret" == "void" ]
            then
               func="procedure"
            else
               func="function";
            fi
            echo "RET:$ret:"
            echo "FUNC:$func:"
            ;;
         cairo_*)
            name=`echo "$line" | cut -d" " -f1`
            echo "NAME:$name:"
            ;;
         *)
            ;;
      esac
   done
}

#list_func_names
cat << EOF
with System;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Streams;

private package Cairo.Support is

   pragma Elaborate_Body;

   -- Thin binding for Cairo functions

   function To_Chars_Ptr is new Ada.Unchecked_Conversion (System.Address, Interfaces.C.Strings.Chars_Ptr);
   type Cairo_Glyph_Ptr is access all Cairo.Cairo_Glyph;
   function To_Glyph_Ptr is new Ada.Unchecked_Conversion (System.Address, Cairo_Glyph_Ptr);
   type Cairo_Text_Cluster_Ptr is access all Cairo.Cairo_Text_Cluster;
   type Double_Ptr is access Double;
   function To_Double_Ptr is new Ada.Unchecked_Conversion (System.Address, Double_Ptr);

   function To_Address is new Ada.Unchecked_Conversion (Void_Ptr, System.Address);

   type Cairo_Bool is new Interfaces.C.int;

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
   type Cairo_Closure is new Void_Ptr;
   function To_Closure is new Ada.Unchecked_Conversion (Stream_Access, Cairo_Closure);
   type Cairo_Write_Func is access function (Closure : Cairo_Closure; Data : Void_Ptr; Length : unsigned) return Cairo.Cairo_Status;
   type Cairo_Read_Func is access function (Closure : Cairo_Closure; Data : Void_Ptr; Length : unsigned) return Cairo.Cairo_Status;
   function Stream_Write (Closure : Cairo_Closure; Data : Void_Ptr; Length : unsigned) return Cairo.Cairo_Status;
   function Stream_Read (Closure : Cairo_Closure; Data : Void_Ptr; Length : unsigned) return Cairo.Cairo_Status;

   type Cairo_User_Scaled_Font_Init_Func is access function
     (Scaled_Font : Scale_Font_Ptr;
      Context : Context_Ptr;
      Extents : access Cairo_Font_Extent)
      return Cairo.Cairo_Status;

   type Cairo_User_Scaled_Font_render_Glyph_Func is access function
     (Scaled_Font : Scale_Font_Ptr;
      Glyph : unsigned_long;
      Context : Context_Ptr;
      Extents : access Cairo_Font_Extent)
      return Cairo.Cairo_Status;

   type Cairo_User_Scaled_Font_Text_To_Glyph_Func is access function
     (Scaled_Font : Scale_Font_Ptr;
      UTF8 : Interfaces.C.Strings.Chars_Ptr;
      UTF8_Len : int;
      Glyphs : access Cairo_Glyph_Ptr;
      Num_Glyphs : access int;
      Clusters : access Cairo_Text_Cluster_Ptr;
      Num_Clusters : access int;
      Cluster_Flags : access Cairo_Text_Cluster_FlagsOA)
      return Cairo.Cairo_Status;

   type Cairo_User_Scaled_Font_Init_Func is access function
     (Scaled_Font : Scale_Font_Ptr;
      Unicode : unsigned_long;
      Glyph_Index : access unsigned_long)
      return Cairo.Cairo_Status;
EOF

rm -fr $todo
extract_func
cat << EOF

end Cairo.Support;
EOF
#gen_imports
#cat cairo_thin.h | list_func

