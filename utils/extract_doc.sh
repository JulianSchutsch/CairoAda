#!/bin/sh

incdir=/tools/soft/cairo-1.8/include/cairo
srcdir=/tools/inst/cairo-1.8.0/src
outdir=doc-1.8
#funcnames=funcnames-1.8
funcnames=$outdir/function_names
typenames=$outdir/type_names
patchfunc=$outdir/patch_function_names.sh
patchtype=$outdir/patch_type_names.sh
#fundoc=doc-cairo-1.8-fun
typedoc=doc-cairo-1.8-typ
#headers="cairo.h"

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

# List functions that are in a given header
list_header_funcs()
{
   next=0
   cat $1 | while read line
   do
      #echo ">>>>>>>>>>>>>>>>>>>>> $line"
      case "$line" in
         cairo_public*)
            #echo "       PPPPPPPPPPPPPP $line"
            next=1
            ;;
         *)
            if [ $next -eq 1 ]
            then
               echo "$line"
            fi
            next=0
           ;;
      esac
   done | sed -e "s/(.*$//" -e "s/ //g"
}


# List all functions
list_all_funcs()
{
   echo "-- Fun list extraction">&2
   for h in `\ls $incdir`
   do
     list_header_funcs $incdir/$h
   done  | sort
}

save_funcs()
{
   {
   for f in $all_funcs
   do
      echo $f
   done
   } > $funcnames
}

# Create a shell that will change "cairo_foo()" into "Foo" for all cairo functions
make_func_patcher()
{
   echo "-- Create function names patcher">&2
   (
   echo "#!/bin/sh"
   echo
   echo "sed \\"
   cat $funcnames | while read f
   do
      echo "   -e \"s/$f()/`to_mixed $f`/g\"\\"
   done | sed -e "s/Cairo_//" -e "\$s/\\\\//" ) > $patchfunc
   chmod +x $patchfunc
}

# Extact doc asociated to a given function
extract_function_doc()
{
   fname=$1
   rank=`expr $rank + 1`
   #echo "------------------------------------------------------------------------------------"
   echo "$1 ($rank)">&2
   #echo "func: $1 ($rank)"
   file=`grep -l "^$fname " $srcdir/*.c`
   #echo "file: $file"
   #echo "------------------------------------------------------------------------------------"
   #grep " \* $fname:" $file
   next=0
   cat $file | while read line
   do
      case "$line" in
         *\*\ $fname:|*\*\ $fname)
            #echo "--- $line"
            next=1
            ;;

         *\*\*\/|*\*\/)
            #echo "--- $line"
            next=0
            ;;
         *)
            if [ $next -eq 1 ]
            then
               echo "$line"
            fi
            ;;
      esac
   done | sed -e "s/\*/--/"
}

# Extract doc of all functions
extract_all_functions_doc()
{
   echo "-- Fun doc extraction">&2
   rank=0
   for f in $all_funcs
   do
      extract_function_doc $f > $outdir/$f
      #exit
   done
}

save_types()
{
   echo "-- Save type names">&2
   cat $incdir/cairo.h | while read line
   do
      case "$line" in
         *\*\ cairo_*_t:*|*\*\ cairo_t:*)
            echo "$line" | sed -e "s/^.*cairo_/cairo_/" -e "s/:.*$//"
	    ;;
      esac
   done > $typenames
}

# Create a shell that will change 
make_type_patcher()
{
   echo "-- Create type names patcher">&2
   (
   echo "#!/bin/sh"
   echo
   echo "sed \\"
   cat $typenames | while read t
   do
      echo "   -e \"s/$t/`to_mixed $t`/g\"\\"
   done | sed -e "s/_T//" -e "\$s/\\\\//" -e "s#/Cairo/#/Cairo_Context/#" -e "s/Cairo_Bool/Boolean/") > $patchtype
   chmod +x $patchtype
}

# Extact doc asociated to a given function

extract_types_doc()
{
   echo "-- Type list extraction">&2
   next=0
   cat $incdir/cairo.h | while read line
   do
      case "$line" in
         *\*\ cairo_*_t:*|*\*\ cairo_t:*)
            echo "------------------------------------------------------------"
            echo "$line" | sed -e "s/^.*cairo_/cairo_/" -e "s/:.*$//" -e "s/^/type: /"
            echo "------------------------------------------------------------"
            next=1
            ;;
         *\*\*\/|*\*\/)
            next=0
            ;;
         *)
            if [ $next -eq 1 ]
            then
               echo "$line"
            fi
            ;;
      esac
   done | sed -e "s/^\*/-- /" > $typedoc
}


patch_doc()
{
   sed -e "s/^--/   -- /"\
       -e "s/@cr/@Context/" \
       -e "s/--  @\([a-zA-Z_0-9][a-zA-Z_0-9]*\)[:][ ]\(.*\)/-- <parameter name=\"\1\">\2<\/parameter>/"
}

patch_functions()
{
   echo "-- Patch function doc">&2
   for f in $all_funcs
   do
      g=`echo $f | sed -e "s/cairo_//"`
      cat $outdir/$f | patch_doc | $patchtype | $patchfunc > $outdir/$g
   done
}

mkdir -p $outdir

all_funcs=`list_all_funcs`
save_funcs
make_func_patcher
extract_all_functions_doc

extract_types_doc
save_types
make_type_patcher
patch_functions

