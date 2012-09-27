Cairo Ada binding

This is an Ada binding for the cairo graphic library

License
*******
This binding is covered by the GNAT Modified GPL (GMGPL).

Cairo Version
*************
This binding matches cairo 1.8 API
It has been tested with cairo 1.8.0

Naming conventions
******************
This binding tries to follow cairo official rules for bindings.
Ada naming convention is better adapted to usage of "use".

Ada function and procedure names
********************************
All function names normally match their cairo original name, with
the "cairo_{class}_" prefix removed.
There are few exceptions :
  - For some getters, a Get_ has been added to Ada names, e.g.:
    Get_Status, Get_Fill_Extents, ...
  - For some setters, a Set_ has been added to Ada names, e.g.:
    Set_Identity_Matrix
  - For constructors, New_XXX is used instead of Create_XXX

Ada type names
**************
All types defined in Cairo packages match the original name, with "_t"
suffix removed.

Types
*****
C enums are normally translated as Ada enumerated types, except when the C semantic is a mask.
In that case, the Ada correponding type is an unsigned.
This is for example the case with Cairo_Content and Cairo_Text_Cluster_Flags.
A symptom is usually the use of representation in the C enum.

Garbage collection
******************
For all ref counted cairo objects, two Ada types are available:
  - A Wrapper for the cairo object
  - A Handle to this wrapper
At most one Ada wrapper is created for each C cairo objet.
It is reused by all Ada handles.
The handles take care of reference counts.
With this implementation, it is possible to compare wrappers, but this behaviour
is not guaranteed for future releases.

Path
****
As suggested by cairo binding documentation, Path is a read-only class: there
is no direct way to modify a path, unless cairo is used.

Rectangle_List
**************
Ar this time, no binding doc seems to exist for Rectangle_List.
It has been considered as Path.

User fonts
**********
The user fontd API is now easier to use, notably to attach specific data.
A specific abstract data type must be derived for each user font data.
An object of that type must be attached to a cairo user font.

Portability
***********
This binding has been developped and tested with GNAT under Linux.
Tests have been done with GNAT GPL 2007 and GNAT GPL 2008.
Portability to another platform should only impact cairo-support* files.
On main issue is with 'pragma Import (C, ...)' that may need to be replaced by
'pragma Import (Stdcall, ...) for windows.
Regarding compiler portablity, no GNAT specific feature or package has
been used.
Modifications should be limited.
Some users have reported successful usage of this binding on Windows
and some Unix platform using GNAT.