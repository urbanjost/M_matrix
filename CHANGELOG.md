## GPF Changelog

The intent of this changelog is to keep everyone in the loop about
what's new in the M_matrix project. It is a curated, chronologically ordered
list of notable changes including records of change such as bug fixes,
new features, changes, and relevant notifications.
<!-- ======================================================================== -->
---
**2025-04-22**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
  + maxloc,minloc  added maxloc and minloc.
<!-- ======================================================================== -->
---
**2025-04-18**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
  + sum            added option to sum along rows or columns, and checks on
                   parameters.

### :red_circle: DIFF:
  + rand           removed all overloading of "rand" and added randu(),
                   randn(), and randseed().

<!-- ======================================================================== -->
---
**2025-04-14**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
  +  add ^ and .^ power operators.
  + pow      raise elements of an array to a power
  + reshape  added function to reshape arrays
  + all      return 1 if all values are not zero,
             return 0 if any value is zero.
  + any      return 1 if any values are not zero,
             return 0 if all value is zero.
  + lt,le,eq,ge,gt,ne  added relational functions
  + date_and_time      added additional representations of current
                       time.
  + fmtc,fmti,fmtr     convert numeric values to string
  + pack               select array elements via a mask into a vector
  + minval,maxval      extrema of real component of an array

<!-- ======================================================================== -->
---
**2021-08-20**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: DIFF:
  +  renamed "laff" to "lala".

<!-- ======================================================================== -->
---
**2021-05-20**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: DIFF:
  +  renamed size() to shape() as it resembles the Fortran procedure of that
     name more than the Fortran size() intrinsic function..

### :green_circle: ADD:
  +  added Fortran intrinsic documentation via the "fhelp" command.

<!-- ======================================================================== -->
---
**2021-05-20**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: DIFF:
  +  renamed "mat88" to "laff".

### :green_circle: ADD:
  +  added ifexists_laff() for determining if a variable name is in use in laff(3).
  +  LAFF_PATH environment variable can be used to define a colon-delimited list of
     directories to search for exec(1) files. Names are search for as specified and
     with the ".la" suffix.

<!-- ======================================================================== -->
---
**2021-05-19**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
  +  put_into_NAMELIST() and get_into_NAMELIST() are functional, allowing most
     intrinsic types from
     scalar to vector to MxN matrix to be passed to and from the calling
     program onto the NAMELIST stack.

     Currently, returned arrays must be declared allocatable in the calling
     program, as NAMELIST() can change the size of the arrays.

<!-- ======================================================================== -->
---
**2021-05-16**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: DIFF:
  +  changed to using the full ASCII character set

### :green_circle: ADD:

  +  added getenv('varname') to allow branching based on environment
     variables

### :green_circle: FIX:

<!-- ======================================================================== -->
---
**2021-05-14**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:

  +  added delete('filename') to allow for erasing scratch files

### :green_circle: FIX:

  +  fixed "save" and "load" use of a filename with uppercase or underscore


<!-- ======================================================================== -->
---
**2021-05-08**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:

  +  all printable characters are allowed in display()
  +  underscores are allowed in variable and function names
  +  character arrays of commands may be passed to NAMELIST()
  +  began a set of unit tests
  +  add zeros() function to complement ones()

### :green_circle: DIFF:

The help text is displayed with the M_help(3f) module from an array in
memory instead of a file, simplifying usage by not requiring an external
file by default, but still allowing for easily supporting alternate help
files in future versions.

<!-- ======================================================================== -->
---
**2021-04-24**  John S. Urban  <https://github.com/urbanjost>
### :green_circle: ADD:

  + Variable names now have 32 significant characters instead of 4. 
  + Up to 480 unique variable names are allowed instead of a limit of 48.

### :green_circle: DIFF:

The inverse Hilbert function was renamed from "hilb" to "invh" or
"inverse_hilbert".

<!-- ======================================================================== -->
---
**2021-04-19**  John S. Urban  <https://github.com/urbanjost>
### :green_circle: FIX:

Filenames greater than 32 characters in length were corrupted by data in the
input line due to only the first 32 characters being reset when the filename
was placed in the buffer (the previous value was left in the resulting buffer).
<!-- ======================================================================== -->
---
**2021-04-18**  John S. Urban  <https://github.com/urbanjost>
### :green_circle: ADD:

The "sh" command uses the environment variable SHELL to start a shell
from within the program. Note that in addition, a line beginning with a
single exclamation will cause the rest of the line to be passed to the
system as a command line.
<!-- ======================================================================== -->
---
**2021-04-17**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
initial commit

The "??" command enters command recall/edit mode. Enter "?" at the prompt
after entering "??" for help.
<!-- ======================================================================== -->
---
**????-??-??**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:

Converted to do most I/O via journal() so can be used with my codes
more easily. Also allow additional comment indicator (# in column 1)
so can read back in trail files made by DIARY command.

That is, the log file created by diary() prefixes output lines with a
pound character and a pound character beginning a line is now considered
a second type of comment line so that a diary() file can be replayed
with exec().


Made case-sensitive

Can now take directives from string on LAFF() routine call

Allowing longer filenames

Partly converted program away from use of HOLLERITH towards use of ADE
or maybe even character variables (enough to be able to use GNU g95
compiler, anyway). Might have to change the way I make a letter
"hollerith" on non little-endian non-32bit platforms.

changed RETURN command to QUIT

help document is now built-in 
<!-- ======================================================================== -->
---
<!--
   - [ ] manpage
   - [ ] demo program
   - [ ] unit test
-->
