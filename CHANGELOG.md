## GPF Changelog

The intent of this changelog is to keep everyone in the loop about
what's new in the M_matrix project. It is a curated, chronologically ordered
list of notable changes including`records of change such as bug fixes,
new features, changes, and relevant notifications.
<!-- ======================================================================== -->
---
**2021-04-19**  John S. Urban  <https://github.com/urbanjost>
### :green_circle: DIFF:

Variable names now have 32 significant characters instead of 4. 

Up to 480 unique variable names are allowed instead of a limit of 48.

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

The "." command enters command recall/edit mode. Enter "?" at the prompt
after entering "." for help.
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

Can now take directives from string on MAT88() routine call

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

