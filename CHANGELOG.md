## GPF Changelog

The intent of this changelog is to keep everyone in the loop about
what's new in the M_matrix project. It is a curated, chronologically ordered
list of notable changes including`records of change such as bug fixes,
new features, changes, and relevant notifications.

<!-- ======================================================================== ->
---
**2021-04-19**  John S. Urban  <https://github.com/urbanjost>
### :green_circle: FIX:

Filenames greater than 32 characters in length were corrupted by data in the
input line due to only the first 32 characters being reset when the filename
was placed in the buffer (the previous value was left in the resulting buffer).
<!-- ======================================================================== ->
---
**2021-04-18**  John S. Urban  <https://github.com/urbanjost>
### :green_circle: ADD:

The "sh" command uses the environment variable SHELL to start a shell
from within the program. Note that in addition, a line beginning with a
single exclamation will cause the rest of the line to be passed to the
system as a command line.
<!-- ======================================================================== ->
---
**2021-04-17**  John S. Urban  <https://github.com/urbanjost>

### :green_circle: ADD:
initial commit

The "." command enters command recall/edit mode. Enter "?" at the prompt
after entering "." for help.
<!-- ======================================================================== ->
---
<!--
   - [ ] manpage
   - [ ] demo program
   - [ ] unit test
-->
