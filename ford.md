---
src_dir: src
         app
         example
         test
output_dir: docs/fpm-ford
project: M_matrix
summary: embeddable command-line matrix utility
project_github: https://github.com/urbanjost/M_matrix
project_download: https://github.com/urbanjost/M_matrix/archive/refs/heads/master.zip
author: John S. Urban
author_email: urbanjost@comcast.net
github: https://github.com/urbanjost/M_matrix
media_dir: ./docs/images
exclude_dir: ./archive
             ./arrpointer
             ./bugs
             ./build
             ./demos
             ./docs
             ./FODDER
             ./man
             ./src/source
             ./testit
             ./tmp
display: public
         protected
source: true
proc_internals: true
sort: permission-alpha
favicon: docs/images/favicon.ico
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---
<!--
author_pic:
twitter:
website:
-->
{!README.md!}
---
