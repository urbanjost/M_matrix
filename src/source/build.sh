#!/bin/bash
set -x
cd $(dirname $0)
####################################
rm -rfv ../source/doc
mkdir -p ../source/doc/PRIVATE
mkdir -p ../../man/man3
mkdir -p ../../man/man1
####################################
(
#export UFPP_DOCUMENT_DIR=$(pwd)
export PREP_DOCUMENT_DIR=$(pwd)
for NAME in lala
do
   #ufpp F90 --cstyle doxygen --verbose --allow_links -i ../../app/source/$NAME.[fF][fF] -o ../../app/$NAME.f90
   prep F90 --comment doxygen --verbose  -i ../../app/source/$NAME.[fF][fF] -o ../../app/$NAME.f90
done
)
####################################
export GITHUB=TRUE
export DEMO_OUTDIR=../../example/
export DEMO_SUBDIR=FALSE
GPF_build_module M_matrix
####################################
cp ../../docs/man3.html ../../docs/index.html
cp ../../docs/BOOK_M_matrix.html ../../docs/index.html
ccall ../../test/test_suite_M_matrix.[fF]90
####################################
exit
####################################
