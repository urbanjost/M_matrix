#!/bin/bash
skip(){
rm `which mold`
rm `which M_matrix`
gfortran mold.f90 -fno-range-check -O3 -o mold
mv mold $HOME/bin/`sr`/
quiet ccall M_matrix.FF|tee x
}
skip
xterm -e mold &
xterm -e M_matrix &
