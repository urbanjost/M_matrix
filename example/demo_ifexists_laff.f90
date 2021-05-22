            program demo_ifexists_laff
            use M_matrix, only : ifexists_laff
            implicit none
               write(*,*)'eps ',ifexists_laff('eps')
               write(*,*)'unknown ',ifexists_laff('unknown')
            end program demo_ifexists_laff
