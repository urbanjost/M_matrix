            program demo_ifexists_mat88
            use M_matrix, only : ifexists_mat88
            implicit none
               write(*,*)'eps ',ifexists_mat88('eps')
               write(*,*)'unknown ',ifexists_mat88('unknown')
            end program demo_ifexists_mat88
