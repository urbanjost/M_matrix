             program demo_MAT88
             use M_matrix, only : mat88

                write(*,'(a)')'optionally initialize scratch area size'
                call MAT88(20000)

                write(*,'(a)')'do some commands'
                call MAT88([character(len=256) :: &
                & 'semi;                         ',&
                & 'semi;                         ',&
                & 'a=magic(4),b=-a               ',&
                & 'a+b;a;b                       ',&
                & 'display("That is all Folks")  '])

                write(*,'(a)')'do a single command'
                call MAT88('who')

                write(*,'(a)')'enter interactive mode'
                call MAT88()

                write(*,'(a)')'ending program'
       end program demo_MAT88
