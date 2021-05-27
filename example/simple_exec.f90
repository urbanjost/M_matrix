    program demo_laff
    use M_matrix, only : laff, put_into_laff, get_from_laff, ifin_laff

    call laff("A=10;who")
    ! read a file containing laff(3f) commands
    call laff("exec('mycommands');")
    call laff("exec('mycommands2');")
    ! interactively interact with laff(3f) interpreter
    call laff() 

    end program demo_laff
