    program demo_lala
    use M_matrix, only : lala, put_into_lala, get_from_lala, ifin_lala

    call lala("A=10;who")
    ! read a file containing lala(3f) commands
    call lala("exec('mycommands');")
    call lala("exec('mycommands2');")
    ! interactively interact with lala(3f) interpreter
    call lala() 

    end program demo_lala
