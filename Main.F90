Program Main

    use grid
    Use input
    use fields
    implicit none
    call get_input()
    call initialize_grid()
    call initialize_fields()
    call initialize_output() ! This opens files
    call evolve_fields()    
    call finalize_output() ! This closes files that are open
end Program Main

