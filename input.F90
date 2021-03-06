Module input
    use output
    use grid
    use Initialization
    Use Evolve
! Added ny 3/23/15
    Namelist /input_namelist/ dt,output_interval, density_init_type,max_time, max_iteration, nx, ny, sigma, wave_num, &
                vxamp, vyamp, velocity_init_type, alpha, expx0
contains
    subroutine get_input()
        character*120 :: input_file
        input_file = './main_input'
        open(unit=19, file=input_file, status='old', position='rewind')
        read(unit=19,nml=input_namelist)
        close(19)
        ymin = 0 ! New
        ymax = 2*acos(-1.0d0)  ! New
        xmin = 0.0
        xmax = 2*acos(-1.0d0)
    end subroutine get_input 
End Module input
