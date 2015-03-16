Module input
    use output
    use grid
    use Initialization
    Namelist /input_namelist/ output_interval, dt, density_init_type, max_time, wave_num, nx, max_iteration
contains
    subroutine get_input()
        character*120 :: input_file
        input_file = './main_input'
        !open(unit=19, file=input_file, status='old', position='rewind')
        !read(unit=19,nml=input_namelist)
        !close(19)
        output_interval = 100  !How often (in iterations) we output      
        dt = .001  ! time step
        density_init_type = 2
        max_time = 10.0  ! Time we evolve until
        max_iteration = 20000 
        wave_num = 2.0  !k for the density init
        nx = 128
        vxamp = 0.0d0
        xmin = 0.0
        xmax = 2*acos(-1.0d0)
    end subroutine get_input 
End Module input
