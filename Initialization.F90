Module initialization
    use evolve    
    use grid
    use fields
    implicit none
    integer :: density_init_type = 2
    real*8 :: wave_num
    real*8 :: vxamp = 1.0d0
contains
    subroutine initialize_velocity()
        
        allocate (vx(1:nx))
            vx = vxamp
    end subroutine initialize_velocity
    subroutine initialize_fields()
        call initialize_density()
        !call initialize_temperature()
        call initialize_velocity()       
    end subroutine initialize_fields
    subroutine initialize_density()
        integer :: i
        allocate(density(1:nx))        
        select case (density_init_type)
            case (1)
            Do i= 1, nx
                density(i) = sin(wave_num*x(i))
            endDo
            case (2)
            Do i=1,nx
                density(i) = exp(-(x(i)-3.14)**2)
            endDo            
            case default
        end select
    end subroutine initialize_density
End Module initialization
