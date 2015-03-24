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
        allocate (vx(1:nx,1:ny))
            vx = vxamp
    end subroutine initialize_velocity
    subroutine initialize_fields()
        call initialize_density()
        !call initialize_temperature()
        call initialize_velocity()       
    end subroutine initialize_fields
    subroutine initialize_density()
        integer :: i
        allocate(density(1:nx,1:nx))        
        select case (density_init_type)
            case (1)
            Do j = 1, nx    ! added a second dimension 3/23/15
                Do i = 1, nx
                    density(i,j) = sin(wave_num*x(i))
                endDo
            endDo
            case (2)
            Do j=1, nx      ! added a secpnd dimension 3/23/15
                Do i=1,nx
                    density(i,j) = exp(-(x(i)-3.14)**2)
                endDo
            endDo            
            case default
        end select
    end subroutine initialize_density
End Module initialization
