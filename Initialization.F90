Module initialization
    use evolve    
    use grid
    use fields
    implicit none
    integer :: density_init_type = 2, velocity_init_type = 0
    real*8 :: wave_num, alpha = 1.0d0
    real*8 :: vxamp = 1.0d0, vyamp = 1.0d0
contains
    subroutine initialize_velocity()
        integer :: i, j
        allocate (vx(1:nx,1:ny),vy(1:nx,1:ny))
        select case (velocity_init_type)
            case (1)
            Do j = 1, ny    ! added a second dimension 3/23/15
                Do i = 1, nx
                    vx(i,j) = vxamp
                    vy(i,j) = 0
                endDo
            endDo
            case (2)
            Do j=1, ny      ! added a secpnd dimension 3/23/15
                Do i=1,nx
                    vx(i,j) = 0
                    vy(i,j) = vyamp
              endDo
            endDo            
            case (3)
            Do j=1, ny
                Do i=1, nx
                    vx(i,j) = vxamp*sin(x(i)/2)*cos(y(j)/2)
                    vy(i,j) = vyamp*(-sin(y(j)/2))*cos(x(i)/2)
                end do
            end do
            case default
                vx = 0 
                vy = 0
        end select
    end subroutine initialize_velocity
    subroutine initialize_fields()
        call initialize_density()
        !call initialize_temperature()
        call initialize_velocity()       
    end subroutine initialize_fields
    subroutine initialize_density()
        integer :: i, j
        real*8 :: yval, xval, x0, y0, pi, r2
        allocate(density(1:nx,1:ny))       
        pi = acos(-1.0d0) 
        select case (density_init_type)
            case (1)
            Do j = 1, ny    ! added a second dimension 3/23/15
                Do i = 1, nx
                    density(i,j) = sin(wave_num*x(i))
                endDo
            endDo
            case (2)
            Do j=1, ny      ! added a secpnd dimension 3/23/15
                Do i=1,nx
                    density(i,j) = exp(-(x(i)-3.14)**2)
                endDo
            endDo    
            case (3)
            x0 = pi
            y0 = pi
            Do j=1, ny
                yval = y(j)-y0
                Do i=1, nx
                    xval = x(i)-x0
                    r2 = xval**2+yval**2 
                    density(i,j) = exp(-alpha*r2)
                end do
            end do  
            case (4)
            Do j=1, ny
                Do i=1, nx
                    xval = x(i)-pi
                    density(i,j) = exp(-alpha*xval**2)
                end do
            end do     
            case (5)
            Do j=1, ny
                yval = y(j)-y0
                Do i=1, nx
                    density(i,j) = exp(-alpha*yval**2)
                end do
            end do
            case default
        end select
    end subroutine initialize_density
End Module initialization
