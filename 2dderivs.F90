Program Main
    Integer :: nx, ny, i, j
    Real*8, Allocatable :: rho(:,:), drho(:,:)  ! < --- rho is 2-D array. For 3-D do this rho(:,:,:)
    Real*8 :: xmin, xmax, ymin, ymax, dx, dy
    Real*8, Allocatable :: x(:), y(:)  ! < ----- x and y are 1-D
    Real*8 :: over_two_dy, over_two_dx

    !//////////////////////////////
    ! Initialize the grid
    nx = 100
    ny = 100

    xmin = 0.0 
    xmax = 1.0
    ymin = 1.2
    ymax = 2.2
    Allocate(x(1:nx))
    Allocate(y(1:ny))
    dx = (xmax-xmin)/dble(nx-1)
    dy = (ymax-ymin)/dble(ny-1)
    over_two_dy = 1.0d0/(2*dy)
    over_two_dx = 1.0d0/(2*dx)

    x(1) = xmin
    y(1) = ymin
    Do i = 2, nx
        x(i) = x(i-1)+dx
    Enddo

    Do j = 2, ny
        y(j) = y(j-1)+dy
    Enddo


    !//////////////////////////////////////
    ! Initialize rho(x,y)

    Allocate(rho(1:nx,1:ny),drho(1:nx,1:ny))

    ! Initialize density to something -- how about rho(x,y) = x/10.0 + y^2
    Do j = 1, ny
        Do i = 1, nx
            rho(i,j) = x(i)/10.0 + y(j)**2
        Enddo
    Enddo


    ! Now take a derivative.  Note that I'm not doing anything with the boundaries here.  
    ! You will want to mimic the logic already in your code to handle the periodicity correcly.  
    ! Also note that the rho I'm using isn't periodic.  I'm just giving you the flavor of things here


    ! Compute d rho / dy
    Do j = 2, ny -1
        Do i = 1, nx
            
            drho(i,j) = (rho(i,j+1)-rho(i,j-1)) * over_two_dy
        Enddo
    Enddo


    ! Or.. Compute d rho / dx  (this would overwrite d rho /dy from above)
    Do j = 1, ny 
        Do i = 2, nx-1
 
            drho(i,j) = (rho(i,j+1)-rho(i,j-1)) * over_two_dx
        Enddo
    Enddo

End Program Main
