Module grid
Implicit none

! Added y variables, 3/23/2015
integer :: nx, ny 
Real*8, allocatable :: x(:), y(:) 
Real*8 :: xmin, xmax, ymin, ymax 
contains 
    subroutine initialize_grid()
        allocate(x(1:nx),y(1:ny))
        call gen_grid_periodic(xmin,xmax,x)
        call gen_grid_periodic(ymin,ymax,y)
    end subroutine initialize_grid
subroutine gen_grid_periodic(amin,amax,a)
    Real*8 :: amin, amax, a(:), da, na
    integer :: istart, iend, i
    istart = LBOUND(a,1)
    iend = UBOUND(a,1)
    na = iend-istart+1
    da = (amax-amin)/(na)
    a(istart) = amin
    do i=istart+1, iend
        a(i) = a(i-1) + da
    enddo
end subroutine gen_grid_periodic
subroutine gen_grid(amin,amax,a)
    Real*8 :: amin, amax, a(:), da, na
    integer :: istart, iend, i
    istart = LBOUND(a,1)
    iend = UBOUND(a,1)
    na = iend-istart+1
    da = (amax-amin)/(na-1)
    a(istart) = amin
    do i=istart+1, iend
        a(i) = a(i-1) + da
    enddo
end subroutine gen_grid
end Module grid
