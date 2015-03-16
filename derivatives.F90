module derivatives
implicit none
!variables
    real*8 :: var = 4    
contains
    !This is where we put sub routines
    subroutine initx()
        var = 5
    end subroutine initx    
    subroutine d_by_dx(x,y,dy)
        real*8 :: x(:), y(:), dy(:), dx !Expect to recieve 1D array, to do a 2D array: x(:,:)
        integer :: istart, iend, i
        istart = LBOUND(x,1)
        iend = UBOUND(x,1) 
        dx = x(2)-x(1)
        Do i= istart+1, iend-1
           dy(i) = (y(i+1)-y(i-1))/(2*dx)       
        endDo
        dy(istart) = (y(istart+1)-y(iend))/(2*dx)
        dy(iend) = (y(istart)-y(iend-1))/(2*dx)
    end subroutine d_by_dx

end module derivatives
