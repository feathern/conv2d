module derivatives
implicit none
!variables
    real*8 :: var = 4    
contains
    !This is where we put sub routines
    subroutine initx()
        var = 5
    end subroutine initx    
    subroutine d_by_dx(x,fct,dfct)  ! Added over_two_dx, dy, and y. Changed y-->fct and dy-->dfct, and made fct and dfct 2D 3/23/15 
        real*8 :: x(:), fct(:,:), dfct(:,:), dx, over_two_dx !Expect to recieve 1D array, to do a 2D array: x(:,:)        
        integer :: istart, iend, i, j, jstart, jend
        istart = LBOUND(x,1)
        iend = UBOUND(x,1) 
        jstart = LBOUND(fct,2)
        jend = UBOUND(fct,2)
        dx = x(2)-x(1)
        over_two_dx = 1.0d0/(2*dx)
        Do j = jstart, jend        ! iterating over two dimensions 3/23/15
            Do i= istart+1, iend-1
                dfct(i,j) = (fct(i+1,j)-fct(i-1,j))*over_two_dx     
            endDo
            dfct(istart,j) = (fct(istart+1,j)-fct(iend,j))*over_two_dx ! New
            dfct(iend, j) = (fct(istart,j)-fct(iend-1,j))*over_two_dx  ! New
        endDo        
    end subroutine d_by_dx
    subroutine d_by_dy(y,fct,dfct)  ! Added over_two_dx, dy, and y. Changed y-->fct and dy-->dfct, and made fct and dfct 2D 3/23/15 
        real*8 :: y(:), fct(:,:), dfct(:,:), dy, over_two_dy !Expect to recieve 1D array, to do a 2D array: x(:,:)        
        integer :: istart, iend, i, j, jstart, jend
        istart = LBOUND(fct,1)
        iend = UBOUND(fct,1) 
        jstart = LBOUND(fct,2)
        jend = UBOUND(fct,2)
        dy = y(2)-y(1)
        over_two_dy = 1.0d0/(2*dy)
        Do j = jstart+1, jend-1        ! iterating over two dimensions 3/23/15
            Do i= istart, iend
                dfct(i,j) = (fct(i,j+1)-fct(i,j-1))*over_two_dy     
            endDo
        end do
        Do i = istart, iend
            dfct(i,jstart) = (fct(i,jstart+1)-fct(i,jend))*over_two_dy ! New
            dfct(i, jend) = (fct(i,jstart)-fct(i,jend-1))*over_two_dy  ! New
        endDo        
    end subroutine d_by_dy
end module derivatives
