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
        real*8 :: x(:), y(:) fct(:,:), dfct(:,:), dx, dy, over_two_dx !Expect to recieve 1D array, to do a 2D array: x(:,:)        
        integer :: istart, iend, i, j
        istart = LBOUND(x,1)
        iend = UBOUND(x,1) 
        dx = x(2)-x(1)
        over_two_dx = 1.0d0/(2*dx)
        Do j = istart, iend        ! iterating over two dimensions 3/23/15
            Do i= istart+1, iend-1
                dfct(i,j) = (fct(i,j+1)-fct(i,j-1))*over_two_dx   
                dfct(istart,j) = (fct(istart,j+1)-fct(istart,j-1))*over_two_dx ! New
                dfct(iend, j) = (fct(istart,j+1)-fct(istart,j-1)*over_two_dx  ! New  
            endDo
        endDo        
        dfct(istart,istart) = (fct(istart,istart+1)-fct(istart,iend))*over_two_dx 
        dfct(iend,iend) = (fct(istart,istart)-fct(istart,iend-1))*over_two_dx 
    end subroutine d_by_dx

end module derivatives
