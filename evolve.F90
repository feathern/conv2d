Module evolve
    use output
    use fields
    use grid
    use derivatives
implicit none
real*8 :: dt=0.001, elapsed_time, max_time = 1.0d1, sigma = 1.0d0
real*8, allocatable :: drhodt(:,:), drhodt2(:,:), temp(:,:),temp2(:,:),temp3(:,:)
real*8, allocatable :: piece1(:,:), piece2(:,:), piece3(:,:), piece4(:,:)
integer :: iteration, max_iteration = 20000
contains
    subroutine evolve_fields()
        logical :: keepgoing
        elapsed_time = 0.0d0       
        iteration = 0
        keepgoing = .True.
        allocate(drhodt(1:nx,1:ny),temp(1:nx,1:ny),temp2(1:nx,1:ny),temp3(1:nx,1:ny))
        allocate(piece1(1:nx,1:ny),piece2(1:nx,1:ny),piece3(1:nx,1:ny),piece4(1:nx,1:ny))
        drhodt = 0.0d0
        temp = 0.0d0
        do while (keepgoing)
            call get_drhodt()
            call update_rho()
            elapsed_time = elapsed_time+dt
            iteration = iteration+1
            if (iteration .eq. max_iteration) then
                keepgoing = .False.
            endif
            write(6,*) iteration, elapsed_time
            if (elapsed_time .ge. max_time) then ! ge = >=
                keepgoing = .False.
            endif              
            if (mod(iteration,output_interval).eq.0) then ! Python translation: if(iter%out_nt == 0)
                call write_output()
            endif        
        end do
    end subroutine evolve_fields
    subroutine update_rho()
        integer :: i, j
        do j = 1, ny
            do i=1, nx ! looping over x
                density(i,j) = density(i,j)+drhodt(i,j)*dt
            end do
        end do
    end subroutine update_rho
    subroutine get_drhodt()
        integer :: i, j        
        call d_by_dx(x,density,temp) ! temp = drho/dx
        call d_by_dx(x,temp,piece1) ! piece = (drho/dx)^2
        
        call d_by_dy(y,density,temp2) ! temp2 = drho/dy
        call d_by_dy(y,temp2,piece2) ! piece2 = (drho/dy)^2

        do j = 1, ny
            do i=1, nx
                temp(i,j) = vx(i,j)*density(i,j)
                temp2(i,j) = vy(i,j)*density(i,j)
            end do
        end do
        call d_by_dx(x,temp,piece3) ! piece3 = d(rho*vx)/dx
        call d_by_dy(y,temp2,piece4) ! piece4 = d(rho*vy)/dy   
        do j = 1, ny        ! made density, temp, and drhodt 2D: 3/23/15
            do i=1, nx        
                drhodt(i,j) = sigma*(piece1(i,j)+piece2(i,j))-piece3(i,j)-piece4(i,j) !sigma*drhodt(i,j)+sigma*drhodt2(i,j)-temp(i,j)*vx(i,j)  
            end do  
        end do             
   end subroutine get_drhodt
End Module evolve
