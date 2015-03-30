module output
use grid
use fields
implicit none
integer :: density_unit=20, x_unit=21, y_unit = 22, output_interval = 1000

 character*120 :: xfile = "xgrid", rhofile = "density", yfile ="ygrid"

contains
    subroutine write_output()
        call write_array2d(density,density_unit)
    end subroutine write_output

    subroutine finalize_output()
        call closefile(density_unit)
    end subroutine finalize_output

    subroutine initialize_output()

        call openfile(density_unit,rhofile,nx,dim2 = ny)  ! Open the density file -- keep it open throughout the run

        call openfile(x_unit,xfile,nx)  ! Write the x-grid
        call write_array1d(x,x_unit)
        call closefile(x_unit)

        call openfile(y_unit,yfile,ny) ! Write the y-grid
        call write_array1d(y,y_unit)
        call closefile(y_unit)
    

    end subroutine initialize_output

    subroutine write_msg(a)         
        real*8 :: a(:)        
        write(6,*) a
    end subroutine write_msg
    subroutine write_array1d(array,funit)
        integer :: i, imin, imax, nrec, new_pos, npts
        integer, intent(in) :: funit
        Real*8, intent(in) :: array(:)
        imin = LBOUND(array,1)
        imax = UBOUND(array,1)
        npts = imax-imin+1

        read(funit,pos=5)nrec
        new_pos = 1 +npts*nrec*8+12
        write(funit,pos = new_pos)(array(i),i=imin,imax)
        nrec = nrec + 1
        write(funit,pos=5) nrec

    end subroutine write_array1d

    subroutine write_array2d(array,funit)
        integer :: i, imin, imax, nrec, new_pos, j, jmin, jmax
        integer, intent(in) :: funit
        Real*8, intent(in) :: array(:,:)
        imin = LBOUND(array,1)
        imax = UBOUND(array,1)
        jmin = LBOUND(array,2)
        jmax = UBOUND(array,2)
        read(funit,pos=5)nrec
        new_pos = 1 +nx*ny*nrec*8+16
        write(funit,pos = new_pos)((array(i,j),i=imin,imax),j=jmin,jmax)
        nrec = nrec + 1
        write(funit,pos=5) nrec
    end subroutine write_array2d

    subroutine closefile(funit)
        integer, intent(in) :: funit
        close(funit)
    end subroutine closefile
    subroutine openfile(funit,file_name,dim1,dim2)
        character*120 :: file_name
        integer, intent(in) :: funit, dim1
        integer, intent(in), Optional :: dim2
        integer :: nrec = 0
        integer :: endian_tag = 314

        OPEN(UNIT=funit, FORM="unformatted", FILE=file_name,&
            & access="stream",status="replace" )
	    write(funit) endian_tag
        write(funit) nrec
        write(funit) dim1
        if (present(dim2)) Then
            write(funit) dim2
        endif
    end subroutine openfile

end module output

