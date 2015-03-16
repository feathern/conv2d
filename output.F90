module output
use grid
use fields
implicit none
integer :: density_unit=20, x_unit=21, output_interval 

 character*120 :: xfile = "xgrid", rhofile = "density"

contains
    subroutine write_output()
        call write_array(density,density_unit)
    end subroutine write_output
    subroutine finalize_output()
        call closefile(density_unit)
    end subroutine finalize_output
    subroutine initialize_output()
        call openfile(x_unit,xfile)
        call openfile(density_unit,rhofile)
        call write_array(x,x_unit)
        call closefile(x_unit)
    end subroutine initialize_output
    subroutine write_msg(a)         
        real*8 :: a(:)        
        write(6,*) a
    end subroutine write_msg
    subroutine write_array(array,funit)
        integer :: i, imin, imax, nrec, new_pos
        integer, intent(in) :: funit
        Real*8, intent(in) :: array(:)
        imin = LBOUND(array,1)
        imax = UBOUND(array,1)
        read(funit,pos=1)nrec
        new_pos = 1 +nx*nrec*8+8
        write(funit,pos = new_pos)(array(i),i=imin,imax)
        nrec = nrec + 1
        write(funit,pos=1) nrec
    end subroutine write_array
    subroutine closefile(funit)
        integer, intent(in) :: funit
        close(funit)
    end subroutine closefile
    subroutine openfile(funit,file_name)
        character*120 :: file_name
        integer, intent(in) :: funit
        integer :: nrec = 0
        OPEN(UNIT=funit, FORM="unformatted", FILE=file_name,&
        & access="stream",status="replace" )
        write(funit) nrec
        write(funit) nx
    end subroutine openfile

end module output

