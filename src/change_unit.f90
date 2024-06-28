module change_unit

    use globals, only : kp, nx, ny, nz, nt, ifile, ofile
    use fileio , only : finfo, fopen, fclose, fread, fwrite

    implicit none

    private
    public :: unit_conv

    contains


    subroutine unit_conv()
        type(finfo) :: itype
        type(finfo) :: otype
        real(kp) :: array(nx,ny,nz)

        integer :: t

        call fopen(itype           , &  !! OUT
                 & fname=ifile     , &  !! IN
                 & action='read'   , &  !! IN
                 & recl=kp*nx*ny*nz, &  !! IN
                 & record=1        , &  !! IN
                 & recstep=1         )  !! IN

        call fopen(otype           , &  !! OUT
                 & fname=ifile     , &  !! IN
                 & action='write'  , &  !! IN
                 & recl=kp*nx*ny*nz, &  !! IN
                 & record=1        , &  !! IN
                 & recstep=1         )  !! IN

        do t = 1, nt
            call fread(itype              , &  !! INOUT
                     & array(1:nx,1:ny,1:nz))  !! OUT

            call perDay2perSec(array(1:nz,1:ny,1:nz))  !! INOUT

            call fwrite(otype              , &  !! INOUT
                      & array(1:nx,1:ny,1:nz))  !! IN
        enddo

        call fclose(itype)  !! INOUT
        call fclose(otype)  !! INOUT

    end subroutine unit_conv


    subroutine perDay2perSec(array)
        real(kp), intent(inout) :: array(nx,ny,nz)

        real(kp), parameter :: sec_per_day = 60._kp * 60._kp * 24._kp

        array(1:nx,1:ny,1:nz) = array(1:nx,1:ny,1:nz) * sec_per_day

    end subroutine perDay2perSec



end module change_unit

