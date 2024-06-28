module namelist
    
    use globals, only : nx, ny, nz, nt, ifile, ofile

    implicit none

    private
    public :: read_nml

    contains


    subroutine read_nml()
        integer, parameter :: nml_unit = 5

        namelist / grid / nx, ny, nz
        namelist / record / nt
        namelist / files / ifile, ofile

        nx = 0
        ny = 0
        nz = 0

        nt = 0

        ifile = ''
        ofile = ''

        read(nml_unit,nml=grid)
        read(nml_unit,nml=record)
        read(nml_unit,nml=files)

        call checker()

    end subroutine read_nml


    subroutine checker()

        if (nx <= 0) then
            write(*,'(a)') 'ValueError : Invalid nx value'
            error stop
        endif

        if (ny <= 0) then
            write(*,'(a)') 'ValueError : Invalid ny value'
            error stop
        endif

        if (nz <= 0) then
            write(*,'(a)') 'ValueError : Invalid nz value'
            error stop
        endif

        if (nt <= 0) then
            write(*,'(a)') 'ValueError : Invalid nt value'
            error stop
        endif

        if (ifile == '') then
            write(*,'(a)') 'ifile is not specified'
            error stop
        endif

        if (ifile == '') then
            write(*,'(a)') 'ifile is not specified'
            error stop
        endif

    end subroutine checker


end module namelist

