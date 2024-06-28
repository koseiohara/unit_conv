module fileio

    use NaNchecker, only : isNaN

    implicit none

    private
    public :: finfo, fopen, fclose, fread, fwrite, reset_record, get_record


    type finfo
        integer :: unit             ! Unit number of the opened file
        character(128) :: fname     ! File name
        character(16)  :: action    ! Action to the file (read/write/readwrite)
        integer :: recl             ! Record length of each record
        integer :: record           ! Reord to access
        integer :: recstep          ! Number added to record after eache step of read or write
    end type finfo


    interface fread
        module procedure &
            freadscalar_s, &
            freadscalar_d, &
            fread1d_s    , &
            fread1d_d    , &
            fread2d_s    , &
            fread2d_d    , &
            fread3d_s    , &
            fread3d_d
    end interface fread

    interface fwrite
        module procedure &
            fwritescalar_s, &
            fwritescalar_d, &
            fwrite1d_s    , &
            fwrite1d_d    , &
            fwrite2d_s    , &
            fwrite2d_d    , &
            fwrite3d_s    , &
            fwrite3d_d
    end interface fwrite

    
    contains
    
    
    subroutine fopen(ftype, unit, fname, action, recl, record, recstep)
        type(finfo), intent(out) :: ftype

        integer     , intent(in), optional :: unit
        character(*), intent(in)           :: fname
        character(*), intent(in)           :: action
        integer     , intent(in)           :: recl
        integer     , intent(in)           :: record
        integer     , intent(in)           :: recstep

        integer :: stat


        ftype%fname   = trim(fname)
        ftype%action  = trim(action)
        ftype%recl    = recl
        ftype%record  = record
        ftype%recstep = recstep


        if (recl <= 0) then
            write(*,*)
            write(*,'(a)')    'ValueError ----------------------------------------------------'
            write(*,'(a)')    '|   Invalid argument : Value of recl must be more than 0'
            write(*,'(a)')    '|'
            write(*,'(a)')    '|   FileName : ' // trim(ftype%fname)
            write(*,'(a)')    '|   Action   : ' // trim(ftype%action)
            write(*,'(a,i0)') '|   Recl     : ', ftype%recl
            write(*,'(a,i0)') '|   Record   : ', ftype%record
            write(*,'(a,i0)') '|   RecStep  : ', ftype%recstep
            write(*,'(a)')    '---------------------------------------------------------------'
            ERROR STOP
        endif

        if (record <= 0) then
            write(*,*)
            write(*,'(a)')    'ValueError ----------------------------------------------------'
            write(*,'(a)')    '|   Invalid argument : Value of record must be more than 0'
            write(*,'(a)')    '|'
            write(*,'(a)')    '|   FileName : ' // trim(ftype%fname)
            write(*,'(a)')    '|   Action   : ' // trim(ftype%action)
            write(*,'(a,i0)') '|   Recl     : ', ftype%recl
            write(*,'(a,i0)') '|   Record   : ', ftype%record
            write(*,'(a,i0)') '|   RecStep  : ', ftype%recstep
            write(*,'(a)')    '---------------------------------------------------------------'
            ERROR STOP
        endif

        if (recstep < 0) then
            write(*,*)
            write(*,'(a)')    'ValueError ----------------------------------------------------'
            write(*,'(a)')    '|   Invalid argument : Value of recstep must be equal or more than 0'
            write(*,'(a)')    '|'
            write(*,'(a)')    '|   FileName : ' // trim(ftype%fname)
            write(*,'(a)')    '|   Action   : ' // trim(ftype%action)
            write(*,'(a,i0)') '|   Recl     : ', ftype%recl
            write(*,'(a,i0)') '|   Record   : ', ftype%record
            write(*,'(a,i0)') '|   RecStep  : ', ftype%recstep
            write(*,'(a)')    '---------------------------------------------------------------'
            ERROR STOP
        endif


        if (.NOT. present(unit)) then

            open(NEWUNIT=ftype%unit   , &
               &    FILE=ftype%fname  , &
               &  ACTION=ftype%action , &
               &    FORM='UNFORMATTED', &
               &  ACCESS='DIRECT'     , &
               &    RECL=ftype%recl   , &
               &  IOSTAT=stat           )

        else

            if (unit == 5 .or. unit == 6) then
                write(*,*)
                write(*,'(a)')    'ValueError ----------------------------------------------------'
                write(*,'(a)')    '|   Invalid argument : 5 and 6 are unavailable for unit number'
                write(*,'(a)')    '|'
                write(*,'(a,i0)') '|   Unit     : ', unit
                write(*,'(a)')    '|   FileName : ' // trim(ftype%fname)
                write(*,'(a)')    '|   Action   : ' // trim(ftype%action)
                write(*,'(a,i0)') '|   Recl     : ', ftype%recl
                write(*,'(a,i0)') '|   Record   : ', ftype%record
                write(*,'(a,i0)') '|   RecStep  : ', ftype%recstep
                write(*,'(a)')    '---------------------------------------------------------------'
                ERROR STOP
            endif

            ftype%unit = unit
            open(  UNIT=ftype%unit   , &
               &   FILE=ftype%fname  , &
               & ACTION=ftype%action , &
               &   FORM='UNFORMATTED', &
               & ACCESS='DIRECT'     , &
               &   RECL=ftype%recl   , &
               & IOSTAT=stat           )

        endif

        if (stat/=0) then
            write(*,*)
            write(*,'(a)')    'OpenFileError -------------------------------------------------'
            write(*,'(a)')    '|   Failed to open file'
            write(*,'(a)')    '|'
            write(*,'(a,i0)') '|   Unit     : ', unit
            write(*,'(a)')    '|   FileName : ' // trim(fname)
            write(*,'(a)')    '|   Action   : ' // trim(action)
            write(*,'(a,i0)') '|   Recl     : ', recl
            write(*,'(a,i0)') '|   Record   : ', record
            write(*,'(a,i0)') '|   RecStep  : ', recstep
            write(*,'(a)')    '---------------------------------------------------------------'

            ERROR STOP
        endif

    end subroutine fopen


    subroutine fclose(ftype)
        type(finfo), intent(inout) :: ftype

        call check_opened(ftype)

        CLOSE(ftype%unit)
        
        ftype%unit    = 0
        ftype%fname   = 'ERROR'
        ftype%action  = 'ERROR'
        ftype%recl    = 0
        ftype%record  = 0
        ftype%recstep = 0

    end subroutine fclose


    subroutine freadscalar_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: carrier

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine freadscalar_s


    subroutine freadscalar_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: carrier

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine freadscalar_d


    subroutine fread1d_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: carrier(:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier(:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fread1d_s


    subroutine fread1d_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: carrier(:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier(:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fread1d_d


    subroutine fread2d_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: carrier(:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier(:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fread2d_s


    subroutine fread2d_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: carrier(:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier(:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fread2d_d


    subroutine fread3d_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: carrier(:,:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier(:,:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fread3d_s


    subroutine fread3d_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: carrier(:,:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) carrier(:,:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fread3d_d


    subroutine fwritescalar_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: carrier

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwritescalar_s


    subroutine fwritescalar_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: carrier

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwritescalar_d


    subroutine fwrite1d_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: carrier(:)

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier(:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwrite1d_s


    subroutine fwrite1d_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: carrier(:)

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier(:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwrite1d_d


    subroutine fwrite2d_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: carrier(:,:)

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier(:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwrite2d_s


    subroutine fwrite2d_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: carrier(:,:)

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier(:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwrite2d_d


    subroutine fwrite3d_s(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: carrier(:,:,:)

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier(:,:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwrite3d_s


    subroutine fwrite3d_d(ftype, carrier)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: carrier(:,:,:)

        call check_opened(ftype)

        write(ftype%unit,rec=ftype%record) carrier(:,:,:)
        ftype%record = ftype%record + ftype%recstep

        if (isNaN(carrier)) then
            call NaNWarn(ftype)
        endif

    end subroutine fwrite3d_d


    subroutine reset_record(ftype, newrecord, recstep)
        type(finfo), intent(inout) :: ftype
        integer    , intent(in), optional :: newrecord
        integer    , intent(in), optional :: recstep

        if (present(newrecord)) then
            ftype%record = newrecord
            return
        else if (present(recstep)) then
            ftype%record = ftype%record + recstep
            return
        else
            write(*,'(a)')    'ArgumentError -------------------------------------------------'
            write(*,'(a)')    '|   newrecord or recstep is needed for reset_record'
            write(*,'(a)')    '---------------------------------------------------------------'
            ERROR STOP
        endif

    end subroutine reset_record


    function get_record(ftype) result(output)
        type(finfo), intent(in) :: ftype
        integer :: output

        output = ftype%record

    end function get_record


    subroutine check_opened(ftype)
        type(finfo), intent(in) :: ftype

        logical :: opened

        INQUIRE(UNIT=ftype%unit, &
              & OPENED=opened    )

        if (.NOT. opened) then
            write(*,*)
            write(*,'(a)')    'IO Error ------------------------------------------------------'
            write(*,'(a)')    '|   File is not opened'
            write(*,'(a)')    '|'
            write(*,'(a,i0)') '|   Unit     : ', ftype%unit
            write(*,'(a)')    '|   FileName : ' // trim(ftype%fname)
            write(*,'(a)')    '|   Action   : ' // trim(ftype%action)
            write(*,'(a,i0)') '|   Recl     : ', ftype%recl
            write(*,'(a,i0)') '|   Record   : ', ftype%record
            write(*,'(a,i0)') '|   RecStep  : ', ftype%recstep
            write(*,'(a)')    '---------------------------------------------------------------'

            ERROR STOP
        endif

    end subroutine check_opened


    subroutine NaNWarn(ftype)
        type(finfo), intent(in) :: ftype

        write(*,*)
        write(*,'(a)')    'Warning : NaN is Found ----------------------------------------'
        write(*,'(a,i0)') '|   Unit     : ', ftype%unit
        write(*,'(a)')    '|   FileName : ' // trim(ftype%fname)
        write(*,'(a)')    '|   Action   : ' // trim(ftype%action)
        write(*,'(a,i0)') '|   Recl     : ', ftype%recl
        write(*,'(a,i0)') '|   Record   : ', ftype%record
        write(*,'(a,i0)') '|   RecStep  : ', ftype%recstep
        write(*,'(a)')    '---------------------------------------------------------------'

    end subroutine NaNWarn


end module fileio

