program main

    use namelist, only : read_nml
    use change_unit, only : unit_conv

    implicit none

    real(4) :: begin
    real(4) :: over

    call cpu_time(begin)

    call read_nml()

    call unit_conv()

    call cpu_time(over)

    write(*,'(a,i0,a)') 'ELUPS : ', over-begin, 's'

end program main

