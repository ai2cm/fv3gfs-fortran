module statistics_mod

implicit none

interface mode
   module procedure mode_1d
   module procedure mode_2d
   module procedure masked_mode_2d
end interface mode

contains

  ! qksrt implementation copied and adapted for real arrays from implementation
  ! in FMS: FMS/drifters/quicksort.F90
  function qksrt_partition(n, list, start, end) result(top)
    implicit none
    integer, intent(in) :: n
    real, intent(inout) :: list(n)
    integer, intent(in) :: start, end

    real :: pivot
    integer :: bottom, top
    logical :: done

    pivot = list(end)                          ! Partition around the last value
    bottom = start-1                           ! Start outside the area to be partitioned
    top = end                                  ! Ditto

    done = .false.
    do while (.not. done)                      ! Until all elements are partitioned...

       do while (.not. done)                  ! Until we find an out of place element...
          bottom = bottom+1                  ! ... move the bottom up.

          if(bottom == top) then             ! If we hit the top...
             done = .true.                  ! ... we are done.
             exit
          endif

          if(list(bottom) > pivot) then           ! Is the bottom out of place?
             list(top) = list(bottom)       ! Then put it at the top...
              exit                          ! ... and start searching from the top.
           endif
        enddo

        do while (.not. done)                        ! Until we find an out of place element...
           top = top-1                        ! ... move the top down.

           if(top == bottom) then                  ! If we hit the bottom...
              done = .true.                      ! ... we are done.
              exit
           endif

           if(list(top) < pivot) then              ! Is the top out of place?
              list(bottom) = list(top)       ! Then put it at the bottom...
              exit                          ! ...and start searching from the bottom.
           endif
        enddo
    enddo

    list(top) = pivot                          ! Put the pivot in its place.
    ! Return the split point
  end function qksrt_partition

  recursive subroutine qksrt_quicksort(n, list, start, end)
    implicit none
    integer, intent(in) :: n
    real, intent(inout) :: list(n)
    integer, intent(in) :: start, end
    integer :: split

    if(start < end) then                            ! If there are two or more elements...
        split = qksrt_partition(n, list, start, end)    ! ... partition the sublist...
        call qksrt_quicksort(n, list,  start, split-1)        ! ... and sort both halves.
        call qksrt_quicksort(n, list, split+1, end)
    endif
  end subroutine qksrt_quicksort

  ! This procedure produces the same results as scipy.stats.mode; if there is a
  ! tie in counts, the minimum mode value is returned.
  function mode_1d(array)
    real, dimension(:), intent(in) :: array

    real :: mode_1d

    integer :: i, run, max_run
    real, dimension(size(array)) :: sorted_array

    run = 1
    max_run = 0

    sorted_array = array
    call qksrt_quicksort(size(sorted_array), sorted_array, 1, size(sorted_array))

    if (size(sorted_array) == 1) then
       mode_1d = sorted_array(1)
    else
       do i = 2, size(sorted_array)
          if (sorted_array(i) == sorted_array(i - 1)) then
             run = run + 1
          else
             run = 1
          endif
          if (run > max_run) then
             max_run = run
             mode_1d = sorted_array(i - 1)
          endif
       enddo
    endif
  end function mode_1d

  function mode_2d(array)
    real, dimension(:,:), intent(in) :: array

    real :: mode_2d

    mode_2d = mode_1d(pack(array, .true.))
  end function mode_2d

  function masked_mode_2d(array, mask)
    real, dimension(:,:), intent(in) :: array
    logical, dimension(:,:), intent(in) :: mask
    real :: masked_mode_2d

    masked_mode_2d = mode_1d(pack(array, mask))
  end function masked_mode_2d
end module statistics_mod