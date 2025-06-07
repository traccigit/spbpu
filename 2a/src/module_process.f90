module module_process
   use Environment
   implicit none

contains

!i, j
   subroutine InsertionSort(surnames, phones)
      character(kind=CH_), intent(inout) :: surnames(:, :)
      integer(I_), intent(inout)         :: phones(:)
      integer                            :: i, j, n

      n = size(phones)
      do i = 2, n
         j = i - 1
         do while (j >= 1 .and. Phones(j) < Phones(i))
            j = j - 1
         end do
         surnames(j+1:i, :) = cshift(surnames(j+1:i, :), -1, 1)
         phones(j+1:i) = cshift(phones(j+1:i), -1)
      end do
   end subroutine InsertionSort

end module module_process
