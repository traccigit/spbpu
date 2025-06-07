module module_process
   use Environment
   use module_IO
   implicit none

contains

   subroutine InsertionSort(owners)
      type(owners_array), intent(inout) :: owners
      integer :: i, j

      do i = 2, size(owners%Phone)
         j = i - 1
         do while (j >= 1 .and. owners%Phone(j) < owners%Phone(i))
            j = j - 1
         end do
         owners%Phone(j+1:i)   = cshift(owners%Phone(j+1:i),   -1)
         owners%Surname(j+1:i) = cshift(owners%Surname(j+1:i), -1)
      end do
   end subroutine InsertionSort

end module module_process
