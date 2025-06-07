module module_process
   use Environment
   use module_IO
   implicit none

contains
   subroutine InsertionSort(owners)
      type(owner), intent(inout) :: owners(:)
      integer :: i, j

      do i = 2, size(owners)
         j = i - 1
         do while (j >= 1 .and. owners(j)%Phone < owners(i)%Phone)
            j = j - 1
         end do
         owners(j+1:i)%Phone   = cshift(owners(j+1:i)%Phone,   -1)
         owners(j+1:i)%Surname = cshift(owners(j+1:i)%Surname, -1)
      end do
   end subroutine InsertionSort

end module module_process
