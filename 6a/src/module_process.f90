! module module_process
!    use Environment
!    use module_IO
!    implicit none
!
! contains
!
!    subroutine InsertionSort(OwnersList)
!       type(owner_node), pointer, intent(inout) :: OwnersList
!       type(owner_node), pointer  :: sorted, current, next_node
!
!       if (.not. associated(OwnersList)) return
!
!       sorted => null()
!       current => OwnersList
!
!       do while (associated(current))
!          next_node => current%next
!          call Insert_into_sorted(current, sorted)
!          current => next_node
!       end do
!
!       OwnersList => sorted
!    end subroutine InsertionSort
!
!    subroutine Insert_into_sorted(new_node, sorted)
!       type(owner_node), pointer, intent(in)    :: new_node
!       type(owner_node), pointer, intent(inout) :: sorted
!       type(owner_node), pointer  :: current
!
!       if (.not. associated(sorted) .or. new_node%Phone >= sorted%Phone) then
!          new_node%next => sorted
!          sorted => new_node
!       else
!          current => sorted
!          do while (associated(current%next) .and. new_node%Phone < current%next%Phone)
!             current => current%next
!          end do
!          new_node%next => current%next
!          current%next => new_node
!       end if
!    end subroutine Insert_into_sorted
module module_process
   use Environment
   use module_IO
   implicit none
contains

   ! Сортировка вставками по убыванию телефонов
   recursive subroutine InsertionSort(List, lastSorted)
      type(owner_node), pointer, intent(inout) :: List
      type(owner_node), pointer, intent(inout) :: lastSorted
      type(owner_node), pointer :: tmp

      if (associated(lastSorted%next)) then
         if (lastSorted%next%Phone > lastSorted%Phone) then
            tmp => lastSorted%next
            lastSorted%next => lastSorted%next%next
            tmp%next => null()

            call Paste(List, tmp)
            call InsertionSort(List, lastSorted)
         else
            call InsertionSort(List, lastSorted%next)
         end if
      end if
   end subroutine InsertionSort

   ! Вставка элемента в отсортированный список
   recursive subroutine Paste(current, itemToInsert)
      type(owner_node), pointer, intent(inout) :: current
      type(owner_node), pointer, intent(inout) :: itemToInsert

     ! if (.not. associated(current)) then
      !   current => itemToInsert
      if (itemToInsert%Phone > current%Phone) then
         itemToInsert%next => current
         current => itemToInsert
      else
         call Paste(current%next, itemToInsert)
      end if
   end subroutine Paste

end module module_process

