module module_process
   use Environment
   use module_IO
   implicit none
contains

   pure recursive subroutine InsertionSort(List, lastSorted)
      type(owner_node), allocatable, intent(inout) :: List
      type(owner_node), allocatable, intent(inout) :: lastSorted
      type(owner_node), allocatable                :: tmp

      if (allocated(lastSorted%next)) then
         if (lastSorted%next%Phone > lastSorted%Phone) then
            call move_alloc(lastSorted%next, tmp)
            call move_alloc(tmp%next, lastSorted%next)
            call Paste(List, tmp)
            call InsertionSort(List, lastSorted)
         else
            call InsertionSort(List, lastSorted%next)
         end if
      end if
   end subroutine InsertionSort

   ! Вставка элемента в отсортированный список
   pure recursive subroutine Paste(current, itemToInsert)
      type(owner_node), allocatable, intent(inout) :: current
      type(owner_node), allocatable, intent(inout) :: itemToInsert

      if (itemToInsert%Phone > current%Phone) then
         call move_alloc(current, itemToInsert%next)
         call move_alloc(itemToInsert, current)
      else
         call Paste(current%next, itemToInsert)
      end if
   end subroutine Paste

end module module_process
