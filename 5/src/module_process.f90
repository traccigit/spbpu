module module_process
   use Environment
   use module_IO
   implicit none

contains

   !--------------------------------------
   !
   !InsertionSort вызывает рекурсивную
   !подпрограмму InsertionSortTailRec, начиная с i = 2
   !
   !--------------------------------------

   subroutine InsertionSort(owners)
      type(owners_array), intent(inout) :: owners
      call InsertionSortTailRec(owners, 2)
   end subroutine InsertionSort

   !-------------------------------------------------
   !
   !InsertionSortTailRec работает как хвостовая
   !рекурсия: вся работа завершена до следующего
   !вызова, нет необходимости возвращаться.
   !
   !----------------------------------------------

   recursive subroutine InsertionSortTailRec(owners, i)
      type(owners_array), intent(inout) :: owners
      integer, intent(in) :: i
      integer :: j

      if (i > size(owners%Phone)) return

      j = i - 1
      do while (j >= 1 .and. owners%Phone(j) < owners%Phone(i))
         j = j - 1
      end do

      ! Смещаем с помощью cshift
      owners%Phone(j+1:i)   = cshift(owners%Phone(j+1:i),   -1)
      owners%Surname(j+1:i) = cshift(owners%Surname(j+1:i), -1)

      ! Хвостовой вызов
      call InsertionSortTailRec(owners, i + 1)
   end subroutine InsertionSortTailRec

end module module_process
