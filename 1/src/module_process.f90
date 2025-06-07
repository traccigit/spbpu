module module_process

   use Environment
   use module_IO
   implicit none

contains
subroutine insert_sort(surnames, phones)
   character(SURNAME_LEN), intent(inout) :: surnames(:)
   integer(I_),            intent(inout) :: phones(:)

   integer :: i, j
   integer(I_) :: keyPhone
   character(SURNAME_LEN) :: keyName

   do i = 2, size(phones)
      keyPhone = phones(i)
      keyName  = surnames(i)
      j = i - 1
      do while (j >= 1 .and. phones(j) < keyPhone)
!         phones(j+1)   = phones(j)
!         surnames(j+1) = surnames(j)
         j = j - 1
      end do
!      phones(j+1)   = keyPhone cshift
!      surnames(j+1) = keyName
       surnames(j+1:i) = cshift(surnames(j+1:i), -1)
       phones(j+1:i)   = cshift(phones(j+1:i), -1)
   end do
end subroutine insert_sort


! subroutine insert_sort(surnames, phones)
!    character(SURNAME_LEN), intent(inout) :: surnames(:)
! !   integer(I_),            intent(inout) :: phones(:)
!
! !   integer :: i, j
!
!    do i = 2, size(surnames)
!       j = i - 1
!       do while (j >= 1 .and. surnames(j) > surnames(i))
!          j = j - 1
!       end do
!
!       ! Сдвигаем фамилии и телефоны на одну позицию вправо
!       surnames(j+1:i) = cshift(surnames(j+1:i), -1)
!       phones(j+1:i)   = cshift(phones(j+1:i), -1)
!    end do
! end subroutine insert_sort

end module module_process
