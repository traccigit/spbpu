module module_IO
   use Environment
   implicit none

   type owner_node
      character(15, CH_)           :: Surname
      integer(I_)                  :: Phone
      type(owner_node), allocatable :: next
   end type owner_node

contains

   subroutine Read_owner_list(filename, head)
      character(*), intent(in)       :: filename
      type(owner_node), allocatable, intent(out) :: head
      integer :: In, IO

      open(file=filename, encoding=E_, newunit=In, iostat=IO)
      call Handle_IO_Status(IO, "Opening input file "//filename)

      call Read_owner(In, head)

      close(In)
   end subroutine Read_owner_list

   recursive subroutine Read_owner(In, node)
      integer, intent(in)           :: In
      type(owner_node), allocatable, intent(out) :: node
      character(15, CH_)            :: surname
      integer(I_)                   :: phone
      integer                       :: IO

      read(In, '(a15, 1x, i10)', iostat=IO) surname, phone
      if (IO /= 0) then
         allocate(node)
         return
      end if

      allocate(node)
      node%Surname = trim(surname)
      node%Phone   = phone

      call Read_owner(In, node%next)
   end subroutine Read_owner


   subroutine WriteOriginalData(filename, head)
      character(*), intent(in)       :: filename
      type(owner_node), allocatable, intent(in) :: head
      integer :: Out, IO

      open(file=filename, encoding=E_, newunit=Out, iostat=IO)
      call Handle_IO_Status(IO, "Opening output file "//filename)

      write(Out, '("Исходный список:")', advance='no', iostat=IO)
      call Handle_IO_Status(IO, "writing header")
      write(Out, *)

      call Write_nodes(Out, head)

      close(Out)
   end subroutine WriteOriginalData

   subroutine WriteSortedData(filename, head)
      character(*), intent(in)       :: filename
      type(owner_node), allocatable, intent(in) :: head
      integer :: Out, IO

      open(file=filename, encoding=E_, position='append', newunit=Out, iostat=IO)
      call Handle_IO_Status(IO, "Opening output file for append "//filename)

      write(Out, '(/a)', iostat=IO) "Отсортированный список:"
      call Handle_IO_Status(IO, "writing sorted header")

      call Write_nodes(Out, head)

      close(Out)
   end subroutine WriteSortedData

! recursive subroutine Write_nodes(unit, node)
!    integer, intent(in)                      :: unit
!    type(owner_node), allocatable, intent(in) :: node
!    integer :: IO
!
!    if (.not. allocated(node)) return
!
!    if (len_trim(node%Surname) > 0) then
!       write(unit, '(a15, 1x, i10)', iostat=IO) node%Surname, node%Phone
!       call Handle_IO_Status(IO, "writing node")
!    end if
!
!    if (allocated(node%next)) then
!       call Write_nodes(unit, node%next)
!    end if
! end subroutine Write_nodes

recursive subroutine Write_nodes(unit, node)
   integer, intent(in)                      :: unit
   type(owner_node), allocatable, intent(in) :: node
   integer :: IO

   if (.not. allocated(node)) return

   ! Если это НЕ последний узел — выводим его
   if (allocated(node%next)) then
      if (len_trim(node%Surname) > 0) then
         write(unit, '(a15, 1x, i10)', iostat=IO) node%Surname, node%Phone
         call Handle_IO_Status(IO, "writing node")
      end if
      ! Рекурсивно обрабатываем следующий узел
      call Write_nodes(unit, node%next)
   end if
   ! Последний узел (где node%next не выделен) пропускается
end subroutine Write_nodes

   subroutine PrintElapsedTime(t_start, t_end)
      real, intent(in) :: t_start, t_end
      integer :: Out

      open(file='output.txt', position='append', encoding=E_, newunit=Out)
      write(Out, '(a, f10.6, a)') "Время выполнения: ", t_end - t_start, " секунд"
      close(Out)
   end subroutine PrintElapsedTime

end module module_IO
