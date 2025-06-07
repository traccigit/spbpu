module module_IO
   use Environment
   implicit none

   type owner_node
      character(15, kind=CH_)    :: Surname
      integer(I_)                :: Phone
      type(owner_node), pointer  :: next => null()
   end type owner_node

contains

   function Read_owner_list(filename) result(head)
      character(*), intent(in)   :: filename
      type(owner_node), pointer  :: head
      integer                    :: In, IO

      open(file=filename, encoding=E_, newunit=In, iostat=IO)
      call Handle_IO_Status(IO, "Opening file "//filename)

      head => Read_owner(In)

      close(In)
   end function Read_owner_list


   recursive function Read_owner(In) result(node)
      integer, intent(in)        :: In
      type(owner_node), pointer  :: node
      character(15, kind=CH_)    :: surname
      integer(I_)                :: Phone
      integer                   :: IO

      read(In, '(a15, 1x, i10)', iostat=IO) surname, phone

      call Handle_IO_Status(IO, "reading owner from file")

      if (IO == 0) then
         allocate(node)
         node%Surname = Trim(surname)
         node%Phone = phone
         node%next    => Read_owner(In)

         !print *, "SUR: [", trim(surname), "]"
         !print *, "PHONE:", phone

      else
         nullify(node)
      end if
   end function Read_owner

   subroutine WriteOriginalData(filename, head)
      character(*), intent(in)   :: filename
      type(owner_node), pointer  :: head
      integer                   :: Out, IO
      type(owner_node), pointer  :: current

      open(file=filename, encoding=E_, newunit=Out, iostat=IO)
      call Handle_IO_Status(IO, "Opening output file "//filename)

      write(Out, '("Исходный список:")', advance='no', iostat=IO)
      call Handle_IO_Status(IO, "writing header")
      write(Out, *)

      current => head
      do while (associated(current))
         write(Out, '(a, 1x, i10)', iostat=IO) current%Surname, current%Phone
         call Handle_IO_Status(IO, "writing owner data")
         current => current%next

      end do

      close(Out)
   end subroutine WriteOriginalData

   subroutine WriteSortedData(filename, head)
      character(*), intent(in)   :: filename
      type(owner_node), pointer  :: head
      integer                   :: Out, IO
      type(owner_node), pointer  :: current

      open(file=filename, encoding=E_, position='append', newunit=Out, iostat=IO)
      call Handle_IO_Status(IO, "Opening output file for append "//filename)

      write(Out, '(/a)', iostat=IO) "Отсортированный список:"
      call Handle_IO_Status(IO, "writing sorted header")

      current => head
      do while (associated(current))
         write(Out, '(a, 1x, i10)', iostat=IO) current%Surname, current%Phone
         call Handle_IO_Status(IO, "writing sorted owner data")
         current => current%next
      end do

      close(Out)
   end subroutine WriteSortedData

   subroutine PrintElapsedTime(t_start, t_end)
      real, intent(in) :: t_start, t_end
      integer :: Out

      open(file='output.txt', position='append', encoding=E_, newunit=Out)
      write(Out, '(a, f10.6, a)') "Время выполнения: ", t_end - t_start, " секунд"
      close(Out)
   end subroutine PrintElapsedTime

end module module_IO
