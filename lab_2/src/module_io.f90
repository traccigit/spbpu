module module_io
   use Environment
   implicit none

   type CharNode
      character(CH_)           :: char
      type(CharNode), pointer  :: next => null()
   end type CharNode

contains

   function ReadString(filename, unit) result(Str)
      character(*), intent(in)       :: filename
      integer, intent(out)           :: unit
      type(CharNode), pointer        :: Str

      open(file=filename, newunit=unit, encoding=E_)
      Str => ReadChar(unit)
   end function ReadString

   recursive function ReadChar(unit) result(Str)
      integer, intent(in)        :: unit
      type(CharNode), pointer    :: Str
      character(1, CH_)          :: c
      integer                    :: io

      read(unit, '(a)', advance='no', iostat=io) c

      if (io == 0) then
         allocate(Str)
         Str%char = c
         Str%next => ReadChar(unit)
      else
         Str => null()
      end if
   end function ReadChar

   recursive subroutine OutputChars(unit, Str)
      integer, intent(in)       :: unit
      type(CharNode), pointer   :: Str
      if (.not. associated(Str)) return
      write(unit, '(a)', advance='no') Str%char
      call OutputChars(unit, Str%next)
   end subroutine OutputChars

   subroutine WriteString(filename, Str, mode)
      character(*), intent(in)           :: filename
      type(CharNode), pointer            :: Str
      character(*), intent(in), optional :: mode
      integer                            :: unit

      if (present(mode) .and. mode == 'append') then
         open(file=filename, newunit=unit, position='append', encoding=E_)
      else
         open(file=filename, newunit=unit, encoding=E_)
      end if

      call OutputChars(unit, Str)
      write(unit, *)  ! Перенос строки
      close(unit)
   end subroutine WriteString

   function StringToList(s) result(list)
      character(*, CH_), intent(in) :: s
      type(CharNode), pointer       :: list, tail, newNode
      integer                       :: i

      list => null()
      tail => null()
      do i = 1, len_trim(s)
         allocate(newNode)
         newNode%char = s(i:i)
         newNode%next => null()
         if (.not. associated(list)) then
            list => newNode
         else
            tail%next => newNode
         end if
         tail => newNode
      end do
   end function StringToList

   subroutine WriteCommand(filename, action, N, M, insertStr)
      character(*), intent(in)      :: filename
      character(CH_), intent(in)    :: action
      integer, intent(in)           :: N, M
      type(CharNode), pointer       :: insertStr
      integer                       :: unit

      open(file=filename, newunit=unit, position='append', encoding=E_)
      write(unit, '(a,1x,i0,1x,i0)', advance='no') action, N, M
      if (action == 'I') then
         call OutputChars(unit, insertStr)
      end if
      write(unit, *)
      close(unit)
   end subroutine WriteCommand

end module module_io
