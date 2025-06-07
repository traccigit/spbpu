module module_process
   use Environment
   use module_io
   implicit none

contains

   recursive subroutine DeleteChars(Str, N, M)
      type(CharNode), pointer, intent(inout) :: Str
      type(CharNode), pointer                :: temp
      integer, intent(in)                    :: N, M

      if (N > 1 .and. associated(Str)) then
         call DeleteChars(Str%next, N-1, M)
      else if (M > 0 .and. associated(Str)) then
         temp => Str%next
         deallocate(Str)
         Str => temp
         call DeleteChars(Str, N, M-1)
      end if
   end subroutine DeleteChars

   recursive subroutine InsertChars(Str, N, insertStr)
      type(CharNode), pointer, intent(inout) :: Str
      type(CharNode), pointer, intent(in)    :: insertStr
      type(CharNode), pointer                :: newNode
      integer, intent(in)                    :: N

      if (N > 1 .and. associated(Str)) then
         call InsertChars(Str%next, N-1, insertStr)
      else if (N == 1 .and. associated(insertStr)) then
         allocate(newNode)
         newNode%char = insertStr%char
         newNode%next => Str
         Str => newNode
         call InsertChars(Str%next, N, insertStr%next)
      end if
   end subroutine InsertChars

end module module_process
