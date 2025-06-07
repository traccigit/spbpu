program main
   use Environment
   use module_io
   use module_process
   implicit none

   character(:), allocatable         :: input_file, output_file
   type(CharNode), pointer           :: Str, insertStr
   integer                           :: N, M, In, io
   character(CH_)                    :: action
   character(1024, CH_)              :: buffer

   input_file  = "../data/input.txt"
   output_file = "output.txt"

   Str => ReadString(input_file, In)
   if (associated(Str)) then
      call WriteString(output_file, Str, 'rewind')

      do
read(buffer, *) action, N
if (action == 'X') exit

if (action == 'D') then
   read(buffer, *) action, N, M
   call DeleteChars(Str, N, M)
else if (action == 'I') then
   insertStr => StringToList(trim(buffer(index(buffer, char(iachar(' '), CH_), back=.true.) + 1:)))
   call InsertChars(Str, N, insertStr)
end if


         call WriteCommand(output_file, action, N, M, insertStr)
         call WriteString(output_file, Str, 'append')
      end do
   end if

end program main
