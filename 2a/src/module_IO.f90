module module_IO
   use Environment
   implicit none

   integer, parameter :: SURNAME_LEN = 15
   integer, parameter :: PHONE_LEN   = 10

contains

   integer function CountLines(filename)
      character(*), intent(in) :: filename
      integer :: In, io
      CountLines = 0
      open(file=filename, newunit=In)
      do
         read(In, *, iostat=io)
         if (io /= 0) exit
         CountLines = CountLines + 1
      end do
      close(In)
   end function CountLines

   subroutine Read_phone_list(filename, surnames, phones)
      character(*), intent(in)                      :: filename
      character(kind=CH_), allocatable, intent(out) :: surnames(:, :)
      integer(I_), allocatable, intent(out)         :: phones(:)

      integer :: n, i, j, In
      character(SURNAME_LEN, CH_) :: s
      character(PHONE_LEN, CH_)   :: p

      n = CountLines(filename)
      allocate(surnames(n, SURNAME_LEN), phones(n))

      open(file=filename, newunit=In, encoding=E_)
      do i = 1, n
         read(In, '(a15, 1x, a10)') s, p
         do j = 1, SURNAME_LEN
            surnames(i, j) = s(j:j)
         end do
         read(p, *) phones(i)
      end do
      close(In)
   end subroutine Read_phone_list

   subroutine WriteOriginalData(filename, surnames, phones)
      character(*), intent(in)                      :: filename
      character(kind=CH_), intent(in)               :: surnames(:, :)
      integer(I_), intent(in)                       :: phones(:)

      integer :: i, j, n, Out
      character(SURNAME_LEN, CH_) :: surname_str

      n = size(phones)
      open(file=filename, newunit=Out, encoding=E_)
      write(Out, '("Исходный список:")')
      do i = 1, n
         do j = 1, SURNAME_LEN
            surname_str(j:j) = surnames(i, j)  !читаем строку фамилии
         end do
         write(Out, '(a15, 1x, i10)') surname_str, phones(i)
      end do
      close(Out)
   end subroutine WriteOriginalData

   subroutine WriteSortedData(filename, surnames, phones)
      character(*), intent(in)                      :: filename
      character(kind=CH_), intent(in)               :: surnames(:, :)
      integer(I_), intent(in)                       :: phones(:)

      integer :: i, j, n, Out
      character(SURNAME_LEN, CH_) :: surname_str

      n = size(phones)
      open(file=filename, newunit=Out, position='append', encoding=E_)
      write(Out, '("Отсортированный список:")')
      do i = 1, n
         do j = 1, SURNAME_LEN
            surname_str(j:j) = surnames(i, j)  !читаем строку фамилии
         end do
         write(Out, '(a15, 1x, i10)') surname_str, phones(i)
      end do
      close(Out)
   end subroutine WriteSortedData

         subroutine PrintElapsedTime(t_start, t_end)
  real, intent(in) :: t_start, t_end
  integer :: Out

  open(file='output.txt', position='append', encoding=E_, newunit=Out)
  write(Out, '(a, f10.6, a)') "⏱ Время выполнения программы: ", t_end - t_start, " секунд"
  close(Out)
end subroutine PrintElapsedTime

end module module_IO
