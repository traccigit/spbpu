module module_IO
   use Environment
   implicit none

   integer,  parameter :: SURNAME_LEN = 15
   integer,  parameter :: PHONE_LEN   = 10
   character(len=*), parameter :: format = '(a15,1x,i10)'

contains

   subroutine read_phone_list(fname, surnames, phones, n)
      character(*), intent(in)  :: fname
      character(SURNAME_LEN), allocatable, intent(out) :: surnames(:)
      integer(I_),              allocatable, intent(out) :: phones(:)
      integer,                  intent(out) :: n

      integer :: In, io, idx
      character(50) :: line
      character(SURNAME_LEN) :: name
      integer(I_) :: phone

      ! Подсчёт строк
      n = 0
      open (newunit=In, file=fname, encoding=E_)
      do
         read (In, '(a)', iostat=io) line
         if (io == IOSTAT_END) exit
         if (len_trim(line) < SURNAME_LEN + 1) cycle
         n = n + 1
      end do
      close (In)

      allocate (surnames(n), phones(n))

      ! Чтение
      idx = 0
      open (newunit=In, file=fname, encoding=E_)
      do
         read (In, '(a)', iostat=io) line
         if (io == IOSTAT_END) exit
         if (len_trim(line) < SURNAME_LEN + 1) cycle

         name = adjustl(line(:SURNAME_LEN))
         read (line(SURNAME_LEN+2:), *, iostat=io) phone
         if (io /= 0) cycle

         idx = idx + 1
         surnames(idx) = name
         phones(idx)   = phone
      end do
      close (In)
   end subroutine read_phone_list

   subroutine write_phone_list(fname, surnames, phones, n, title)
      character(*),           intent(in) :: fname
      character(SURNAME_LEN), intent(in) :: surnames(:)
      integer(I_),            intent(in) :: phones(:)
      integer,                intent(in) :: n
      character(*), optional, intent(in) :: title

      integer :: Out, io, i

      open (newunit=Out, file=fname, position='append', encoding=E_)

      if (present(title)) then
         write(Out, *) trim(title)
      end if

      do i = 1, n
         write (Out, format, iostat=io) surnames(i), phones(i)
      end do

      close (Out)
   end subroutine write_phone_list

      subroutine PrintElapsedTime(t_start, t_end)
  real, intent(in) :: t_start, t_end
  integer :: Out

  open(file='output.txt', position='append', encoding=E_, newunit=Out)
  write(Out, '(a, f10.6, a)') "Время выполнения: ", t_end - t_start, " секунд"
  close(Out)
end subroutine PrintElapsedTime

end module module_IO



