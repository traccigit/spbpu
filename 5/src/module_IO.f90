! module module_IO
!    use Environment
!    implicit none
!
!    integer, parameter :: SURNAME_LEN = 15, PHONE_LEN = 10
!
!    type owners_array
!       character(SURNAME_LEN), allocatable :: Surname(:)
!       character(PHONE_LEN),   allocatable :: Phone(:)
!    end type owners_array
!
! contains
!
!    integer function CountLines(filename)
!       character(*), intent(in) :: filename
!       integer :: In, io
!       CountLines = 0
!       open(file=filename, newunit=In)
!       do
!          read(In, *, iostat=io)
!          if (io /= 0) exit
!          CountLines = CountLines + 1
!       end do
!       close(In)
!    end function CountLines
!
!    subroutine Create_data_file(input_file, data_file)
!       character(*), intent(in) :: input_file, data_file
!       character(SURNAME_LEN) :: s
!       character(PHONE_LEN)   :: p
!       integer :: In, Out, IO, i, recl, n
!       character(:), allocatable :: format
!
!       n = CountLines(input_file)
!       open(file=input_file, encoding=E_, newunit=In)
!       recl = SURNAME_LEN + PHONE_LEN
!       open(file=data_file, form='unformatted', newunit=Out, access='direct', recl=recl)
!       format = '(a15, 1x, a10)'
!       do i = 1, n
!          read(In, *, iostat=IO) s, p
!          if (IO /= 0) exit
!          write(Out, iostat=IO, rec=i) s // p
!          if (IO /= 0) exit
!       end do
!       close(In)
!       close(Out)
!    end subroutine Create_data_file
!
!    function Read_owner_list(data_file) result(owners)
!       character(*), intent(in) :: data_file
!       type(owners_array) :: owners
!       integer :: n, In, IO, recl, i
!       character(SURNAME_LEN + PHONE_LEN) :: buffer
!
!       recl = SURNAME_LEN + PHONE_LEN
!       open(file=data_file, form='unformatted', newunit=In, access='direct', recl=recl)
!
!       ! Считаем количество записей
!       n = 0
!       do
!          read(In, iostat=IO, rec=n + 1)
!          if (IO /= 0) exit
!          n = n + 1
!       end do
!       close(In)
!
!       allocate(owners%Surname(n), owners%Phone(n))
!       open(file=data_file, form='unformatted', newunit=In, access='direct', recl=recl)
!       do i = 1, n
!          read(In, rec=i) buffer
!          owners%Surname(i) = buffer(1:SURNAME_LEN)
!          owners%Phone(i)   = buffer(SURNAME_LEN+1:)
!       end do
!       close(In)
!    end function Read_owner_list
!
!    subroutine WriteOriginalData(filename, owners)
!       character(*), intent(in) :: filename
!       type(owners_array), intent(in) :: owners
!       integer :: Out, i
!
!       open(file=filename, encoding=E_, newunit=Out)
!       write(Out, '("Исходный список:")')
!       do i = 1, size(owners%Surname)
!          write(Out, '(a15, 1x, a10)') owners%Surname(i), owners%Phone(i)
!       end do
!       close(Out)
!    end subroutine WriteOriginalData
!
!    subroutine WriteSortedData(filename, owners)
!       character(*), intent(in) :: filename
!       type(owners_array), intent(in) :: owners
!       integer :: Out, i
!
!       open(file=filename, encoding=E_, position='append', newunit=Out)
!       write(Out, '("Отсортированный список:")')
!       do i = 1, size(owners%Surname)
!          write(Out, '(a15, 1x, a10)') owners%Surname(i), owners%Phone(i)
!       end do
!       close(Out)
!    end subroutine WriteSortedData
!
! end module module_IO
!
!
!


module module_IO
   use Environment
   implicit none

   integer, parameter :: SURNAME_LEN = 15

   type owners_array
      character(SURNAME_LEN), allocatable :: Surname(:)
      integer(I_), allocatable            :: Phone(:)
   end type owners_array

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
subroutine Create_data_file(input_file, data_file)
   character(*), intent(in) :: input_file, data_file
   character(SURNAME_LEN) :: s
   integer(I_) :: p
   integer :: In, Out, IO, i, n
   character(SURNAME_LEN+10) :: buffer
   integer, parameter :: RECL = SURNAME_LEN + 10  ! 15 + 10 = 25 байт — как строка

   n = CountLines(input_file)

   open(file=input_file, encoding=E_, newunit=In)
   open(file=data_file, form='unformatted', access='direct', recl=RECL, newunit=Out)

   do i = 1, n
      read(In, '(a15, I10)', iostat=IO) s, p
      if (IO /= 0) cycle

      ! Формируем буфер вручную
      write(buffer, '(a15, I10)') s, p

      ! Записываем как одну строку
      write(Out, rec=i) buffer
   end do

   close(In)
   close(Out)
end subroutine Create_data_file




subroutine Read_owner_list(filename, owners)
   character(*), intent(in)     :: filename
   type(owners_array), intent(out) :: owners
   integer :: i, rec, IO, In, n
   character(SURNAME_LEN+10) :: buffer
   integer, parameter :: RECL = SURNAME_LEN + 10

   ! Подсчёт записей
   open(file=filename, form='unformatted', access='direct', recl=RECL, newunit=In)
   n = 0
   do
      read(In, rec=n+1, iostat=IO)
      if (IO /= 0) exit
      n = n + 1
   end do
   close(In)

   allocate(owners%Surname(n), owners%Phone(n))

   open(file=filename, form='unformatted', access='direct', recl=RECL, newunit=In)
   do i = 1, n
      read(In, rec=i) buffer
      owners%Surname(i) = buffer(1:SURNAME_LEN)
      read(buffer(SURNAME_LEN+1:), *) owners%Phone(i)
   end do
   close(In)
end subroutine Read_owner_list




   subroutine WriteOriginalData(filename, owners)
      character(*), intent(in) :: filename
      type(owners_array), intent(in) :: owners
      integer :: Out, i

      open(file=filename, encoding=E_, newunit=Out)
      write(Out, '("Исходный список:")')
      do i = 1, size(owners%Surname)
         write(Out, '(a15, 1x, I10)') owners%Surname(i), owners%Phone(i)
      end do
      close(Out)
   end subroutine WriteOriginalData

   subroutine WriteSortedData(filename, owners)
      character(*), intent(in) :: filename
      type(owners_array), intent(in) :: owners
      integer :: Out, i

      open(file=filename, encoding=E_, position='append', newunit=Out)
      write(Out, '("Отсортированный список:")')
      do i = 1, size(owners%Surname)
         write(Out, '(a15, 1x, I10)') owners%Surname(i), owners%Phone(i)
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
