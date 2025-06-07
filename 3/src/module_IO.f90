! module module_IO
!    use Environment
!    implicit none
!
!    ! Параметры длины полей
!    integer, parameter :: SURNAME_LEN = 15, PHONE_LEN = 10
!
!    ! Определение структуры owner
!    type owner
!       character(SURNAME_LEN) :: Surname
!       character(PHONE_LEN)   :: Phone
!    end type owner
!
! contains
!    ! Подсчёт строк
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
!       type(owner)              :: ownerers
!       integer                  :: In, Out, IO, i, recl, n
!       character(:), allocatable :: format
!
!       n = CountLines(input_file)
!
!       ! Создание файла
!       open(file=input_file, encoding=E_, newunit=In)
!       recl = SURNAME_LEN + PHONE_LEN
!       open(file=data_file, form='unformatted', newunit=Out, access='direct', recl=recl)
!       format = '(a15, 1x, a10)'
!          do i = 1, n
!             read(In, *, iostat=IO) ownerers%Surname, ownerers%Phone
!             if (IO /= 0) exit ! чтение ошбики
!             write(Out, iostat=IO, rec=i) ownerers
!             if (IO /= 0) exit ! запись ошбкп
!          end do
!       close(In)
!       close(Out)
!    end subroutine Create_data_file
!
!    ! Чтение
!    function Read_owner_list(Data_File) result(owners)
!       character(*), intent(in) :: Data_File
!       type(owner), allocatable :: owners(:)
!       integer :: n, In, IO, recl, i
!
!       recl = SURNAME_LEN + PHONE_LEN
!       open(file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
!       n = 0
!          do
!             read(In, iostat=IO, rec=n + 1)
!             if (IO /= 0) exit
!             n = n + 1
!          end do
!       close(In)
!
!       allocate(owners(n))
!
!       open(file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
!          do i = 1, n
!             read(In, iostat=IO, rec=i) owners(i)
!          end do
!       close(In)
!    end function Read_owner_list
!
!    !Вывод исходного
!    subroutine WriteOriginalData(filename, owners)
!    character(*), intent(in) :: filename
!    type(owner), intent(in)  :: owners(:)
!    integer :: Out
!
!       open(file=filename, encoding=E_, newunit=Out)
!          write(Out, '("Исходный список:")')
!          write(Out, '(a15, 1x, a10)') owners
!       close(Out)
!    end subroutine WriteOriginalData
!
!    !Вывод отсортированного
!    subroutine WriteSortedData(filename, owners)
!       character(*), intent(in) :: filename
!       type(owner), intent(in)  :: owners(:)
!       integer :: Out
!
!       open(file=filename, encoding=E_, position='append', newunit=Out)
!          write(Out, '("Отсортированный список:")')
!          write(Out, '(a15, 1x, a10)') owners
!       close(Out)
!    end subroutine WriteSortedData
!
! end module module_IO



module module_IO
  use Environment
  implicit none

  ! Параметры длины полей
  integer, parameter :: SURNAME_LEN = 15, PHONE_LEN = 10

  ! Определение структуры owner
  type owner
    character(SURNAME_LEN, kind=CH_) :: Surname
    character(PHONE_LEN, kind=CH_) :: Phone
  end type owner

contains

  ! Подсчет количества строк в файле
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
    type(owner) :: ownerr
    integer :: In, Out, IO, i, recl, n
    character(:), allocatable :: format

    ! Подсчет строк и выделение памяти
    n = CountLines(input_file)
    ! Создание неформатированного файла
    open(file=input_file, encoding=E_, newunit=In)
    recl = (SURNAME_LEN + PHONE_LEN) * CH_
    open(file=data_file, form='unformatted', newunit=Out, access='direct', recl=recl)
    format = '(a15, 1x, a10)'
    do i = 1, n
      read(In, format, iostat=IO) ownerr
      if (IO /= 0) exit  ! Пропускаем ошибки чтения
      write(Out, iostat=IO, rec=i) ownerr
      if (IO /= 0) exit  ! Пропускаем ошибки записи
    end do
    close(In)
    close(Out)
  end subroutine Create_data_file

  ! Чтение данных из неформатированного файла
  function Read_owner_list(Data_File) result(owners)
    character(*), intent(in) :: Data_File
    type(owner), allocatable :: owners(:)
    integer :: n, In, IO, recl, i

    recl = (SURNAME_LEN + PHONE_LEN) * CH_

    open(file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
    n = 0
    do
      read(In, iostat=IO, rec=n + 1)
      if (IO /= 0) exit
      n = n + 1
    end do
    close(In)

    allocate(owners(n))

    open(file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
    do i = 1, n
      read(In, iostat=IO, rec=i) owners(i)
    end do
    close(In)
  end function Read_owner_list

  ! Вывод исходного списка
  subroutine WriteOriginalData(filename, owners)
    character(*), intent(in) :: filename
    type(owner), intent(in) :: owners(:)
    integer :: Out

    open(file=filename, encoding=E_, newunit=Out)
    write(Out, '(a)') "Исходный список:"
    write(Out, '(a15, 1x, a10)') owners
    close(Out)
  end subroutine WriteOriginalData

  ! Вывод отсортированного списка
  subroutine WriteSortedData(filename, owners)
    character(*), intent(in) :: filename
    type(owner), intent(in) :: owners(:)
    integer :: Out

    open(file=filename, encoding=E_, position='append', newunit=Out)
    write(Out, '(a)') "Отсортированный список:"
    write(Out, '(a15, 1x, a10)') owners
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

