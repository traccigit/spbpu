program main

   use Environment
   use module_IO
   use module_process
   implicit none

   character(:), allocatable :: inFile, outFile
   character(SURNAME_LEN), allocatable :: surnames(:)
   integer(I_),            allocatable :: phones(:)
   integer :: n
   real :: t_start, t_end

   inFile  = "../data/input.txt"      ! входной файл
   outFile = "output.txt"      ! выходной



   call cpu_time(t_start)

   !чтение
   call read_phone_list(inFile,  surnames, phones, n)

   ! Вывод исходного списка
   call write_phone_list(outFile, surnames, phones, n, "Исходный")

   !работа
   call insert_sort(surnames, phones)

   !вывод отработанного списка
   call write_phone_list(outFile, surnames, phones, n, "Отсортировано")



   call cpu_time(t_end)

   call PrintElapsedTime(t_start, t_end)
end program main
