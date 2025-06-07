program main
   use Environment
   use module_IO
   use module_process
   implicit none

   character(kind=CH_), allocatable :: surnames(:, :)
   integer(I_), allocatable         :: phones(:)

   character(*), parameter :: input_file  = "../data/input.txt"
   character(*), parameter :: output_file = "output.txt"
   real :: t_start, t_end

   call cpu_time(t_start)

   call Read_phone_list(input_file, surnames, phones)
   call WriteOriginalData(output_file, surnames, phones)
   call InsertionSort(surnames, phones)
   call WriteSortedData(output_file, surnames, phones)

   call cpu_time(t_end)

   call PrintElapsedTime(t_start, t_end)
end program main
