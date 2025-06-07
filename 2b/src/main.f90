program main
   use Environment
   use module_IO
   use module_process
   implicit none

   character(kind=CH_), allocatable :: surnames(:, :)
   integer(I_), allocatable         :: phones(:)
   real :: t_start, t_end

   character(*), parameter :: input_file  = "../data/input.txt"
   character(*), parameter :: output_file = "output.txt"

   call Read_phone_list(input_file, surnames, phones)
   call WriteOriginalData(output_file, surnames, phones)

   call cpu_time(t_start)

   call InsertionSort(surnames, phones)

   call cpu_time(t_end)


   call WriteSortedData(output_file, surnames, phones)

   call PrintElapsedTime(t_start, t_end)
end program main
