program main
   use Environment
   use module_IO
   use module_process
   implicit none

   character(:), allocatable :: input_file, output_file
   type(owner_node), allocatable :: List
   real :: t_start, t_end


   input_file  = "../data/input.txt"
   output_file = "output.txt"

   call Read_owner_list(input_file, List)!сделать подпрограммой

   call WriteOriginalData(output_file, List)

   if (.not. allocated(List%next)) stop "Список пустой"
   !current = List%next

   call cpu_time(t_start)

   call InsertionSort(List, List)

   call cpu_time(t_end)

   call WriteSortedData(output_file, List)

   call PrintElapsedTime(t_start, t_end)

end program main
