program main
   use Environment
   use module_IO
   use module_process
   implicit none


   type(owner_node), pointer  :: owners_list => null()
   real :: t_start, t_end

   owners_list => Read_owner_list("../data/input.txt")

   call WriteOriginalData("output.txt", owners_list)


   call cpu_time(t_start)

   call InsertionSort(owners_list, owners_list)

   call cpu_time(t_end)

   call WriteSortedData("output.txt", owners_list)

   call PrintElapsedTime(t_start, t_end)
end program main
