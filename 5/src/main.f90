program main
   use Environment
   use module_IO
   use module_process
   implicit none

   character(len=*), parameter :: input_file  = '../data/input.txt'
   character(len=*), parameter :: data_file   = '../data/data.dat'
   character(len=*), parameter :: output_file = 'output.txt'

   type(owners_array) :: owners

         real :: t_start, t_end  ! ✅ ЭТО ОЧЕНЬ ВАЖНО


   call cpu_time(t_start)

   call Create_data_file(input_file, data_file)
   !owners = Read_owner_list(data_file)
   call Read_owner_list(data_file, owners)

   call WriteOriginalData(output_file, owners)
   call InsertionSort(owners)
   call WriteSortedData(output_file, owners)



   call cpu_time(t_end)

   call PrintElapsedTime(t_start, t_end)
end program main
