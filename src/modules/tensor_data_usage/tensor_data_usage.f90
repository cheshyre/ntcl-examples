module tensor_data_usage_module

    use :: data_api, only : &
            scratch_buffer, &
            create_scratch_buffer

    use :: tensor_api, only : &
            tensor, &
            allocate_and_copy_tensor, &
            allocate_and_create_tensor, &
            allocate_and_copy_tensor_to_scratch, &
            allocate_and_create_tensor_in_scratch, &
            secure_fortran_pointer_from_tensor, &
            release_pointer_from_remote_tensor, &
            update_remote_tensor_and_release_pointer



    implicit none
    private

    public :: run_tensor_data_usage_examples
contains
    subroutine run_tensor_data_usage_examples()

        call run_allocate_and_copy_example_default1()
        call run_allocate_and_create_example_default1()

        call run_allocate_and_copy_example_scratch1()
        call run_allocate_and_create_example_scratch1()

        call run_interaction_example()
    end subroutine run_tensor_data_usage_examples

    subroutine run_allocate_and_copy_example_default1()

        real, dimension(10,10,10) :: fortran_real_rank3_array
        class(tensor), allocatable :: ntcl_real_rank3_tensor

        call populate_rank_rank3_array(fortran_real_rank3_array)

        call allocate_and_copy_tensor(ntcl_real_rank3_tensor, &
                fortran_real_rank3_array)
    end subroutine run_allocate_and_copy_example_default1

    subroutine run_allocate_and_create_example_default1()

        class(tensor), allocatable :: ntcl_real_rank3_tensor

        call allocate_and_create_tensor(ntcl_real_rank3_tensor, &
                [10,10,10], "real32")
    end subroutine run_allocate_and_create_example_default1

    subroutine run_allocate_and_copy_example_scratch1()

        real, dimension(10,10,10) :: fortran_real_rank3_array
        class(tensor), allocatable :: ntcl_real_rank3_tensor
        type(scratch_buffer) :: scratch

        call create_scratch_buffer(scratch)
        call scratch%initialize()

        call populate_rank_rank3_array(fortran_real_rank3_array)

        call allocate_and_copy_tensor_to_scratch(ntcl_real_rank3_tensor, &
                scratch, fortran_real_rank3_array)
    end subroutine run_allocate_and_copy_example_scratch1

    subroutine run_allocate_and_create_example_scratch1()

        class(tensor), allocatable :: ntcl_real_rank3_tensor
        type(scratch_buffer) :: scratch

        call create_scratch_buffer(scratch)
        call scratch%initialize()

        call allocate_and_create_tensor_in_scratch(ntcl_real_rank3_tensor, &
                scratch, [10,10,10], "real32")
    end subroutine run_allocate_and_create_example_scratch1

    subroutine run_interaction_example()
        class(tensor), allocatable :: a, b
        real, dimension(:,:), pointer, contiguous :: a_ptr, b_ptr

        call allocate_and_create_tensor(a, [10,10], "real32")
        call allocate_and_create_tensor(b, [10,10], "real32")

        call secure_fortran_pointer_from_tensor(a_ptr, a)
        call secure_fortran_pointer_from_tensor(b_ptr, b)

        ! Modify a_ptr, leave b_ptr alone.

        call update_remote_tensor_and_release_pointer(a_ptr, a)
        call release_pointer_from_remote_tensor(b_ptr, b)

        call a%cleanup(); call b%cleanup()
        deallocate(a, b)
    end subroutine run_interaction_example

    subroutine populate_rank_rank3_array(array)
        real, dimension(:,:,:), intent(inout) :: array

        call random_number(array)
    end subroutine populate_rank_rank3_array
end module tensor_data_usage_module
