program tensor_data_usage
    use :: algorithms_initializer, only : &
            algorithms_initialize, &
            algorithms_finalize

    use :: tensor_data_usage_module, only : run_tensor_data_usage_examples

    implicit none

    call algorithms_initialize()

    call run_tensor_data_usage_examples()

    call algorithms_finalize()

end program tensor_data_usage
