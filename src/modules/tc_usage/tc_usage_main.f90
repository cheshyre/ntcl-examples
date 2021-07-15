program tc_usage
    use :: algorithms_initializer, only : &
            algorithms_initialize, &
            algorithms_finalize

    use :: tc_usage_module, only : run_tc_usage_examples

    implicit none

    call algorithms_initialize()

    call run_tc_usage_examples()

    call algorithms_finalize()

end program tc_usage
