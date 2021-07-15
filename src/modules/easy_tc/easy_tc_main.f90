program easy_tc
    use :: algorithms_initializer, only : &
            algorithms_initialize, &
            algorithms_finalize

    use :: easy_tc_module, only : run_easy_tc

    implicit none

    call algorithms_initialize()

    call run_easy_tc()

    call algorithms_finalize()

end program easy_tc
