program main
    use :: timed_app_entry_module, only : timed_app_entry
    use :: timed_tc_app_module, only : timed_tc_app

    use :: algorithms_initializer, only : &
            algorithms_initialize, &
            algorithms_finalize

    implicit none

    type(timed_app_entry) :: app

    call algorithms_initialize()

    app = timed_app_entry(timed_tc_app())
    call app%run()
    call app%cleanup()

    call algorithms_finalize()
end program main
