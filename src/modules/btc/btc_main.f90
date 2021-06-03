program main
    use :: util_api, only : application_config

    use :: algorithms_initializer, only : &
            algorithms_initialize, &
            algorithms_finalize

    use :: btc_app_module, only : btc_app

    type(application_config) :: config
    type(btc_app) :: app

    call algorithms_initialize()

    call config%read_commandline()
    app = btc_app(config)

    call app%run()

    call app%cleanup()
    call algorithms_finalize()
end program main
