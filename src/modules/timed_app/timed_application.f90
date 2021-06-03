module timed_application_module
    use :: util_api, only : &
            measurement, &
            application_config

    implicit none
    private

    public :: timed_application

    type, abstract :: timed_application
    contains
        procedure(initialize_interface), deferred :: initialize
        procedure(empty), deferred :: run
        procedure(empty), deferred :: zero_measurements
        procedure(get_interface), deferred :: get_measurements
        procedure(empty), deferred :: cleanup
    end type timed_application

    abstract interface
        subroutine initialize_interface(this, config)
            import :: timed_application
            import :: application_config

            class(timed_application), intent(inout) :: this
            type(application_config), intent(in) :: config
        end subroutine initialize_interface

        subroutine empty(this)
            import :: timed_application

            class(timed_application), intent(inout) :: this
        end subroutine empty

        function get_interface(this) result(measurements)
            import :: timed_application
            import :: measurement

            class(timed_application), intent(in) :: this
            type(measurement), dimension(:), allocatable :: measurements
        end function get_interface
    end interface
end module timed_application_module
