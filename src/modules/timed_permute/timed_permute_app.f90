module timed_permute_app_module
    use, intrinsic :: iso_fortran_env, only : &
            real64, &
            int64

    use :: util_api, only : &
            measurement, &
            string, &
            string_converter, &
            timer, &
            application_config

    use :: tensor_api, only : tensor_datatype_helper

    use :: timed_application_module, only : timed_application
    use :: permute_app_module, only : permute_app

    implicit none
    private

    public :: timed_permute_app

    type, extends(timed_application) :: timed_permute_app
        type(permute_app) :: app
        integer(int64), dimension(:), allocatable :: sizes
        integer :: datatype_size
        type(measurement) :: allocated_gibibytes, gibibytes_read, gibibytes_written
        type(measurement) :: initial_allocation_time, permute_time
    contains
        procedure :: initialize => initialize
        procedure :: run => run
        procedure :: zero_measurements => zero_measurements
        procedure :: get_measurements => get_measurements
        procedure :: cleanup => cleanup
        procedure, private :: initialize_metrics => initialize_metrics
        procedure :: clear => clear
    end type timed_permute_app

    interface timed_permute_app
        module procedure constructor
    end interface timed_permute_app

contains
    function constructor() result(this)
        type(timed_permute_app) :: this

        call this%clear()
    end function constructor

    subroutine initialize(this, config)
        class(timed_permute_app), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(string), dimension(2) :: priorities
        type(tensor_datatype_helper) :: helper
        type(timer) :: atimer
        type(string_converter) :: converter

        atimer = timer()
        priorities = [string("timed_permute-"), string("permute-")]

        call this%app%check_config(config, priorities)
        call this%initialize_metrics()

        call this%app%setup_permute(config, priorities)

        this%datatype_size = helper%get_datatype_size(options=config%full_config, priorities=priorities)
        this%sizes = converter%to_int64_array(config%full_config%get_value("dimensions", priorities))

        call atimer%startit()
        call this%app%allocate_and_setup_data(config, priorities)
        call atimer%stopit()
        call this%initial_allocation_time%add(atimer%current_time)

        call this%allocated_gibibytes%add(product(real(this%sizes, real64))*2*this%datatype_size/1024/1024/1024)
    end subroutine initialize

    subroutine initialize_metrics(this)
        class(timed_permute_app), intent(inout) :: this

        ! Set to rate as they are only performed once.
        this%allocated_gibibytes = measurement(string("bytes_allocated"), string("GiB"), rate=.true.)
        this%initial_allocation_time = measurement(string("allocation_time"), string("s"), rate=.true.)

        this%gibibytes_read = measurement(string("bytes_read"), string("GiB"))
        this%gibibytes_written = measurement(string("bytes_written"), string("GiB"))
        this%permute_time = measurement(string("permute_time"), string("s"))
    end subroutine initialize_metrics

    subroutine run(this)
        class(timed_permute_app), intent(inout) :: this

        type(timer) :: atimer

        atimer = timer()
        call atimer%startit()
        call this%app%run()
        call atimer%stopit()
        call this%permute_time%add(atimer%current_time)

        call this%gibibytes_read%add(product(real(this%sizes, real64))*this%datatype_size/1024/1024/1024)
        call this%gibibytes_written%add(product(real(this%sizes, real64))*this%datatype_size/1024/1024/1024)
    end subroutine run

    subroutine zero_measurements(this)
        class(timed_permute_app), intent(inout) :: this

        call this%gibibytes_read%reset()
        call this%gibibytes_written%reset()
        call this%permute_time%reset()
    end subroutine zero_measurements

    function get_measurements(this) result(measurements)
        class(timed_permute_app), intent(in) :: this
        type(measurement), dimension(:), allocatable :: measurements

        type(measurement) :: allocation_bw, read_bw, write_bw

        allocation_bw = measurement(string("allocation_bandwidth"), string("GiB/s"), rate=.true.)
        read_bw = measurement(string("read_bandwidth"), string("GiB/s"), rate=.true.)
        write_bw = measurement(string("write_bandwidth"), string("GiB/s"), rate=.true.)

        call allocation_bw%add(this%allocated_gibibytes%val/this%initial_allocation_time%val)
        call read_bw%add(this%gibibytes_read%val/this%permute_time%val)
        call write_bw%add(this%gibibytes_written%val/this%permute_time%val)

        measurements = [ &
                this%allocated_gibibytes, &
                this%initial_allocation_time, &
                allocation_bw, &
                this%gibibytes_read, &
                this%gibibytes_written, &
                this%permute_time, &
                read_bw, &
                write_bw &
            ]
    end function get_measurements

    subroutine cleanup(this)
        class(timed_permute_app), intent(inout) :: this

        call this%app%cleanup()

        call this%allocated_gibibytes%cleanup()
        call this%initial_allocation_time%cleanup()
        call this%gibibytes_read%cleanup()
        call this%gibibytes_written%cleanup()
        call this%permute_time%cleanup()

        if ( allocated(this%sizes) ) deallocate(this%sizes)
    end subroutine cleanup

    subroutine clear(this)
        class(timed_permute_app), intent(inout) :: this

        this%app = permute_app()
        this%datatype_size = 0
    end subroutine clear
end module timed_permute_app_module
