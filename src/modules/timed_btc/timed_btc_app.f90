module timed_btc_app_module
    use, intrinsic :: iso_fortran_env, only : &
            real64, &
            int64

    use :: util_api, only : &
            measurement, &
            string, &
            timer, &
            application_config

    use :: tensor_api, only : tensor_datatype_helper

    use :: algorithms_dev, only : &
            tc_descriptor, &
            tc_parser

    use :: timed_application_module, only : timed_application
    use :: btc_app_module, only : btc_app

    implicit none
    private

    public :: timed_btc_app

    type, extends(timed_application) :: timed_btc_app
        type(btc_app) :: app
        integer(int64), dimension(:,:), allocatable :: all_sizes
        integer :: datatype_size
        type(tc_descriptor) :: descriptor
        type(measurement) :: allocated_gibibytes, gibibytes_read, gibibytes_written, gigaflops
        type(measurement) :: initial_allocation_time, tc_time
    contains
        procedure :: initialize => initialize
        procedure :: run => run
        procedure :: zero_measurements => zero_measurements
        procedure :: get_measurements => get_measurements
        procedure :: cleanup => cleanup
        procedure, private :: initialize_metrics => initialize_metrics
        procedure, private :: set_allocation_metric => set_allocation_metric
        procedure, private :: add_gibi_metric => add_gibi_metric
        procedure, private :: add_flop_count_metric => add_flop_count_metric
        procedure :: clear => clear
    end type timed_btc_app

    interface timed_btc_app
        module procedure constructor
    end interface timed_btc_app

contains
    function constructor() result(this)
        type(timed_btc_app) :: this

        call this%clear()
    end function constructor

    subroutine initialize(this, config)
        class(timed_btc_app), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(string), dimension(2) :: priorities
        type(tc_parser) :: parser
        type(tensor_datatype_helper) :: helper
        type(timer) :: atimer

        atimer = timer()
        priorities = [string("timed_btc-"), string("btc-")]

        call this%app%check_config(config, priorities)
        call this%initialize_metrics()

        call this%app%setup_btc(config, priorities)

        this%datatype_size = helper%get_datatype_size(options=config%full_config, priorities=priorities)
        this%descriptor = parser%parse(config%full_config%get_value("contraction", priorities))
        this%all_sizes = this%app%read_dimensions(config, priorities)

        call atimer%startit()
        call this%app%allocate_and_setup_data(config, priorities)
        call atimer%stopit()
        call this%initial_allocation_time%add(atimer%current_time)

        call this%set_allocation_metric(config, priorities)
    end subroutine initialize

    subroutine initialize_metrics(this)
        class(timed_btc_app), intent(inout) :: this

        ! Set to rate as they are only performed once.
        this%allocated_gibibytes = measurement(string("bytes_allocated"), string("GiB"), rate=.true.)
        this%initial_allocation_time = measurement(string("allocation_time"), string("s"), rate=.true.)

        this%gibibytes_read = measurement(string("bytes_read"), string("GiB"))
        this%gibibytes_written = measurement(string("bytes_written"), string("GiB"))
        this%gigaflops = measurement(string("flops_count"), string("GFLOP"))
        this%tc_time = measurement(string("tc_time"), string("s"))
    end subroutine initialize_metrics

    subroutine set_allocation_metric(this, config, priorities)
        class(timed_btc_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        call this%add_gibi_metric(this%allocated_gibibytes, this%datatype_size, this%all_sizes, this%descriptor%c_indices)
        call this%add_gibi_metric(this%allocated_gibibytes, this%datatype_size, this%all_sizes, this%descriptor%a_indices)
        call this%add_gibi_metric(this%allocated_gibibytes, this%datatype_size, this%all_sizes, this%descriptor%b_indices)
    end subroutine set_allocation_metric

    subroutine add_gibi_metric(this, metric, datatype_size, sizes, indices)
        class(timed_btc_app), intent(in) :: this
        type(measurement), intent(inout) :: metric
        integer, intent(in) :: datatype_size
        integer(int64), dimension(:,:), intent(in) :: sizes
        integer, dimension(:), intent(in) :: indices

        integer :: idx

        do idx = 1, size(sizes, 2)
            call metric%add(product(real(sizes(indices, idx), real64))*datatype_size/1024/1024/1024)
        end do
    end subroutine add_gibi_metric

    subroutine add_flop_count_metric(this, metric, sizes)
        class(timed_btc_app), intent(in) :: this
        type(measurement), intent(inout) :: metric
        integer(int64), dimension(:,:), intent(in) :: sizes

        integer :: idx

        do idx = 1, size(sizes, 2)
            call metric%add(product(real(sizes(:, idx), real64))*2/1000/1000/1000)
        end do
    end subroutine add_flop_count_metric

    subroutine run(this)
        class(timed_btc_app), intent(inout) :: this

        type(timer) :: atimer

        atimer = timer()
        call atimer%startit()
        call this%app%run()
        call atimer%stopit()
        call this%tc_time%add(atimer%current_time)

        call this%add_gibi_metric(this%gibibytes_read, this%datatype_size, this%all_sizes, this%descriptor%a_indices)
        call this%add_gibi_metric(this%gibibytes_read, this%datatype_size, this%all_sizes, this%descriptor%b_indices)
        call this%add_gibi_metric(this%gibibytes_written, this%datatype_size, this%all_sizes, this%descriptor%c_indices)
        call this%add_flop_count_metric(this%gigaflops, this%all_sizes)
    end subroutine run

    subroutine zero_measurements(this)
        class(timed_btc_app), intent(inout) :: this

        call this%gibibytes_read%reset()
        call this%gibibytes_written%reset()
        call this%gigaflops%reset()
        call this%tc_time%reset()
    end subroutine zero_measurements

    function get_measurements(this) result(measurements)
        class(timed_btc_app), intent(in) :: this
        type(measurement), dimension(:), allocatable :: measurements

        type(measurement) :: allocation_bw, read_bw, write_bw, tc_perf

        allocation_bw = measurement(string("allocation_bandwidth"), string("GiB/s"), rate=.true.)
        read_bw = measurement(string("read_bandwidth"), string("GiB/s"), rate=.true.)
        write_bw = measurement(string("write_bandwidth"), string("GiB/s"), rate=.true.)
        tc_perf = measurement(string("flop_rate"), string("GFLOPS"), rate=.true.)

        call allocation_bw%add(this%allocated_gibibytes%val/this%initial_allocation_time%val)
        call read_bw%add(this%gibibytes_read%val/this%tc_time%val)
        call write_bw%add(this%gibibytes_written%val/this%tc_time%val)
        call tc_perf%add(this%gigaflops%val/this%tc_time%val)

        measurements = [ &
                this%allocated_gibibytes, &
                this%initial_allocation_time, &
                allocation_bw, &
                this%gibibytes_read, &
                this%gibibytes_written, &
                this%gigaflops, &
                this%tc_time, &
                read_bw, &
                write_bw, &
                tc_perf &
            ]
    end function get_measurements

    subroutine cleanup(this)
        class(timed_btc_app), intent(inout) :: this

        integer :: idx

        call this%app%cleanup()

        call this%allocated_gibibytes%cleanup()
        call this%initial_allocation_time%cleanup()
        call this%gibibytes_read%cleanup()
        call this%gibibytes_written%cleanup()
        call this%gigaflops%cleanup()
        call this%tc_time%cleanup()

        call this%descriptor%cleanup()

        if ( allocated(this%all_sizes) ) deallocate(this%all_sizes)
    end subroutine cleanup

    subroutine clear(this)
        class(timed_btc_app), intent(inout) :: this

        this%app = btc_app()
        this%datatype_size = 0
    end subroutine clear
end module timed_btc_app_module
