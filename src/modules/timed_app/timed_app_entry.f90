module timed_app_entry_module
    use :: util_api, only : &
            string, &
            dictionary_converter, &
            measurement, &
            measurement_writer, &
            application_config

    use :: timed_application_module, only : timed_application

    implicit none
    private

    public :: timed_app_entry

    type :: timed_app_entry
        integer :: number_of_iterations, number_of_warmup_steps
        type(string), dimension(:), allocatable :: metrics
        type(application_config) :: config
        class(timed_application), allocatable :: app
    contains
        procedure :: run => run
        procedure :: run_iterations => run_iterations
        procedure :: get_writer => get_writer
        procedure :: has_metric => has_metric
        procedure :: read_commandline => read_commandline
        procedure :: parse_arguments => parse_arguments
        procedure :: cleanup => cleanup
    end type timed_app_entry

    interface timed_app_entry
        module procedure constructor
    end interface timed_app_entry

    integer, parameter :: default_number_of_iterations = 1
contains
    function constructor(app) result(this)
        class(timed_application), intent(in) :: app
        type(timed_app_entry) :: this

        this%app = app
        call this%read_commandline()
        call this%parse_arguments(this%config)
    end function constructor

    subroutine run_iterations(this)
        class(timed_app_entry), intent(inout) :: this

        integer :: idx

        do idx = 1, this%number_of_warmup_steps
            call this%app%run()
        end do

        call this%app%zero_measurements()

        do idx = 1, this%number_of_iterations
            call this%app%run()
        end do
    end subroutine run_iterations

    function get_writer(this) result(writer)
        class(timed_app_entry), intent(in) :: this
        type(measurement_writer) :: writer

        if ( this%config%write_to_file() ) then
            writer = measurement_writer(this%config%get_output_filename())
        else
            writer = measurement_writer()
        end if
    end function get_writer

    subroutine run(this)
        class(timed_app_entry), intent(inout) :: this

        type(measurement_writer) :: writer
        type(measurement), dimension(:), allocatable :: measurements
        integer :: idx

        call this%config%dump(6)
        call this%app%initialize(this%config)

        call this%run_iterations()

        measurements = this%app%get_measurements()

        writer = this%get_writer()
        do idx = 1, size(measurements)
            if ( this%has_metric(measurements(idx)%id_string) ) then
                call writer%write(measurements(idx), this%number_of_iterations)
            end if
        end do
    end subroutine run

    subroutine cleanup(this)
        class(timed_app_entry), intent(inout) :: this

        integer :: idx

        if ( allocated(this%app) ) then
            call this%app%cleanup()
            deallocate(this%app)
        end if

        call this%config%cleanup()

        if ( allocated(this%metrics) ) then
            do idx = 1, size(this%metrics)
                call this%metrics(idx)%cleanup()
            end do
            deallocate(this%metrics)
        end if
    end subroutine cleanup

    logical function has_metric(this, metric)
        class(timed_app_entry), intent(in) :: this
        type(string), intent(in) :: metric

        integer :: idx

        has_metric = .false.
        if ( size(this%metrics) == 0 ) has_metric = .true.
        do idx = 1, size(this%metrics)
            if ( metric == this%metrics(idx) ) has_metric = .true.
        end do
    end function has_metric

    subroutine read_commandline(this)
        class(timed_app_entry), intent(inout) :: this

        call this%config%read_commandline()
    end subroutine read_commandline

    subroutine parse_arguments(this, config)
        class(timed_app_entry), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(dictionary_converter) :: converter

        this%metrics = converter%to_string_array("metrics", config%full_config, delimiter=",")
        this%number_of_warmup_steps = converter%to_int("number_of_warmup_steps", config%full_config)
        this%number_of_iterations = converter%to_int("number_of_iterations", config%full_config, &
                default_value=default_number_of_iterations)
    end subroutine parse_arguments
end module timed_app_entry_module
