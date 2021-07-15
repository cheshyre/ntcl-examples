module tc_app_module
    use, intrinsic :: iso_fortran_env, only : int64

    use :: util_api, only : &
            string, &
            string_converter, &
            application_config

    use :: data_api, only : &
            stream, &
            stream_handler, &
            concurrency_factory

    use :: tensor_api, only : &
            tensor, &
            tensor_builder, &
            tensor_initializer, &
            create_tensor_builder, &
            create_tensor_initializer, &
            tensor_datatype_helper

    use :: algorithms_api, only : &
            tensor_contraction, &
            tensor_contraction_factory

    use :: algorithms_dev, only : &
            tc_descriptor, &
            tc_parser


    implicit none
    private

    public :: tc_app

    type :: tc_app
        class(tensor_contraction), allocatable :: tc
        class(tensor), allocatable :: c, a, b
        type(stream) :: my_stream
        class(stream_handler), allocatable :: handler
    contains
        procedure :: initialize => initialize
        procedure :: check_config => check_config
        procedure :: allocate_and_setup_data => allocate_and_setup_data
        procedure :: setup_tc => setup_tc
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type tc_app

    interface tc_app
        module procedure constructor_empty
        module procedure constructor
    end interface tc_app

contains
    function constructor_empty() result(this)
        type(tc_app) :: this

        call this%clear()
    end function constructor_empty

    function constructor(config) result(this)
        type(application_config), intent(in) :: config
        type(tc_app) :: this

        this = tc_app()
        call this%initialize(config)
    end function constructor

    subroutine initialize(this, config)
        class(tc_app), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(string), dimension(1) :: priorities

        priorities = [string("tc-")]

        call this%check_config(config, priorities)
        call this%setup_tc(config, priorities)
        call this%allocate_and_setup_data( config, priorities)
    end subroutine initialize

    subroutine check_config(this, config, priorities)
        class(tc_app), intent(in) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string), dimension(:), allocatable :: required
        integer :: idx
        logical :: error

        error = .false.
        required = [ &
                string("dims"), &
                string("contraction") &
            ]

        if ( .not. config%config_has_required_options(required, priorities) ) &
                error stop "tc_app::check_config:Missing options."
    end subroutine check_config

    subroutine setup_tc(this, config, priorities)
        class(tc_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string) :: contraction

        call concurrency_factory%create_default_stream_handler( &
                this%handler, options=config%full_config, priorities=priorities)
        call this%handler%create(this%my_stream)

        contraction = config%full_config%get_value("contraction", priorities)
        call tensor_contraction_factory%create(this%tc, contraction%char_array, &
                options=config%full_config, priorities=priorities)
    end subroutine setup_tc

    subroutine allocate_and_setup_data(this, config, priorities)
        class(tc_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(tensor_builder) :: builder
        class(tensor_initializer), allocatable :: initializer
        type(tc_descriptor) :: tcd
        integer :: datatype
        integer(int64), dimension(:), allocatable :: sizes
        type(tensor_datatype_helper) :: helper
        type(tc_parser) :: parser
        type(string_converter) :: converter

        tcd = parser%parse(config%full_config%get_value("contraction", priorities))
        datatype = helper%get_datatype(options=config%full_config, priorities=priorities)
        sizes = converter%to_int64_array(config%full_config%get_value("dims", priorities))

        call create_tensor_builder(builder, options=config%full_config, priorities=priorities)
        call create_tensor_initializer(initializer, "random")

        call builder%allocate_and_create(this%c, datatype, sizes(tcd%c_indices), .true.)

        call builder%allocate_and_create(this%a, datatype, sizes(tcd%a_indices), .false.)
        call initializer%initialize(this%a)
        call builder%allocate_and_create(this%b, datatype, sizes(tcd%b_indices), .false.)
        call initializer%initialize(this%b)
    end subroutine allocate_and_setup_data

    subroutine run(this)
        class(tc_app), intent(inout) :: this

        call this%tc%contract(this%c, this%a, this%b, astream=this%my_stream)
        call this%tc%synchronize(this%my_stream)
    end subroutine run

    subroutine cleanup(this)
        class(tc_app), intent(inout) :: this

        integer :: idx

        call this%handler%destroy(this%my_stream)
        deallocate(this%handler)

        if ( allocated(this%tc) ) then
            call this%tc%cleanup()
            deallocate(this%tc)
        end if

        if ( allocated(this%c) ) then
            call this%c%cleanup()
            deallocate(this%c)
        end if

        if ( allocated(this%a) ) then
            call this%a%cleanup()
            deallocate(this%a)
        end if

        if ( allocated(this%b) ) then
            call this%b%cleanup()
            deallocate(this%b)
        end if

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(tc_app), intent(inout) :: this

        this%my_stream = stream()
    end subroutine clear
end module tc_app_module
