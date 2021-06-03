module btc_app_module
    use, intrinsic :: iso_fortran_env, only : int64

    use :: util_api, only : &
            string, &
            integer_column_data_reader, &
            application_config

    use :: data_api, only : &
            stream, &
            stream_handler, &
            concurrency_factory

    use :: tensor_api, only : &
            tensor_array_element, &
            tensor_builder, &
            tensor_initializer, &
            create_tensor_builder, &
            create_tensor_initializer, &
            tensor_datatype_helper

    use :: algorithms_api, only : &
            batched_tensor_contraction, &
            batched_tensor_contraction_factory

    use :: algorithms_dev, only : &
            tc_descriptor, &
            tc_parser


    implicit none
    private

    public :: btc_app

    type :: btc_app
        class(batched_tensor_contraction), allocatable :: tc
        type(tensor_array_element), dimension(:), allocatable :: c, a, b
        type(tensor_builder) :: builder
        class(tensor_initializer), allocatable :: initializer
        type(stream) :: my_stream
        class(stream_handler), allocatable :: handler
    contains
        procedure :: initialize => initialize
        procedure :: check_config => check_config
        procedure :: read_dimensions => read_dimensions
        procedure :: allocate_and_setup_data => allocate_and_setup_data
        procedure :: setup_elements => setup_elements
        procedure :: setup_btc => setup_btc
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type btc_app

    interface btc_app
        module procedure constructor_empty
        module procedure constructor
    end interface btc_app

contains
    function constructor_empty() result(this)
        type(btc_app) :: this

        call this%clear()
    end function constructor_empty

    function constructor(config) result(this)
        type(application_config), intent(in) :: config
        type(btc_app) :: this

        this = btc_app()
        call this%initialize(config)
    end function constructor

    subroutine initialize(this, config)
        class(btc_app), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(string), dimension(1) :: priorities

        priorities = [string("btc-")]

        call this%check_config(config, priorities)
        call this%setup_btc(config, priorities)
        call this%allocate_and_setup_data( config, priorities)
    end subroutine initialize

    subroutine check_config(this, config, priorities)
        class(btc_app), intent(in) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string), dimension(:), allocatable :: required
        integer :: idx
        logical :: error

        error = .false.
        required = [ &
                string("dims_filename"), &
                string("contraction") &
            ]

        if ( .not. config%config_has_required_options(required, priorities) ) &
                error stop "btc_app::check_config:Missing options."
    end subroutine check_config

    subroutine setup_btc(this, config, priorities)
        class(btc_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string) :: contraction

        call concurrency_factory%create_default_stream_handler( &
                this%handler, options=config%full_config, priorities=priorities)
        call this%handler%create(this%my_stream)

        contraction = config%full_config%get_value("contraction", priorities)
        call batched_tensor_contraction_factory%create(this%tc, contraction%char_array, &
                options=config%full_config, priorities=priorities)
    end subroutine setup_btc

    subroutine allocate_and_setup_data(this, config, priorities)
        class(btc_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(tc_descriptor) :: tcd
        integer :: datatype
        integer(int64), dimension(:,:), allocatable :: sizes
        integer :: number_of_tcs
        type(tensor_datatype_helper) :: helper
        type(tc_parser) :: parser

        call create_tensor_builder(this%builder, options=config%full_config, priorities=priorities)
        call create_tensor_initializer(this%initializer, "random")

        sizes = this%read_dimensions(config, priorities)
        number_of_tcs = size(sizes, 2)

        allocate(this%c(number_of_tcs))
        allocate(this%a(number_of_tcs))
        allocate(this%b(number_of_tcs))

        tcd = parser%parse(config%full_config%get_value("contraction", priorities))
        datatype = helper%get_datatype(options=config%full_config, priorities=priorities)
        call this%setup_elements(this%c, datatype, sizes, tcd%c_indices, .false.)
        call this%setup_elements(this%a, datatype, sizes, tcd%a_indices, .true.)
        call this%setup_elements(this%b, datatype, sizes, tcd%b_indices, .true.)
    end subroutine allocate_and_setup_data

    function read_dimensions(this, config, priorities) result(dims)
        class(btc_app), intent(in) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities
        integer(int64), dimension(:,:), allocatable :: dims

        type(integer_column_data_reader) :: reader

        reader = integer_column_data_reader( &
                config%full_config%get_value("dims_filename", priorities))

        dims = reader%read()
        call reader%cleanup()
    end function read_dimensions

    subroutine setup_elements(this, elements, datatype, sizes, indices, initialize_random)
        class(btc_app), intent(in) :: this
        type(tensor_array_element), dimension(:) :: elements
        integer, intent(in) :: datatype
        integer(int64), dimension(:,:), intent(in) :: sizes
        integer, dimension(:), intent(in) :: indices
        logical, intent(in) :: initialize_random

        logical :: initialize
        integer(int64), dimension(:), allocatable :: dims
        integer :: idx
        type(tensor_datatype_helper) :: helper

        initialize = .not. initialize_random

        do idx = 1, size(elements)
            dims = sizes(indices, idx)
            call this%builder%allocate_and_create(elements(idx)%element, datatype, dims, initialize)
            if ( initialize_random ) call this%initializer%initialize(elements(idx)%element)
        end do
    end subroutine setup_elements

    subroutine run(this)
        class(btc_app), intent(inout) :: this

        call this%tc%contract(this%c, this%a, this%b, astream=this%my_stream)
        call this%tc%synchronize(this%my_stream)
    end subroutine run

    subroutine cleanup(this)
        class(btc_app), intent(inout) :: this

        integer :: idx

        call this%handler%destroy(this%my_stream)
        deallocate(this%handler)

        call this%builder%cleanup()
        if ( allocated(this%initializer)) deallocate(this%initializer)

        if ( allocated(this%tc) ) then
            call this%tc%cleanup()
            deallocate(this%tc)
        end if

        if ( allocated(this%c) ) then
            do idx = 1, size(this%c)
                call this%c(idx)%cleanup()
            end do
            deallocate(this%c)
        end if

        if ( allocated(this%a) ) then
            do idx = 1, size(this%a)
                call this%a(idx)%cleanup()
            end do
            deallocate(this%a)
        end if

        if ( allocated(this%b) ) then
            do idx = 1, size(this%b)
                call this%b(idx)%cleanup()
            end do
            deallocate(this%b)
        end if

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(btc_app), intent(inout) :: this

        this%my_stream = stream()
    end subroutine clear
end module btc_app_module
