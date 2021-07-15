module mm_app_module
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
            matrix, &
            tensor_builder, &
            tensor_initializer, &
            create_tensor_builder, &
            create_tensor_initializer, &
            tensor_datatype_helper

    use :: algorithms_api, only : &
            matrix_multiplication, &
            matrix_multiplication_factory

    implicit none
    private

    public :: mm_app

    type :: mm_app
        class(matrix_multiplication), allocatable :: mm
        type(matrix) :: c, a, b
        type(stream) :: my_stream
        class(stream_handler), allocatable :: handler
    contains
        procedure :: initialize => initialize
        procedure :: check_config => check_config
        procedure :: allocate_and_setup_data => allocate_and_setup_data
        procedure :: setup_mm => setup_mm
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type mm_app

    interface mm_app
        module procedure constructor_empty
        module procedure constructor
    end interface mm_app

contains
    function constructor_empty() result(this)
        type(mm_app) :: this

        call this%clear()
    end function constructor_empty

    function constructor(config) result(this)
        type(application_config), intent(in) :: config
        type(mm_app) :: this

        this = mm_app()
        call this%initialize(config)
    end function constructor

    subroutine initialize(this, config)
        class(mm_app), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(string), dimension(1) :: priorities

        priorities = [string("mm-")]

        call this%check_config(config, priorities)
        call this%setup_mm(config, priorities)
        call this%allocate_and_setup_data( config, priorities)
    end subroutine initialize

    subroutine check_config(this, config, priorities)
        class(mm_app), intent(in) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string), dimension(:), allocatable :: required
        logical :: error

        error = .false.
        required = [ &
                string("mnk") ]

        if ( .not. config%config_has_required_options(required, priorities) ) &
                error stop "mm_app::check_config:Missing options."
    end subroutine check_config

    subroutine setup_mm(this, config, priorities)
        class(mm_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string) :: contraction

        call concurrency_factory%create_default_stream_handler( &
                this%handler, options=config%full_config, priorities=priorities)
        call this%handler%create(this%my_stream)

        call matrix_multiplication_factory%create(this%mm, &
                options=config%full_config, priorities=priorities)
    end subroutine setup_mm

    subroutine allocate_and_setup_data(this, config, priorities)
        class(mm_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(tensor_builder) :: builder
        class(tensor_initializer), allocatable :: initializer
        integer :: datatype
        integer(int64), dimension(:), allocatable :: sizes
        type(tensor_datatype_helper) :: helper
        type(string_converter) :: converter

        datatype = helper%get_datatype(options=config%full_config, priorities=priorities)
        sizes = converter%to_int64_array(config%full_config%get_value("mnk", priorities))

        call create_tensor_builder(builder, options=config%full_config, priorities=priorities)
        call create_tensor_initializer(initializer, "random")

        call builder%create(this%c, datatype, sizes([1,2]), .true.)

        call builder%create(this%a, datatype, sizes([1,3]), .false.)
        call initializer%initialize(this%a)
        call builder%create(this%b, datatype, sizes([3,2]), .false.)
        call initializer%initialize(this%b)
    end subroutine allocate_and_setup_data

    subroutine run(this)
        class(mm_app), intent(inout) :: this

        call this%mm%mm(this%c, this%a, this%b, astream=this%my_stream)
        call this%mm%synchronize(this%my_stream)
    end subroutine run

    subroutine cleanup(this)
        class(mm_app), intent(inout) :: this

        integer :: idx

        call this%handler%destroy(this%my_stream)
        deallocate(this%handler)

        if ( allocated(this%mm) ) then
            call this%mm%cleanup()
            deallocate(this%mm)
        end if

        call this%c%cleanup()
        call this%a%cleanup()
        call this%b%cleanup()

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(mm_app), intent(inout) :: this

        this%my_stream = stream()
    end subroutine clear
end module mm_app_module
