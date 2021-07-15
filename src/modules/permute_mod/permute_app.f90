module permute_app_module
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
            tensor_permute, &
            tensor_permute_factory

    implicit none
    private

    public :: permute_app

    type :: permute_app
        class(tensor_permute), allocatable :: permute
        class(tensor), allocatable :: src, dst
        integer, dimension(:), allocatable :: permutation
        type(stream) :: my_stream
        class(stream_handler), allocatable :: handler
    contains
        procedure :: initialize => initialize
        procedure :: check_config => check_config
        procedure :: allocate_and_setup_data => allocate_and_setup_data
        procedure :: setup_permute => setup_permute
        procedure :: run => run
        procedure :: cleanup => cleanup
        procedure :: clear => clear
    end type permute_app

    interface permute_app
        module procedure constructor_empty
        module procedure constructor
    end interface permute_app

contains
    function constructor_empty() result(this)
        type(permute_app) :: this

        call this%clear()
    end function constructor_empty

    function constructor(config) result(this)
        type(application_config), intent(in) :: config
        type(permute_app) :: this

        this = permute_app()
        call this%initialize(config)
    end function constructor

    subroutine initialize(this, config)
        class(permute_app), intent(inout) :: this
        type(application_config), intent(in) :: config

        type(string), dimension(1) :: priorities

        priorities = [string("permute-")]

        call this%check_config(config, priorities)
        call this%setup_permute(config, priorities)
        call this%allocate_and_setup_data( config, priorities)
    end subroutine initialize

    subroutine check_config(this, config, priorities)
        class(permute_app), intent(in) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string), dimension(:), allocatable :: required
        logical :: error

        error = .false.
        required = [ &
                string("dimensions"), string("permutation") ]

        if ( .not. config%config_has_required_options(required, priorities) ) &
                error stop "permute_app::check_config:Missing options."
    end subroutine check_config

    subroutine setup_permute(this, config, priorities)
        class(permute_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(string_converter) :: converter

        call concurrency_factory%create_default_stream_handler( &
                this%handler, options=config%full_config, priorities=priorities)
        call this%handler%create(this%my_stream)

        call tensor_permute_factory%create(this%permute, &
                options=config%full_config, priorities=priorities)

        this%permutation = converter%to_int_array(config%full_config%get_value("permutation", priorities))
    end subroutine setup_permute

    subroutine allocate_and_setup_data(this, config, priorities)
        class(permute_app), intent(inout) :: this
        type(application_config), intent(in) :: config
        type(string), dimension(:), intent(in) :: priorities

        type(tensor_builder) :: builder
        class(tensor_initializer), allocatable :: initializer
        integer :: datatype
        integer(int64), dimension(:), allocatable :: sizes
        type(tensor_datatype_helper) :: helper
        type(string_converter) :: converter

        datatype = helper%get_datatype(options=config%full_config, priorities=priorities)
        sizes = converter%to_int64_array(config%full_config%get_value("dimensions", priorities))

        call create_tensor_builder(builder, options=config%full_config, priorities=priorities)
        call create_tensor_initializer(initializer, "random")

        call builder%allocate_and_create(this%dst, datatype, sizes(this%permutation), .true.)

        call builder%allocate_and_create(this%src, datatype, sizes, .false.)
        call initializer%initialize(this%src)
    end subroutine allocate_and_setup_data

    subroutine run(this)
        class(permute_app), intent(inout) :: this

        call this%permute%permute(this%src, this%dst, this%permutation, astream=this%my_stream)
        call this%permute%synchronize(this%my_stream)
    end subroutine run

    subroutine cleanup(this)
        class(permute_app), intent(inout) :: this

        call this%handler%destroy(this%my_stream)
        deallocate(this%handler)

        if ( allocated(this%permute) ) then
            call this%permute%cleanup()
            deallocate(this%permute)
        end if

        if ( allocated(this%src) ) then
            call this%src%cleanup()
            deallocate(this%src)
        end if

        if ( allocated(this%dst) ) then
            call this%dst%cleanup()
            deallocate(this%dst)
        end if

        if ( allocated(this%permutation) ) deallocate(this%permutation)

        call this%clear()
    end subroutine cleanup

    subroutine clear(this)
        class(permute_app), intent(inout) :: this

        this%my_stream = stream()
    end subroutine clear
end module permute_app_module
