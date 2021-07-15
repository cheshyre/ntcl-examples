module tc_usage_module
    use :: util_api, only : &
            dictionary, &
            get_application_configuration

    use :: tensor_api, only : &
            scalar, &
            tensor, &
            allocate_and_create_tensor

    use :: algorithms_api, only : &
            tensor_contraction, &
            tensor_contraction_factory

    implicit none

    public :: run_tc_usage_examples
contains
    subroutine run_tc_usage_examples()

        call run_tc_usage_default()
        call run_tc_usage_with_options()
        call run_tc_usage_with_config()
    end subroutine run_tc_usage_examples

    subroutine run_tc_usage_default()

        class(tensor), allocatable :: a, b, c, d
        class(tensor_contraction), allocatable :: tc

        ! Allocate tensors
        call allocate_and_create_tensor(a, [10,10,10], "real64")
        call allocate_and_create_tensor(b, [10,10,10], "real64")
        call allocate_and_create_tensor(c, [10,10,10,10], "real64")
        call allocate_and_create_tensor(d, [10,10,10], "real64")

        ! Populate input tensors with data

        call tensor_contraction_factory%create(tc, &
                "c(p,q,r,s)=a(s,t,p)*b(t,q,r)")
        call tc%contract(c, a, b)
        call tc%contract(c, a, d, beta=scalar(1))

        ! Process output tensor

        ! Deallocate everything
        call tc%cleanup()
        call a%cleanup; call b%cleanup()
        call c%cleanup(); call d%cleanup()
    end subroutine run_tc_usage_default

    subroutine run_tc_usage_with_options()

        class(tensor), allocatable :: a, b, c, d
        class(tensor_contraction), allocatable :: tc
        type(dictionary) :: options

        ! Set options
        options = dictionary()
        call options%set_value("contraction_driver", "unbuffered_ttgt")
        call options%set_value("memory_type", "host")
        call options%set_value("unbuffered_ttgt-mm_driver", "intrinsic")
        call options%set_value("unbuffered_ttgt-permute_driver", "loops")

        ! Allocate tensors
        call allocate_and_create_tensor(a, [10,10,10], "real64", &
                options=options)
        call allocate_and_create_tensor(b, [10,10,10], "real64", &
                options=options)
        call allocate_and_create_tensor(c, [10,10,10,10], "real64", &
                options=options)
        call allocate_and_create_tensor(d, [10,10,10], "real64", &
                options=options)

        ! Populate input tensors with data

        call tensor_contraction_factory%create(tc, &
                "c(p,q,r,s)=a(s,t,p)*b(t,q,r)", options=options)

        call tc%contract(c, a, b, alpha=scalar(1.2))
        call tc%contract(c, a, d, alpha=scalar(1.2), beta=scalar(1))

        ! Process output tensor

        ! Deallocate everything
        call options%cleanup()
        call tc%cleanup()
        call a%cleanup; call b%cleanup()
        call c%cleanup(); call d%cleanup()
    end subroutine run_tc_usage_with_options

    subroutine run_tc_usage_with_config()

        class(tensor), allocatable :: a, b, c, d
        class(tensor_contraction), allocatable :: tc
        type(dictionary) :: options

        ! Get options
        options = get_application_configuration()

        ! Allocate tensors
        call allocate_and_create_tensor(a, [10,10,10], "real64", &
                options=options)
        call allocate_and_create_tensor(b, [10,10,10], "real64", &
                options=options)
        call allocate_and_create_tensor(c, [10,10,10,10], "real64", &
                options=options)
        call allocate_and_create_tensor(d, [10,10,10], "real64", &
                options=options)

        ! Populate input tensors with data

        call tensor_contraction_factory%create(tc, &
                "c(p,q,r,s)=a(s,t,p)*b(t,q,r)", options=options)

        call tc%contract(c, a, b, alpha=scalar(1.4), beta=scalar(0))
        call tc%contract(c, a, d, beta=scalar(1))

        ! Process output tensor

        ! Deallocate everything
        call options%cleanup()
        call tc%cleanup()
        call a%cleanup; call b%cleanup()
        call c%cleanup(); call d%cleanup()
    end subroutine run_tc_usage_with_config
end module tc_usage_module
