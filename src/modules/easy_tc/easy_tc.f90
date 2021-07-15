module easy_tc_module
    use :: util_api, only : dictionary

    use :: algorithms_api, only : contract

    implicit none
    private

    public :: run_easy_tc

contains
    subroutine run_easy_tc()

        call run_real_mm_default()
        call run_complex_mm_default()
        call run_real_rank3_default()

        call run_real_rank3_with_options()
    end subroutine run_easy_tc

    subroutine run_real_mm_default()

        real, dimension(10,10) :: a, b, c

        call populate_real_matrix(a); call populate_real_matrix(b)
        call contract(c, a, b, "c(m,n)=a(m,k)*b(k,n)")
    end subroutine run_real_mm_default

    subroutine run_complex_mm_default()

        complex, dimension(10,10) :: a, b, c

        call populate_complex_matrix(a); call populate_complex_matrix(b)
        call contract(c, a, b, "c(m,n)=a(m,k)*b(k,n)")
    end subroutine run_complex_mm_default

    subroutine run_real_rank3_default()

        real, dimension(10,10) :: c
        real, dimension(10,10,10) :: a, b

        call populate_real_rank3(a); call populate_real_rank3(b)
        call contract(c, a, b, "c(p,q)=a(p,r,s)*b(q,s,r)")
    end subroutine run_real_rank3_default

    subroutine run_real_rank3_with_options()

        real, dimension(10,10) :: c
        real, dimension(10,10,10) :: a, b
        type(dictionary) :: options

        options = dictionary()
        call options%set_value("contraction_driver", "unbuffered_ttgt")
        call options%set_value("unbuffered_ttgt-mm_driver", "intrinsic")
        call options%set_value("unbuffered_ttgt-permute_driver", "loops")

        call populate_real_rank3(a); call populate_real_rank3(b)
        call contract(c, a, b, "c(p,q)=a(p,r,s)*b(q,s,r)", options=options)
    end subroutine run_real_rank3_with_options

    subroutine populate_real_matrix(a)
        real, dimension(:,:), intent(inout) :: a

        call random_number(a)
    end subroutine populate_real_matrix

    subroutine populate_real_rank3(a)
        real, dimension(:,:,:), intent(inout) :: a

        call random_number(a)
    end subroutine populate_real_rank3

    subroutine populate_complex_matrix(a)
        complex, dimension(:,:), intent(inout) :: a

        real, dimension(:,:), allocatable :: a_real, a_imag

        allocate(a_real(size(a,1), size(a,2)))
        allocate(a_imag(size(a,1), size(a,2)))

        call random_number(a_real)
        call random_number(a_imag)
        a = cmplx(a_real, a_imag)
    end subroutine populate_complex_matrix
end module easy_tc_module
