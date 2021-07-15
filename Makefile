# Auto-generated -- do not modify!

SOURCE_DIR := $(shell dirname ${MAKEFILE_LIST})
MAKEINC := ${NTCL_ROOT}/ntcl-build/makefile_fragments

include ${MAKEINC}/standard_preample.mk

modules      += timed_app btc_mod tc_mod permute_mod mm_mod
modules      += example1 timed_tc btc timed_btc easy_tc tensor_data_usage tc_usage timed_permute timed_mm

test_modules += 

library_name := 

external_include := 
external_libraries := ${NTCL_ROOT}/ntcl-algorithms/lib/libntcl-algorithms.a ${NTCL_ROOT}/ntcl-tensor/lib/libntcl-tensor.a ${NTCL_ROOT}/ntcl-data/lib/libntcl-data.a ${NTCL_ROOT}/ntcl-util/lib/libntcl-util.a
internal_include_dirs := ${NTCL_ROOT}/ntcl-algorithms/include ${NTCL_ROOT}/ntcl-tensor/include ${NTCL_ROOT}/ntcl-data/include ${NTCL_ROOT}/ntcl-util/include

ifdef use_blas
external_include += ${INCBLAS}
external_libraries += ${LIBBLAS}
endif

ifdef use_magma
external_include += -I${MAGMA_ROOT}/include ${INCBLAS}
external_libraries += -L${MAGMA_ROOT}/lib -lmagma -L${CUDA_ROOT}/lib64 -lcublas -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++ ${LIBBLAS}
endif

ifdef use_cublas
external_libraries += -L${CUDA_ROOT}/lib64 -lcublas -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif

ifdef use_cutensor
external_include += -I${CUTENSOR_ROOT}/include
external_libraries += -L${CUTENSOR_ROOT}/lib -lcutensor -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif

ifdef use_cuda
external_libraries += -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif

ifdef use_rocblas
ifeq (${HIP_PLATFORM},amd)
external_include += -I${ROCM_PATH}/rocblas/include
external_libraries += -L${ROCM_PATH}/rocblas/lib -lrocblas -ldl -L${HIP_PATH}/lib -lamdhip64 -lstdc++
endif

ifeq (${HIP_PLATFORM},nvidia)
external_include += -I${ROCM_PATH}/rocblas/include
external_libraries += -L${ROCM_PATH}/rocblas/lib -lrocblas -ldl -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif
endif

ifdef use_hip
ifeq (${HIP_PLATFORM},amd)
external_libraries += -L${HIP_PATH}/lib -lamdhip64 -lstdc++
endif

ifeq (${HIP_PLATFORM},nvidia)
external_libraries += -L${CUDA_ROOT}/lib64 -lcudart -lcuda -lstdc++
endif
endif

include ${MAKEINC}/standard_defs.mk
