# Memory type
memory_type = device

# Matrix multiplication drivers
mm_driver = rocblas
bmm_driver = simple_loops

# Permute drivers
permute_driver = hip_async
btp_driver = simple_loops
hip_async-scratch_buffer_size = 10000
hip_async-scratch_buffer-number_of_slots = 10
hip_async-scratch_buffer-number_of_checkpoints = 5

# Tensor contraction drivers
contraction_driver = buffered_ttgt
btc_driver = simple_loops

# Scratch buffer - 100 MB, default is 10 MB
scratch_buffer_size = 100000000
scratch_buffer-number_of_slots = 100
scratch_buffer-number_of_checkpoints = 10
