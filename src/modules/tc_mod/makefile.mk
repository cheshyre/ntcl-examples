local_src := $(wildcard $(subdirectory)/*.f90) $(wildcard $(subdirectory)/*.cu) \
    $(wildcard $(subdirectory)/*.F90) $(wildcard $(subdirectory)/*.c) \
	$(wildcard $(subdirectory)/*.h) $(wildcard $(subdirectory)/*.hip)
$(eval $(call make-library, $(shell basename $(subdirectory)).a, $(local_src)))
$(eval $(subst #,$(newline),$(shell $(finddep) $(subdirectory) | tr '\n' '#')))
