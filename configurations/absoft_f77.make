#
# rules for compilation of .f file with f77
#

%.o : %.f
	$(FF) -c $(F77FLAGS) $<


