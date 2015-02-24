#
# rules for compilation of .f file with g77
#

%.o : %.f
	$(FF) -c $(F77FLAGS) $<


