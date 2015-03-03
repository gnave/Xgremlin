#
# rules for compilation of .f file with lf95
#

%.o : %.f
	$(FF) -c $(F95FLAGS) $<


