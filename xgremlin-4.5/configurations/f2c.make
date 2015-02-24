#
# rules for compilation of .f file with f2c and cc
#

%.o : %.c
	$(CC) -c $(CFLAGS) $<

# problem when compiling with -O6 using the Pentium GCC
chebychev.o : chebychev.c
	$(CC) -c -O $<

%.c : %.f
	f2c $(F77FLAGS) $<


