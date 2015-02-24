#
# test libMACH library
#

all : machtest_sp machtest_dp testmach1 testmach2

machtest_sp : machtest.c
	gcc -DSP -o machtest_sp machtest.c

machtest_dp : machtest.c
	gcc -DDP -o machtest_dp machtest.c

testmach1 : testmach1.f
	g77 -o testmach1 testmach1.f -L.. -lMPAR

testmach2 : testmach2.f
	g77 -o testmach2 testmach2.f -L.. -lMPAR

clean :
	rm -f machtest_sp machtest_dp testmach[12] *.o testmach[12].c
