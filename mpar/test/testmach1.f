      program testmach1

      double precision d1mach
      external d1mach

      print *,'Double precision constants'
      print *,'smallest positive magnitude',d1mach(1)
      print *,'largest magnitude          ',d1mach(2)
      print *,'smallest relative spacing  ',d1mach(3)
      print *,'largest relative spacing   ',d1mach(4)
      print *,'log(b)                     ',d1mach(5)
      end
