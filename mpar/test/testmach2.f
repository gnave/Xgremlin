      program testmach2

      real r1mach
      external r1mach

      print *,'Single precision constants'
      print *,'smallest positive magnitude',r1mach(1)
      print *,'largest magnitude          ',r1mach(2)
      print *,'smallest relative spacing  ',r1mach(3)
      print *,'largest relative spacing   ',r1mach(4)
      print *,'log(b)                     ',r1mach(5)
      end
