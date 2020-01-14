       subroutine BC(LX, LN, KBC, NEQ, NDIM, ISTOP)
C
C       Purpose: To Identify the boundary nodes
C
       level 2, LX
       dimension LX(NDIM), LRT(20)
C      
       NST = 0
   80      if (NST.GE.LN) go to 150
       read (5, 40) (LRT(ITT), ITT=1,20)
       IA = I
       J = LRT(I)
       NST = NST + 1
           if (J.LE.NEQ) go to 90
       write (6, 50) J
           go to 10
   90  LX(J) = KBC
  100  write (6, 60) (LRT(ITT), ITT=1, IA)
           if (IA.EQ.20) go to 80
  150      if (NST.EQ.LN) go to 10                        
       write (6, 70) NST, LN
                 ISTOP = ISTOP + 1
   40  format (20I4)
   50  format (11X, 10(1H*), 14H BOUNDARY NODE, I4, 15H, DOES NOT EXIST)
   60  format (1H0, 5(1H*), 30H NUMBER OF BOUNDARY NODES READ, I10,
      1    34H DISAGREES WITH NUMBER ANTICIPATED, I10)
   10  return   
       end