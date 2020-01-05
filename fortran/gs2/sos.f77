      subroutine sos(A, NEQ, IBAND, ISTART, NDIM, MDIM)
      
      level 2, A
      dimension A(NDIM, MDIM)

      NB2 = (IBAND+9) / 10 * 10 - 9
          do 20 K1=1, NB2, 10
      K2 = K1 + 9
          if (K1.EQ.NB2) K2=IBAND
          do 10 I=ISTART, NEQ
            
   10 write (6, 90) I, (A(I, J), J=K1, K2)
   20 write (6, 50)        

   90 format (I5, 10E12.4)
   50 format (//)
      return
      end