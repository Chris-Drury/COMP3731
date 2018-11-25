PROGRAM A10Q2
!--
! A program to implement the `Sieve of Eratosthenes` algorithm
!--
IMPLICIT NONE
INTEGER, DIMENSION(1:4999) :: S, Sfinal
INTEGER :: i, j, jprev, p, pnew
REAL :: size

PRINT *, "Determines the list of prime numbers from 0-5000 using the 'Sieve of Eratosthenes' method"
!- initialize the arrays
DO i = 2,5000
	S(i - 1) = i 
    Sfinal(i-1) = 0 
END DO
!- implement the algorithm
p = 2
pnew = 2
size = 5000
DO WHILE (SQRT(size) .ge. p)
	DO i= 1,4999
		IF (MOD(S(i),p) .eq. 0 .and. S(i) .ne. p) S(i) = 0
   	END DO
	DO WHILE (pnew .eq. p)
		DO i = 1,4999
			IF (S(i) .ne. 0 .and. S(i) .gt. p .and. p .eq. pnew) pnew = S(i)
        END DO
    END DO
    p = pnew
END DO
!- isolate the non zeros
j = 1
DO i=1,4999
  	IF (S(i) .ne. 0) THEN
 		Sfinal(j) = S(i)
        j = j + 1
    END IF
END DO
!- print the final result
j = 0
jprev = 1
DO i = 1,334
  	IF (j + 15 > 334) THEN
    	j = 4999
    ELSE 
  		j = j + 15
    END IF
	PRINT '(15I5)', PACK(Sfinal(jprev:j), Sfinal(jprev:j)/=0)
    jprev = j +1
END DO

END PROGRAM A10Q2