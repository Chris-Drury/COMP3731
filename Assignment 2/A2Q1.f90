PROGRAM A2Q1
!---
!Determines the GCD of ints a and b given that a > b and a != 0
!---
IMPLICIT NONE
INTEGER :: a,b, gcd
DO
  	PRINT *, 'Please enter a b. (To stop enter 0 0)'
    READ *, a,b
    IF (a .eq. 0 .and. b .eq. 0) STOP
    IF (a .eq. 0) THEN
      	gcd = b
    ELSE IF (b .eq. 0) THEN
      	gcd = a
    ELSE IF (b .gt. a) THEN 
		gcd = FindGCD(b,a)
    ELSE
      	gcd = FindGCD(a,b)
    END IF
    PRINT *, 'The GCD is:', gcd
END DO

CONTAINS
REAL FUNCTION FindGCD(a,b)
	INTEGER, INTENT(INOUT) :: a,b
	INTEGER :: q,r
    r = 1
    DO WHILE (r .ne. 0)
		q = a/b
    	r = MOD(a,b)
    	PRINT *, 'quotient: ', q, ' remainder: ', r
        a = b
       	b = r
        PRINT *, a,b
    END DO
	FindGCD = a
    
END FUNCTION FindGCD
END PROGRAM