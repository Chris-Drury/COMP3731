PROGRAM A8Q1
!---
!Determines the GCD of ints a and b given that a > b and a != 0. This program uses recusrion 
!---
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE :: MandNs, results
INTEGER :: a, b, n, check, i

PRINT *, "This program determines the GCD of each two elements in a sequence, in order"
DO
  	PRINT *, "Enter the size of the integer sequence (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(MandNs(n), results(n-1), STAT=check)
    IF (check .ne. 0) THEN
      	PRINT *, "Array allocation unsuccessful, please try again"
	ELSE
    	PRINT *, "Please enter the integer sequence:"
		READ *, MandNs
        PRINT *, "GCDs: "
        DO i = 1,n-1
          	a = MandNs(i)
            b = MandNs(i + 1)
            IF (a .eq. 0 .and. b .eq. 0) STOP
    		IF (a .eq. 0) THEN
      			results(i) = b
   			ELSE IF (b .eq. 0) THEN
      			results(i) = a
    		ELSE
      			results(i) = GCD(a,b)
    		END IF
            PRINT *, results(i)
        END DO
	END IF
    DEALLOCATE(MandNs, results)
END DO

CONTAINS
INTEGER RECURSIVE FUNCTION GCD(m,n) RESULT(g)
	INTEGER, INTENT(IN) :: m, n
    
	IF (n .gt. m) THEN
    	g = GCD(n, m)
    ELSE IF (n .eq. 0) THEN
      	g = m
    ELSE IF (m .gt. n .and. n .gt. 0) THEN
      	g = GCD(n, mod(m,n))
	END IF

END FUNCTION GCD
END PROGRAM A8Q1