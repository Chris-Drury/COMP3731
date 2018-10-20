PROGRAM A2Q2
!---
!Approximates the square root of a real value a given that a>=0.
!---
REAL :: a, sqrt, diff
diff = .001
PRINT *, 'Approximates the square root of a to a default precision of:', diff
DO
	PRINT *, 'Enter a (Enter a negative value to stop.)'
    READ *, a
    IF (a .lt. 0) STOP 
	sqrt = mysqrt(a, diff)
    PRINT *, 'a:', a, ' sqrt(a):', sqrt
END DO

CONTAINS
REAL FUNCTION mysqrt(a, diff)
	REAL :: x0, x1
    x0 = a/2
    x1 = (x0 + a/x0)/2
    PRINT *, x0, x1
    DO WHILE (abs(x0 - x1) .gt. diff)
		x0 = x1
    	x1 = (x0 + a/x0)/2
		PRINT *, x0, x1
    END DO
    mysqrt = x1

END FUNCTION mysqrt
END PROGRAM A2Q2