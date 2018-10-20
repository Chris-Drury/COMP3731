PROGRAM A3Q1
!--
!Reads consecutive values of x and approximates the sum of the values using the Series(x) function.
!--
IMPLICIT NONE
REAL :: x, sum
INTEGER :: count

PRINT *, 'Reads consecutive values of x and adds the values to a series.'
DO
	PRINT *, 'Enter your value of x. (0 to stop)'
    READ *, x
    IF (x .eq. 0.0) STOP
	sum = Series(x, count)
    PRINT *, 'x:', x, 'Sum:', sum, 'loop count:', count
END DO

CONTAINS
REAL FUNCTION Series(x, n)
	REAL, INTENT(IN) :: x
    INTEGER, INTENT(OUT) :: n
    REAL :: a, an, sum
    
	! Series => a = -an * x/(n+1)
    n = 1
    a = x
    an = 0.0
    sum = x
    DO WHILE (a .ne. an)
		an = a
    	n = n + 1
        a = -a * x / n
        sum = sum + a
        ! Print information on each loop.
        PRINT *, a, an, sum, n
    END DO
    Series = sum

END FUNCTION Series
END PROGRAM A3Q1