PROGRAM A9Q1
!--
! Converts an int value to an array of characters.
!--
IMPLICIT NONE
CHARACTER(80) :: C
INTEGER :: i, r

PRINT *, "This program take an integer input and returns the number of decimal digits in the integer"
DO
	PRINT *, "Please enter the value of i (0 to stop)"
    READ *, i
    IF (i .eq. 0) STOP
    r = Convert(i, C, 80)
    PRINT *, "The number of decimal digits:", r
END DO

CONTAINS
INTEGER FUNCTION Convert(n,t,m)
INTEGER, INTENT(IN) :: n,m
CHARACTER(m), INTENT(INOUT) :: t
INTEGER :: i, j, s, rprev, r, f

	!- find the number of decimal digits
	j = 1
    i = 0
    DO WHILE (n/j .gt. 0)
		i = i + 1
        j = j * 10
    END DO
    Convert = i
	!- If its too large the string becomes a string of '*'
	IF (m .lt. i) THEN
		t = '*'
        RETURN
    END IF
	!- isolate each decimal digit
    rprev = 0
    r = 0
    i = 1
	DO WHILE (j .gt. 1)
    	r = n /(j/10)
    	j = j / 10
        f = r - rprev
        !- CHAR requires an ASCII value, digits are 48-57 so add 48 to f
        t(i:i) = CHAR(48 + f)
        rprev = r * 10
        i = i + 1
    END DO
    PRINT *, "The String:  ", t(1:Convert)

END FUNCTION Convert
END PROGRAM A9Q1