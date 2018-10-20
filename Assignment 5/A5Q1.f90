PROGRAM A5Q1
!--
!Recieves two array inputs representing a set of intervals and returns all merged non-overlapping intervals
!--
IMPLICIT NONE
INTEGER, DIMENSION (:), ALLOCATABLE :: Xn, Yn
INTEGER :: n, check, output

output = 0
PRINT *, "Solves for all intervals that do not overlap. will merge overlapping intervals."
DO
	PRINT *, "Input the size of arrays X and Y (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(Xn(n), Yn(n), STAT=check)
    IF (check .ne. 0) THEN
    	PRINT *, "Allocation unsuccessful for X and Y. Please try again"
    ELSE
		PRINT *, "Please enter the values for X"
        READ *, Xn
        PRINT *, "Please enter the values for Y"
        READ *, Yn
        PRINT *, "Input:"
        PRINT *, "X:", Xn
        PRINT *, "Y:", Yn
        IF (n .eq. 1) THEN
          	PRINT *, 1
        ELSE
          	PRINT *, "Output: "
        	output = Merge(Xn, Yn, n)
            PRINT *, "Numbr of intervals:", output
        END IF
        DEALLOCATE(Xn, Yn)
    END IF
END DO

CONTAINS
INTEGER FUNCTION Merge(X, Y, n)
	INTEGER, DIMENSION (:), INTENT(INOUT) :: X, Y
	INTEGER, INTENT(IN) :: n
	INTEGER :: xi, yi, xj, yj, xk, yk, i, j, tmpx, tmpy
    LOGICAL :: merging
    LOGICAL :: merged
    
	merging = .true.
   	merged = .false.
	Merge = 1
    ! Isolate the different intervals
	DO WHILE (merging)    
		DO i = 1,n
        	xi = X(i)
            yi = Y(i)
            xk = X(i)
            yk = Y(i)
			DO j = 1,n
            	IF (j .ne. i)THEN
                xj = X(j)
                yj = Y(j)
                    IF (xi .le. xj .and. xj .le. yi) THEN
						xk = xi
                        yk = yj
                        IF (xi .ne. xj .and. yi .ne. yj) THEN
                        	merged = .true.
                        END IF
                        X(i) = xk
            			Y(i) = yk                       
                        X(j) = xk
            			Y(j) = yk
                    ELSE IF (xj .lt. xi .and. xi .lt. yj) THEN
						xk = xj
                        yk = yi
                        IF (xi .ne. xj .and. yi .ne. yj) THEN
                        	merged = .true.
                        END IF
                        X(i) = xk
           				Y(i) = yk
                        X(j) = xk
           				Y(j) = yk
                    END IF
                END IF
			END DO
        END DO  
		IF (.not. merged) THEN
        	merging = .false.
        ELSE
          	merged = .false.
        END IF

    END DO
    ! Order the arrays X and Y
    DO i = 1, (n - 1)
      	xi = X(i)
      	yi = Y(i)
       	IF (xi .ne. X(i +1) .and. yi .ne. Y(i + 1)) THEN
			DO j = i + 1, n
            	xj = X(j)
                yj = Y(j)
              	IF (xi .eq. X(j) .and. yi .eq. Y(j)) THEN
           			tmpx = X(i + 1)
                    tmpy = Y(i + 1)
                  	X(i + 1) = X(j)
               		Y(i + 1) = Y(j)
                    X(j) = tmpx
                    Y(j) = tmpy
                END IF
        	END DO
        END IF
    END DO
    ! Now count the number of different intervals in X0 and Y0
    ! Also print all unique pairs
    xi = X(1)
    yi = Y(1)
    PRINT "('(', I3, ' ,', I3, ' )')", xi, yi
    DO j = 2, n
        xj = X(j)
        yj = Y(j)
        IF (xi .ne. xj .and. yi .ne. yj) THEN
			Merge = Merge + 1
            PRINT "('(', I3, ' ,', I3, ' )')", xj, yj
        END IF
		xi = xj
        yi = yj
    END DO
    
END FUNCTION Merge
END PROGRAM A5Q1