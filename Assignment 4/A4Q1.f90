PROGRAM A4Q1
!--
! Finds the number of weakly increasing/decreasing sequences given an array.
!--
IMPLICIT NONE
INTEGER, DIMENSION (:), ALLOCATABLE :: a
INTEGER :: k, check
    
PRINT *, 'Finds the number of weakly increasing/decreasing sequences given the specified array.'
DO
    PRINT *, 'Enter the size of the array: k (or 0 to stop).'
    READ *, k
    IF (K .eq. 0) STOP
    IF (k .lt. 0) THEN
      	PRINT *, 'k must be a positive nonzero number. Try again.'
    ELSE
      	ALLOCATE(a(k), STAT=check)
        IF (check .ne. 0) THEN
          	PRINT "(1X, 'Allocation unseccessful for an array of size:', I3)", k
        ELSE
			PRINT "(1X, 'enter the values of all', I3, ' elements')", k
        	READ *, a
        	PRINT "(1X, 'accepted: ', 15I5)", a
            PRINT *, 'result: ', CountSeq(a, k)
            DEALLOCATE(a)
        END IF
    END IF
END DO

CONTAINS
INTEGER FUNCTION CountSeq(a,k)
    INTEGER, DIMENSION (1:k), INTENT(IN) :: a 
	INTEGER, INTENT(IN) :: k
    INTEGER :: c, i, direction
    ! direction:
    ! 1 => increasing
    ! -1 => decreasing
    
    direction = 0
    c = 0
    IF (k .eq. 1) THEN
      	CountSeq = 0
	ELSE 
		DO i = 1,(k - 1)
    		IF (a(i) .lt. a(i+1) .and. direction .lt. 1) THEN
      			direction = 1
      			c = c + 1
    		ELSE IF (a(i) .gt. a(i+1) .and. direction .gt. -1) THEN
      			direction = -1
       			c = c + 1
    		END IF
    	END DO 
    END IF 
	CountSeq = c	

END FUNCTION CountSeq
END PROGRAM A4Q1