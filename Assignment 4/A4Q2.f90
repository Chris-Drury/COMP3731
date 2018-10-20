PROGRAM A4Q2
!--
!	Finds the number of non-negative number groups ('blobs') in the given 2D array.
!--
IMPLICIT NONE
INTEGER, DIMENSION(:,:), ALLOCATABLE :: A
INTEGER :: m, n, check
INTEGER :: i,j, bs

PRINT *, "Finds the number of non-negative number groups ('blobs') in the given 2D array."
DO
	PRINT *, 'Enter the dimensions m, n of the array. (enter a 0 for either value to stop)'
    READ *, m, n
    IF (m .eq. 0 .or. n .eq. 0) STOP
    ALLOCATE(A(m,n), STAT=check)
    IF (check .ne. 0) THEN
      	PRINT "(1X, 'Allocation unseccessful for an array of size:', I3, I3)", m, n
    ELSE 
		PRINT "(1X, 'Enter the values for the array A with dimenstions: ', I3, I3)", m, n
        READ *, ((A(i, j),i=1,m),j=1,n)
		DO i = 1,m
			PRINT "(5X, 15I5)", A(i, 1:n)
        END DO
        PRINT "(1X, I5, ' blobs for the Array:' )", CountBlobs(A, m, n)
        DO i = 1,m
			PRINT "(5X, 15I5)", A(i, 1:n)
        END DO
		DEALLOCATE(A)
    END IF
END DO

CONTAINS
!-- Determine the # of 'blobs' in A
INTEGER FUNCTION CountBlobs(A, m, n)
	INTEGER, DIMENSION(1:m, 1:n), INTENT(INOUT) :: A
	INTEGER, INTENT(IN) :: m, n 
    INTEGER :: c, i, j, p
    
    c = 0
	DO WHILE (ContainsPos(A, m, n))
		DO i = 1,m
			DO j = 1,n
            	IF (A(i, j) .gt. 0) THEN
                	! We've found a blob, lets see how big it is...
                	c = c + 1
                    A(i, j) = -c
                    CALL FollowBlob(A, m, n, c)
                END IF
            END DO
        END DO
    END WHILE
	CountBlobs = c
    
END FUNCTION CountBlobs
!-- Switch as many adjacent non-negative positive numbers as possible
SUBROUTINE FollowBlob(A, m, n, c)
	INTEGER, DIMENSION(1:m, 1:n), INTENT(INOUT) :: A
    INTEGER, INTENT(IN) :: m, n, c
    INTEGER :: p, q

    DO p = 1,m
      	DO q = 1,n
        	IF (A(p, q) .eq. -c)THEN
  				IF (p+1 .le. m .and. A(p+1, q) .gt. 0) THEN
                	A(p+1, q) = -c
                END IF
                IF (p-1 .gt. 0 .and. A(p-1, q) .gt. 0) THEN
                  	A(p-1, q) = -c
                END IF
                IF (q+1 .le. n .and. A(p, q+1) .gt. 0) THEN
                  	A(p, q+1) = -c
                END IF
                IF (q-1 .gt. 0 .and. A(p, q-1) .gt. 0) THEN
                  	A(p, q-1) = -c
                END IF
			END IF
        END DO
    END DO


END SUBROUTINE FollowBlob
!-- Does the array A still have positive numbers?
LOGICAL FUNCTION ContainsPos(A, m, n)
	INTEGER, DIMENSION(1:m, 1:n), INTENT(IN) :: A
	INTEGER, INTENT(IN) :: m, n
    INTEGER :: i, j
    LOGICAL :: found
    
    found = .false.
    DO i = 1,m
      	DO j = 1,n
			if (A(i,j) .gt. 0) THEN
            	found = .true.
            END IF
        END DO
    END DO
    ContainsPos = found
    
END FUNCTION ContainsPos
END PROGRAM A4Q2