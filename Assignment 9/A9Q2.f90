PROGRAM A9Q2
!--
! This program uses Dijkstra's algorithm to find the smallest route between two cities.
!--
IMPLICIT NONE
INTEGER, DIMENSION(:,:), ALLOCATABLE :: A
INTEGER :: Check, n, i, m, k, len

PRINT *, "This Program uses Dijkstra's algorithm to find the smallest path between two 'cities'"
DO
	PRINT *, "Please enter the size of the array A[1:n,1:n] as n: (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(A(n,n), STAT=Check)
    IF (Check .eq. 0) THEN
		PRINT *, "Please enter the data for A:"
        READ *, (A(i,1:n), i=1,n)
        PRINT *, "Now please enter the starting and ending points (m and k respectively)"
		READ *, m,k
		len = ShortestPath(A,n,m,k)
        IF (len .ne. -1) THEN
        	PRINT "(1X, 'The shortest path from', I5, '  to', I5, '  is: ', I5)", m, k, len
        ELSE
          	PRINT "(1X, 'There exists no path from', I5, '  to', I5)", m, k
        END IF
    	DEALLOCATE(A)
	ELSE 
    	PRINT *, "Allocation unsuccessful, please try again."
  	END IF
END DO

CONTAINS
INTEGER FUNCTION ShortestPath(A,n,m,k)
INTEGER, DIMENSION(n,n), INTENT(IN) :: A
INTEGER, INTENT(IN) :: n,m,k
LOGICAL :: different
INTEGER, DIMENSION(n) :: B, Bprev
INTEGER :: i,j,s,min

PRINT *, "Calculating... B will be printed at each step."
Bprev = 0
!- Initilize the array B with the sum of A + 1. B[m] = 0
s = SUM(A) + 1
DO i=1,n
	B(i) = s
END DO
B(m) = 0
PRINT *, B
!- now loop through and modify B until B is no longer changed
different = .true.
DO WHILE (different)
    Bprev = B
    !- For each node,
    DO i= 1,n
    	IF (i .ne. m) THEN
            !- then find the minimum value
            min = s
	    DO j=1,n
        	!- Do not consider node loops or when a node is not connected to another
		IF (j .ne. i .and. A(j,i) .ne. 0) THEN
            	    IF (min > B(j) + A(j, i)) THEN
                    	min =  B(j) + A(j, i)
                    END IF
		END IF
	    END DO
       	    !- Then update the node in B
	    B(i) = min
        END IF
    END DO
    PRINT *, B
    !- prove that B changed:
    different = .false.
    DO i = 1,n
        IF (B(i) .ne. Bprev(i))different = .true.
    END DO
END DO
IF (B(k) .ne. s) THEN
    ShortestPath = B(k)
ELSE
    ShortestPath = -1
END IF

END FUNCTION ShortestPath
END PROGRAM A9Q2
