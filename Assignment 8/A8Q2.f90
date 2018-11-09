PROGRAM A8Q2
!--
! Determines if a graph represente by a Adjancy matrix A is strongly connected 
! (if there exists a path from one node to any other node)
!--
IMPLICIT NONE
CHARACTER(80) :: Chars
INTEGER, DIMENSION(:, :), ALLOCATABLE :: A
INTEGER :: n, check, i, j, m
LOGICAL :: r
        
PRINT *, "Determines if a graph represente by a Adjancy matrix A is strongly connected"
PRINT *, "(if there exists a path from one node to any other node)"
DO
  	PRINT *, "Please enter the number of nodes, n (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(A(n,n), STAT=check)
    IF (check .ne. 0) THEN
      	PRINT *, "Allocation unsuccessful, please try again"
    ELSE
      	A = 0
		PRINT *, "Enter the graph, node after node:"
        DO i = 1, n
			READ "(A)", Chars
            READ (Chars,*) j
            m = INDEX(Chars, ':')
            DO WHILE (m .gt. 0)
				Chars(m:m) = ' '
                READ (Chars(m:),*) j
                IF (j .ne. 0) A(i,j) = 1
                m = INDEX(Chars, ',')
            END DO
        END DO
        PRINT *, "Adjancy matrix A:"
        DO i = 1,n
			PRINT "(5X, 15I5)", A(i, 1:n)
        END DO
        r = StronglyConn(A, n)
        IF (r) THEN
          	PRINT *, "This graph is strongly connected!"
        ELSE
          	PRINT *, "This graph is not strongly connected!"
        END IF
        DEALLOCATE(A)
    END IF
END DO

CONTAINS
LOGICAL FUNCTION StronglyConn(A, n)
INTEGER, DIMENSION(n, n), INTENT(IN) :: A
INTEGER, INTENT(IN) :: n
INTEGER, DIMENSION(n) :: C
INTEGER :: i, j

C = 0
!- Innocent until proven guilty!
StronglyConn = .true.
DO i = 1,n
	DO j = 1,n
    	!- We can't consider node loops, since that loop may be the only path to the node
        !- and therefore there is no path to that node from any other node.
    	IF (A(i,j) .eq. 1 .and. i .ne. j) THEN
			C(j) = 1
		END IF        	
	END DO
END DO
DO i = 1,n
	IF (C(i) .eq. 0) THEN
		PRINT "(1X, 'There is no path to node', I3)", i
        stronglyConn = .false.
    END IF
END DO

END FUNCTION StronglyConn
END PROGRAM A8Q2