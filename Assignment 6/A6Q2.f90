PROGRAM A6Q2
!--
! Determines the next permutation of the array A.
!--
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE :: An
INTEGER :: n, check, invokenum, idx
LOGICAL :: permcheck

PRINT *, "Determines the next possible permutation of A, if there is one"
DO
	PRINT *, "Please enter the length of the array A (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(An(n), STAT=check)
    IF (check .ne. 0) THEN
      	PRINT *, "Allocation for array A was unseccessfull"
    ELSE
      	PRINT *, "Please enter the data for array A"
        READ *, An
		PRINT *, "How many permutations would you like to cycle through?"
        READ *, invokenum
        DO idx = 1, invokenum
			permcheck = NextPerm(An, n)
            PRINT *, "Permutation", idx
            IF (permcheck) THEN
              	PRINT "(1X, I2, 15I3)", An
            ELSE
              	PRINT *, "No further permutations are available"
            END IF
        END DO
        DEALLOCATE(An)
    END IF
END DO

CONTAINS
!- Do the next array shift 
LOGICAL FUNCTION NextPerm(A,n)
INTEGER, DIMENSION(n), INTENT(INOUT) :: A
INTEGER :: n
INTEGER :: x, idxX, n0, n1, tmp
LOGICAL :: possibleperm

x = A(n)
idxX = n
n1 = n
n0 = n - 1
possibleperm = .false.
DO WHILE (n0 .gt. 0)
	IF (A(n1) .gt. A(n0) .and. A(n0) .gt. x) idxX = n1
    IF (A(n0) .lt. A(n1)) THEN
      	x = A(idxX)
        tmp = A(n0)
        A(n0) = x
        A(idxX) = tmp
        possibleperm = .true.
        n0 = 0
    ELSE
      	n1 = n0
        n0 = n0 - 1
    END IF
END DO
IF (possibleperm) CALL SelectionSort(A, n1, n)
NextPerm = possibleperm

END FUNCTION NextPerm
!- Sort the required set of A
SUBROUTINE SelectionSort(A, i, j)
INTEGER, DIMENSION(j), INTENT(INOUT) :: A
INTEGER, INTENT(IN) :: i, j
INTEGER :: min_idx, h, k, tmp

h = i
DO WHILE (h .lt. j)
  	min_idx = h
    	DO k = h,j
        	IF (A(min_idx) .gt. A(k)) min_idx = k
        END DO
	tmp = A(h)
    A(h) = A(min_idx)
    A(min_idx) = tmp
	h = h + 1
END DO

END SUBROUTINE SelectionSort
END PROGRAM 