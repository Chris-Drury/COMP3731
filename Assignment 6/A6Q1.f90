PROGRAM A6Q1
!--
!Given an array A[1:n], returns the indices of an ordered version of array A
!--
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE :: An
INTEGER :: n, check

PRINT *, "Retruns an array R such that A[R[i]] .le. A[R[J]] for 1 .ge. i < j .le. n"
DO
	PRINT *, "Please enter the length of array A (0 to stop)"
	READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(An(n), STAT=check)
    IF (check .ne. 0) THEN
      	PRINT *, "Allocation for A unsuccessful"
    ELSE
      	PRINT *, "Please enter the values for A"
        READ *, An
        PRINT *, FindOrder(An, n)
        DEALLOCATE(An)
    END IF
END DO

CONTAINS
FUNCTION FindOrder(A, n) RESULT (R)
INTEGER, DIMENSION(n), INTENT(IN) :: A
INTEGER, INTENT(IN) :: n
INTEGER, DIMENSION(n) :: B, R
INTEGER :: i, j, maxvalue, minvalue

!- We'll be working with the array A but don't want to change it
!- so lets define that the array B is A but we can change B
B = A
R = A
!- Find the largest element of the array, 
!- then introduce a new element that is greater than the rest
maxvalue = B(1)
DO i = 1,n
  	IF (maxvalue .lt. B(i)) maxvalue = B(i)
END DO
maxvalue = maxvalue + 1
!- Sort through the array B and find the lowest value, and it's index.
!- once the min is found then replace it with our largest value found above.
minvalue = 1
DO i = 1,n
    DO j = 1,n
      	IF (B(minvalue) .gt. B(j)) minvalue = j
	END DO
    B(minvalue) = maxvalue
    R(i) = minvalue 
END DO

END FUNCTION FindOrder
END PROGRAM


