PROGRAM HeapSorter
!--
!This program uses heap sort to create a sorted array from the data contained in Q2_Data.txt
!For reading to work correctly, a space must be used to start the array of data in the file Q2_Data.txt
!Once sorted, the data is stored back into the file
!--
IMPLICIT NONE

CHARACTER*10 :: B, C
INTEGER, DIMENSION(:), ALLOCATABLE :: A, Anew
INTEGER :: s, check

!- Firstly we need to determine the size of A (the value of n)
OPEN(unit=10, file= "Data\\Q2_Data.txt")
READ(10, *) B, C, s
PRINT *, s
!- Allocate the Array to be read, and the array for the output
ALLOCATE(A(s), Anew(s), STAT=check)
IF (check .eq. 0) THEN
  	!- Read the array
  	READ(10, *) A
	PRINT *, A
    PRINT *, "Sorting..."
	Anew = HeapSort(A, s)
    PRINT *, "Done!"
    PRINT *, "HeapSorted Array:"
    PRINT *, Anew
    WRITE(10, *) "Sorted Data:"
    WRITE (10, *) Anew
    DEALLOCATE(A, Anew)
ELSE 
  	PRINT *, "The array could not be allocated, Please try again."
END IF

CONTAINS
!- HeapSort
FUNCTION HeapSort(A, s)
INTEGER, INTENT(IN) :: s
INTEGER, DIMENSION(s), INTENT(IN) :: A
INTEGER, DIMENSION(s) :: HeapSort
INTEGER :: i, n, tmp

n = s
HeapSort = A
!- Build the max heap
DO i = n, 1, -1
	CALL Heapify(HeapSort, n, i, s)
END DO
!- Now sort by the largest elements
DO i = n, 1, -1
  	tmp = HeapSort(i)
    HeapSort(i) = HeapSort(1)
    HeapSort(1) = tmp
	CALL Heapify(HeapSort, i, 1, s)
END DO

END FUNCTION HeapSort
!- Heapify
RECURSIVE SUBROUTINE Heapify(Arr, n, i, s)
INTEGER, INTENT(IN) :: n, i, s
INTEGER, DIMENSION(s), INTENT(INOUT) :: Arr
INTEGER :: l, r, max, tmp

l = 2 * i 
r = 2 * i + 1
max = i
IF (l < n .and. Arr(max) < Arr(l)) THEN
  	max = l
END IF
IF (r < n .and. Arr(max) < Arr(r)) THEN
	max = r
END IF
IF (max .ne. i) THEN
 	tmp = Arr(i)
    Arr(i) = Arr(max)
    Arr(max) = tmp

    CALL Heapify(Arr, n, max, s)
END IF

END SUBROUTINE Heapify
END PROGRAM HeapSorter