PROGRAM A5Q2
!--
!Checks if a matrix is a block matrix and returns the sizes of blocks in the matrix
!--
IMPLICIT NONE
REAL, DIMENSION(:,:), ALLOCATABLE :: An
INTEGER, DIMENSION(:), ALLOCATABLE :: Bn
INTEGER :: i, n, check, blocknum

PRINT *, "This program reads a matrix A,determines if it is a block matrix, and if so, returns the sizes of each block."
DO 
	PRINT *, "Input the size of A (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(An(n, n), Bn(n), STAT=check)
    IF (check .ne. 0) THEN
		PRINT *, "Could not allocate the array A"
    ELSE
		PRINT *, "Please enter matrix A:"
        READ *, (An(i, 1:n), i=1,n)
        PRINT *, "Array A accepted."
        blocknum = Blocks(An, n, Bn)
        IF (blocknum .eq. -1) THEN
			PRINT *, "A is not a block matrix"
		ELSE 
			PRINT *, "number of blocks:", blocknum
            PRINT *, "block sizes are:", (Bn(1:blocknum))
		END IF
        DEALLOCATE(An, Bn)

    END IF
END DO

CONTAINS
!- returns -1 if A is not a block matrix
INTEGER FUNCTION Blocks(A,n,B)
REAL, DIMENSION(:,:), INTENT(IN) :: A
INTEGER, DIMENSION(:), INTENT(INOUT) :: B
INTEGER, INTENT(IN) :: n
INTEGER :: i, j, is, js, s, count, bi
LOGICAL :: blkmatrix

!- Innocent until proven guilty
blkmatrix = .true.
count = 0
s = 0
bi = 0
DO i = 1,n
  	!- Consider that if we're not in a block there may be a 0 on the diagonal.
    !- so if there is a 0 continue without stopping unless it should be in a block
    j = i
    ! find the first new element of a new block
  	IF (A(i, j) .ne. 0.0 .and. s.eq. 0) THEN
        s = 0
        !- this is the corner so as we count 'out' from the first element we should get the size of the block
  		DO is = i, n
        	js = is
            IF (A(is, j) .ne. 0 .and. A(i, js) .ne. 0.0) THEN
				s = s + 1
            !- if one is nonzero and the other is not, then this is not symmetrical off the diagonal and therefore not a block matrix
            ELSE IF ((A(is, j) .ne. 0 .or. A(i, js) .ne. 0.0).and.( A(is, j) .eq. 0 .or. A(i, js) .eq. 0.0)) THEN
              	blkmatrix = .false.
            END IF
        END DO
		!- Now we have the supposed size of the block matrix 
        !- Check that the matrix matches that size
        count = s
        is = i + 1
        !- go down the digonal until we know we're outside of the block (block's length = s)
        DO is = (i+1), (i+s-1)
			js = is
            IF (js .le. n .and. is .le. n) THEN
				IF ((A(is, j) .ne. 0.0 .or. A(i, js) .ne. 0.0).and.( A(is, j) .eq. 0 .or. A(i, js) .eq. 0.0)) THEN
            		!- again, if one is nonzero and the other is not, then this is not symmetrical off the diagonal and therefore not a block matrix
					blkmatrix = .false.
                END IF
            END IF
        END DO
        !- when we leave the loop we need to realie that s is the size, but is also included in the current i
        !- therefore when we iterate through i s times to get outside of this discovered block, we'll have to decerement s once
        s = s - 1
        !- bi is our index for B but also the count of blocks to be passed back
        bi = bi + 1
		B(bi) = count
    ELSE IF (s .ne. 0) THEN
      	s = s - 1
	END IF
END DO
IF (blkmatrix .and. bi .gt. 0) THEN
  	Blocks = bi
ELSE 
  	Blocks = -1
    DO i = 1,n
		B(i) = 0
    END DO
END IF

END FUNCTION Blocks
END PROGRAM A5Q2