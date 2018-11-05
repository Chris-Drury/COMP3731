PROGRAM A7Q2
!--
!Program that adds two CSR matricies and returns the result, or -1 if invalid.
!--
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE :: R1, C1, V1
INTEGER, DIMENSION(:), ALLOCATABLE :: R2, C2, V2
INTEGER, DIMENSION(:), ALLOCATABLE :: R3, C3, V3
INTEGER ::n, nz1, nz2, n3, check, r, i

PRINT *, "This program adds to CSR matricies and returns the result, or a -1 if C is too small"
DO
	PRINT *, "please enter n, nz1, nz2, n3 (or 0 0 0 0 to stop)"
    READ *, n, nz1, nz2, n3
    IF (n .eq. 0 .and. nz1 .eq. 0 .and. nz2 .eq. 0 .and. n3 .eq. 0) STOP
    ALLOCATE(R1(n), R2(n), C1(nz1), V1(nz1), C2(nz2), V2(nz2), R3(n3), C3(n3), V3(n3), STAT=check)
    IF (check .ne. 0) THEN
      	PRINT *, "Allocation unseccessful, please try again."
    ELSE
      	DO i = 1, n3
    		C3(i) = 0
    		V3(i) = 0
		END DO
        DO i = 1, n
			R3(i) = 0
        END DO
    	PRINT *, "please enter R1, C1, V1:"
    	READ *, R1, C1, V1 
        PRINT *, "please enter R2, C2, V2:"
        READ *, R2, C2, V2
        r = SparseAdd(R1, C1, V1, R2, C2, V2, R3, C3, V3, n, nz1, nz2, n3)
        IF (r .eq. -1) THEN
          	PRINT *, "result was -1, C was too small to store the result!"
            PRINT *, "please try again with a bigger size for C."
        END IF
    END IF
	DEALLOCATE(R1, R2, C1, C2, V1, V2, R3, C3, V3)
END DO

CONTAINS
INTEGER FUNCTION SparseAdd(R1, C1, V1, R2, C2, V2, R3, C3, V3, n, nz1, nz2, n3)
INTEGER, DIMENSION(n), INTENT(IN) :: R1, R2
INTEGER, DIMENSION(n), INTENT(INOUT) :: R3
INTEGER, DIMENSION(nz1), INTENT(IN) :: C1, V1
INTEGER, DIMENSION(nz2), INTENT(IN) :: C2, V2
INTEGER, DIMENSION(n3), INTENT(INOUT) :: C3, V3
INTEGER, INTENT(IN):: n, nz1, nz2, n3
INTEGER, DIMENSION(n) :: Rnew
INTEGER, DIMENSION(n3) :: Rtmp
INTEGER :: idx, idx2, upper1, upper2, i, j, col1, col2, tmp, k

DO i = 1, n3
  	Rtmp(i) = 0
END DO
upper1 = 0
upper2 = 0
idx2 = 1
!- lets add the elements first
IF (n3 .lt. nz1 + nz2) THEN
  	SparseAdd = -1
    RETURN
ELSE
  	SparseAdd = 0
END IF
j = 1
i = 1
DO idx = 1,n
  	upper1 = R1(idx)
    upper2 = R2(idx)
    DO WHILE (j .le. upper1 .or. i .le. upper2)
		IF (j .le. upper1) col1 = C1(j)
        IF (i .le. upper2) col2 = C2(i)
        IF (j .ge. upper1 + 1) THEN
          	Rtmp(idx2) = idx
			C3(idx2) = col2
			V3(idx2) = V2(i)
            idx2 = idx2 + 1
			i = i + 1
        ELSE IF (i .ge. upper2 + 1) THEN
          	Rtmp(idx2) = idx
			C3(idx2) = col1
			V3(idx2) = V1(j)
            idx2 = idx2 + 1
			j = j + 1
        ELSE IF col1 .lt. col2 THEN
			Rtmp(idx2) = idx
			C3(idx2) = col1
			V3(idx2) = V1(j)
            idx2 = idx2 + 1
			j = j + 1
        ELSE IF (col1 .eq. col2) THEN
			Rtmp(idx2) = idx
			C3(idx2) = col1
			V3(idx2) = V1(j) + V2(i)
            idx2 = idx2 + 1
			j = j + 1
            i = i + 1
        ELSE IF (col1 .gt. col2) THEN
			Rtmp(idx2) = idx
			C3(idx2) = col2
			V3(idx2) = V2(i)
            idx2 = idx2 + 1
			i = i + 1
        END IF
	END DO
END DO
!- now R needs to be condensed
tmp = 1
k = 1
j = 0
DO i = 1,n3
  	j = Rtmp(i)
    IF (j .ne. tmp) THEN
    	Rnew(k) = i - 1
        k = k + 1
    END IF
    tmp = j
END DO
R3 = Rnew(1: n)
!- R is now compressed final output is CSR
PRINT "(1X, 'R:', 15I3)", R3
PRINT "(1X, 'C:', 15I3)", C3
PRINT "(1X, 'V:', 15I3)", V3

END FUNCTION SparseAdd
END PROGRAM A7Q2