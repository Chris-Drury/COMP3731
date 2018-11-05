PROGRAM A7Q1
!--
!This program converts CSR matrix format to CSC format and can also do the opposite
!--
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE :: R, C, V, Rlong, Cshort
INTEGER :: n, nz, checkR, checkCV

PRINT *, "This program converts CSR matrix format to CSC format"
DO
	PRINT*, 'Please enter n and nz (0 0 to stop)'
    READ *, n, nz
    IF (n .eq. 0 .and. nz .eq. 0) STOP
    ALLOCATE(R(n), Cshort(n), STAT=checkR)
    ALLOCATE(Rlong(nz), C(nz), V(nz), STAT=checkCV)
    IF (checkR .eq. 0 .and. checkCV .eq. 0) THEN
      	PRINT *, "Please enter the values for R"
        READ *, R
        Rlong(1:n) = R
        PRINT *, "Please enter the values for C"
        READ *, C
		PRINT *, "Please enter the values for V"
        READ *, V
        CALL CSRorCSC(Rlong,C,V,n,nz)
        !- We can't change the size of an array in a subroutine but we only want to read the nth elements of C
        Cshort = C(1:n)
        PRINT *, 'Result:'
        PRINT "(1X, 'C:', 15I3)", Cshort
        PRINT "(1X, 'R:', 15I3)", Rlong
        PRINT "(1X, 'V:', 15I3)", V
    END IF
    DEALLOCATE(R, C, V, Rlong, Cshort)

END DO

CONTAINS
SUBROUTINE CSRorCSC(R,C,V,n,nz)
INTEGER, DIMENSION(:), INTENT(INOUT) :: R,C,V 
INTEGER, INTENT(IN) :: n, nz
INTEGER, DIMENSION(nz) :: Rnew
INTEGER, DIMENSION(n) :: Cnew
INTEGER :: i, j, l, k, min_idx, tmp, check

!- Expand R
k = 0
l = 1
j = 1
DO i = 1,n
	k = R(l)
    DO WHILE (j < k + 1)
		Rnew(j) = i
        j = j + 1
    END DO
    l = l + 1
END DO
R = Rnew
!- sort R, C, and V by C (selection sort)
DO i = 1,nz
	min_idx = i
    DO j = i+1, nz
      	IF (C(j) < C(min_idx)) min_idx = j
    END DO
    ! swap C, R, and V
	tmp = C(i)
    C(i) = C(min_idx)
    C(min_idx) = tmp
    tmp = R(i)
    R(i) = R(min_idx)
    R(min_idx) = tmp
    tmp = V(i)
    V(i) = V(min_idx)
    V(min_idx) = tmp
END DO
!- Sorting is done now compress C
tmp = 1
k = 1
DO i = 1,nz
  	j = C(i)
    IF (j .ne. tmp) THEN
    	Cnew(k) = i - 1
        k = k + 1
    END IF
    tmp = j
END DO
Cnew(k) = i - 1
C(1: n) = Cnew
!- Now C is compressed, and R is expanded and reodered to follow C

END SUBROUTINE CSRorCSC
END PROGRAM A7Q1