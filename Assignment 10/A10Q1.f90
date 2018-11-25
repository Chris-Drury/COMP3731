PROGRAM A10Q1
!--
! A program to find two partitions of a set that add to the closest possible values.
! The original question says "a partitions" We'll assume this means just one partition 
! (two subsets that, in union, realize the original subset).
!--
IMPLICIT NONE
INTEGER, DIMENSION(:), ALLOCATABLE :: S
INTEGER :: n, check, h

PRINT *, "This program finds a partition of a given set A, that both add to the closest possible values"
DO
	PRINT *, "Enter the length of the set, n (0 to stop)"
    READ *, n
    IF (n .eq. 0) STOP
    ALLOCATE(S(n), STAT=check)
    IF (check .eq. 0) THEN
   		PRINT *, "Enter the values for the set S:"
		READ *, S  
        !- Finding half the sum will allow us to find the optimal partition 
        h = SUM(S)/2
        CALL Partition(S, n, h)
		DEALLOCATE(S)
	ELSE
    	PRINT *, "Allocation unsuccessfult, please try again"
    END IF
END DO

CONTAINS
SUBROUTINE Partition(S, n, h)
INTEGER, DIMENSION(n), INTENT(IN) :: S
INTEGER, INTENT(IN) :: n, h
INTEGER, DIMENSION(n) :: S0, S1, Stmp0, Stmp1
INTEGER :: i, j, k, tot, ktot, diff, difftmp

S0 = 0
S1 = 0
tot = 0
diff = h
DO k=1,2**n-1
	Stmp0 = 0
    Stmp1 = S
	ktot = 0
    difftmp = 0
    j = k
    i = 1
    DO WHILE (j .ne. 0 .and. ktot .le. h)
    	IF(MOD(j,2) .eq. 1) THEN
			ktot = ktot + S(i)
            Stmp0(i) = S(i)
            Stmp1(i) = 0
        END IF
		j = j/2
        i = i+1
    END DO
	difftmp = h - ktot
    IF (difftmp .lt. diff .and. difftmp .ge. 0) THEN
		S0 = Stmp0
        S1 = Stmp1
        tot = ktot
    END IF
END DO
PRINT *, "The Result:"
PRINT '("S0: ", 15I5)', PACK(S0, S0/=0)
PRINT '("S1: ", 15I5)', PACK(S1, S1/=0)
PRINT '("SUMS(S0, S1): ", I5, I5)', tot, SUM(S1)

END SUBROUTINE Partition
END PROGRAM A10Q1