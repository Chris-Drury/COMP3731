PROGRAM LargestFactorialInteger
!---
! This Program determines the largest n for which n! can be stored in a 32 bit integer
!---
IMPLICIT NONE

INTEGER :: v, n
REAL :: r, rprev

!- First, the largest possbile signed 32bit integer value is 2**31-1
v = 2**31-1
PRINT *, "The largest integer is:"
PRINT  *, v
!- Next, we'll start with n = 1, n! == 1
n = 1
r = 1
!- continue computing n! with incresing n, until we surpass v
DO WHILE (v .gt. r)
    n = n + 1
  	rprev = r
    r = r * n
END DO
!- Now notice n will increase even if r becomes greater than v, so n--
n = n -1
v = rprev
PRINT *, "The largest possible values of n and n! within the 32bit integer is:"
PRINT *, "n:", n
PRINT *, "n!:", v

END PROGRAM LargestFactorialInteger