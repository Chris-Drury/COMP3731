PROGRAM A3Q2
!--
!Finds values of x where f(x)(x^2-2x-1) = 0 using the secant method. 
!--
IMPLICIT NONE
REAL :: a, b, eps, root
INTEGER :: limit, iter

eps = .001
limit = 50
iter = 0
PRINT *, 'Attempts to solve f(x) = x^2-2x-1 for f(x)=0 using the secant method.'
PRINT *, 'Precision:', eps, 'loop limit:', limit
DO
	PRINT *, 'Enter your values for a and b. (enter 0 0 to stop)'
    READ *, a, b
    IF (a .eq. 0.0 .and. b .eq. 0.0) STOP
    root = Secant(a, b, eps, limit, iter)
    PRINT *, 'root:', root, 'iterations:', iter
END DO

CONTAINS
!- Secant method
REAL FUNCTION Secant(a, b, eps, limit, iter)
	REAL, INTENT(IN) :: a, b, eps
    INTEGER, INTENT(IN) :: limit
    INTEGER, INTENT(OUT) :: iter
    REAL :: x, x0, x1, fraction
    
    fraction = 0
    iter = 0
    x = 0
	x0 = a
    x1 = b
    fraction = fun(x1) / (fun(x1) - fun(x0))
    DO WHILE (abs(x0-x1) .gt. eps .and. iter .lt. limit)
    	x = x1 - (x1 - x0) * fraction
        x0 = x1
        x1 = x
        fraction = fun(x1) / (fun(x1) - fun(x0))
        iter = iter + 1
        ! Print additional information
        ! PRINT *, x, x0, x1, iter, abs(x0-x1)
    END DO
    Secant = x

END FUNCTION Secant
!- fun(x) = x^2-2x-1
REAL FUNCTION fun(x)
    REAL, INTENT(IN) :: x
    
	fun = (x - 2) * x - 1
    
END FUNCTION fun
END PROGRAM A3Q2