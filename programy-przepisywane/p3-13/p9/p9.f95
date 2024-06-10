PROGRAM P9
    IMPLICIT NONE
    INTERFACE
        REAL FUNCTION CUBE_ROOT(X)
        END FUNCTION CUBE_ROOT
    END INTERFACE
    REAL :: A,B
    PRINT *, "TYPE A NUMBER:"
    READ *, A
    B = CUBE_ROOT(A)
    PRINT *, "CUBE ROOT OF ", A, " IS ", B
    STOP
END PROGRAM P9

REAL FUNCTION CUBE_ROOT(X)
    IMPLICIT NONE
    REAL :: X, LOG_X
    LOG_X = LOG(X)
    CUBE_ROOT = EXP(LOG_X / 3.0)
END FUNCTION CUBE_ROOT