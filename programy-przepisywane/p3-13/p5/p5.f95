PROGRAM P5
    IMPLICIT NONE
    INTEGER :: I, K
    DO I = 1, 10
        DO K = 1, 12, 5
            WRITE (UNIT=*, FMT=*) I, K
        END DO
    END DO
    STOP
END PROGRAM P5