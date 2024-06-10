PROGRAM P4
    IMPLICIT NONE
    REAL :: A, B
    INTEGER :: C
    OPEN(UNIT=66, FILE="DATA_IN", STATUS="OLD", ACTION="READ", POSITION="REWIND")
    READ(UNIT=66, FMT=*) A,B,C
    WRITE(UNIT=*, FMT="(A8, F6.3, T40, A5, ES12.4, A6, I8)") &
    "a rowne", a, "b rowne", b, "c=", c
    STOP
END PROGRAM P4
