PROGRAM prog42
    IMPLICIT NONE
    REAL :: a, b, c
    WRITE (UNIT = *, FMT = *) " please enter three numbers to be sorted."
    READ (UNIT = *, FMT = *) a, b, c
    WRITE (UNIT = *, FMT = *) " thank you. you have entered: ", a, b, c
    IF (a > b) CALL swap(a, b)
    IF (a > c) CALL swap(a, c)
    IF (b > c) CALL swap(b, c)
    WRITE (UNIT = *, FMT = *) " the numbers in increasing order are: ", a, b, c
    STOP
 CONTAINS
    SUBROUTINE swap(x, y)
       REAL, INTENT(IN OUT) :: x, y
       REAL :: aux
       aux = x
       x = y
       y = aux
       RETURN
    END SUBROUTINE swap
 END PROGRAM prog42