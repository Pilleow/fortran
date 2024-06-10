MODULE zawiera_procedure_Summit
    IMPLICIT NONE
    PUBLIC::Summit
    CONTAINS
        SUBROUTINE Summit(array)
        REAL, INTENT (IN), DIMENSION (:,:) :: array
        WRITE (UNIT=*,FMT=*) "Row sums: ", SUM(array,dim=2)
        WRITE (UNIT=*,FMT=*) "Column sums: ", SUM(array,dim=1)
        WRITE (UNIT=*,FMT=*) "Total: ", SUM(array)
        RETURN; END SUBROUTINE Summit
END MODULE zawiera_procedure_Summit

PROGRAM prog39
    USE zawiera_procedure_Summit; IMPLICIT NONE
    REAL, ALLOCATABLE, DIMENSION(:,:) :: a
    INTEGER::n
    OPEN (UNIT = 1, FILE = "x.txt", STATUS = "OLD", ACTION ="READ", POSITION="REWIND")
    READ (UNIT=1, FMT=*) n
    ALLOCATE (a(n, n))
    READ (UNIT=1, FMT=*) a
    CALL Summit (a); STOP
END PROGRAM prog39

MODULE moje_procedury_i_funkcje
    IMPLICIT NONE
    PUBLIC:: Input, Temp_C, Output
    CONTAINS
        SUBROUTINE Input(f_temp)
        REAL, INTENT(OUT):: f_temp
        WRITE(UNIT = *, FMT = *) "Please enter the Fahrenheit temperature: "
        READ (UNIT=*, FMT=*) f_temp
        RETURN; END SUBROUTINE Input

    FUNCTION Temp_C(f_temp) RESULT (temp_c_r)
    REAL, INTENT(IN) :: f_temp ; REAL :: temp_c_r
    REAL, PARAMETER :: t_scale = 1.8, offset = 32.0
    temp_c_r= (f_temp - offset) / t_scale
    RETURN; END FUNCTION Temp_C


    SUBROUTINE Output (f_temp, temp_c_r)
    REAL, INTENT(IN):: f_temp, temp_c_r
    WRITE(UNIT=*,FMT=*) f_temp, "degrees Fahrenheit", temp_c_r, "degrees Celsius"
    WRITE(UNIT=*,FMT=*) INT(f_temp), "degrees Fahrenheit", INT(temp_c_r), "degrees & Celsius"
    WRITE(UNIT=*,FMT=*) NINT(f_temp), "degrees Fahrenheit", NINT(temp_c_r), "degrees & Celsius"
    RETURN; END SUBROUTINE Output

END MODULE moje_procedury_i_funkcje
