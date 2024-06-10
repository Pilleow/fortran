MODULE J11M
   IMPLICIT NONE
   PUBLIC :: Calc
CONTAINS
   SUBROUTINE Calc(Z, Answer)
      REAL, INTENT(IN), DIMENSION(:) :: Z
      REAL, INTENT(OUT) :: Answer
      Answer = SUM(Z ** 2)
      RETURN
   END SUBROUTINE Calc
END MODULE J11M

PROGRAM prog47
   USE J11M
   IMPLICIT NONE
   CHARACTER(LEN = 79) :: Buffer
   REAL, DIMENSION(13) :: Z
   REAL :: Answer
   INTEGER :: N, EoF

   OPEN (UNIT = 1, FILE = "prog47.txt", STATUS = "OLD", &
      ACTION = "READ", POSITION = "REWIND")
   N = 0
   DO
      N = N + 1
      READ (UNIT = 1, FMT = "(A79)", IOSTAT = EoF) Buffer
      IF (EoF /= 0) THEN
         EXIT
      ELSE IF (Buffer(1:1) == "C") THEN
         WRITE (UNIT = *, FMT = *) Buffer(2:)
      ELSE IF (Buffer(1:1) == "'") THEN
         READ (UNIT = Buffer, FMT = "(TR1, 13F6.3)") Z
         WRITE (UNIT = *, FMT = *) Z
         CALL Calc(Z, Answer)
         WRITE (UNIT = *, FMT = *) Answer
      ELSE
         WRITE (UNIT = *, FMT = *) "Column 1 is not blank or 'C' at record number ", N
      END IF
   END DO
   STOP
END PROGRAM prog47

