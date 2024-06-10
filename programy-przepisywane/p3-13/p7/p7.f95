PROGRAM P7
    IMPLICIT NONE
    CHARACTER(LEN=16) :: A,B,C,D
    A = "A KINDLY GIGANT"
    B = "A SMALL MAN"
    C = B(:8)//"STEP"
    D = "FOR A"//B(8:)
    B = " "//D(:4)//B(9:11)//A(3:6)
    A=A(:2)//A(10:15)//"LEAP"
    PRINT *, "YOUR FIRST NAME"
    READ *, A
    PRINT *, "YOUR SECOND NAME"
    READ *, B
    PRINT *, A, B, "STUDENT"
    PRINT *, TRIM(A), " ", TRIM(B), "STUDENT"
END PROGRAM P7