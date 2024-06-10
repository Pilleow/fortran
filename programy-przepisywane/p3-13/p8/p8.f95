PROGRAM P8
    IMPLICIT NONE
    TYPE PERSON
        CHARACTER(LEN=12) :: FIRST_NAME
        CHARACTER(LEN=1) :: MIDDLE_INITIAL
        CHARACTER(LEN=12) :: LAST_NAME
        INTEGER :: AGE
        CHARACTER(LEN=1) :: SEX
        CHARACTER(LEN = 11) :: SOCIAL_SECURITY
    END TYPE PERSON
    
    TYPE(PERSON) :: JACK, JILL
    JACK = PERSON("JACK", "R", "HAGEN", 47, "M", "12346754125")
    JILL = PERSON("JILL", "M", "SMITH", 39, "F", "51985151251")
    PRINT *, JILL%LAST_NAME
    PRINT *, JACK%AGE
    IF (JACK%SEX == "F") PRINT *, JACK%FIRST_NAME, " JEST KOBIETA"
    IF (JILL%SEX == "F") PRINT *, JILL%FIRST_NAME, " JEST KOBIETA"
END PROGRAM P8