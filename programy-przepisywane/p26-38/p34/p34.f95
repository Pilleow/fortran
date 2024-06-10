
PROGRAM p34
    IMPLICIT NONE
    REAL:: a, b, c
    INTERFACE
       FUNCTION mean(x,y)
          IMPLICIT NONE
          REAL, INTENT(IN):: x,y
          REAL:: mean
       END FUNCTION mean
    END INTERFACE
    a=50 ; b=80 ; c=mean(a,b)
    WRITE (*,*) "mean=", c
    STOP
 END program p34
 
 FUNCTION mean(x,y)
    IMPLICIT NONE
    REAL, INTENT(IN):: x,y
    REAL:: mean
    mean=(x+y)/2
    RETURN
 END FUNCTION mean
 