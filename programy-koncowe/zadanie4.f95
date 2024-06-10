! Program obliczający rekurencyjnie sumę liczb naturalnych od N1 do N2.
! Program powinien zawierać:
! - procedurę rekurencyjną
! - deklaracje zmiennych z określoną precyzją (KIND)
! - sformatowany wydruk rezultatu z tekstem

program Zadanie4
   implicit none
   integer, parameter :: dp = selected_int_kind(15)
   integer(dp) :: N1, N2, suma

   print *, 'Podaj dwie liczby naturalne (N1 i N2):'
   read *, N1, N2

   suma = suma_rekurencyjna(N1, N2)
   print *, 'Suma liczb naturalnych od ', N1, ' do ', N2, ' wynosi ', suma

contains

   recursive function suma_rekurencyjna(a, b) result(s)
      integer(dp), intent(in) :: a, b
      integer(dp) :: s

      if (a > b) then
         s = 0
      else
         s = a + suma_rekurencyjna(a + 1, b)
      end if
   end function suma_rekurencyjna

end program Zadanie4
