program Zadanie5
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp) :: kwota, stopa_procentowa
    integer :: lata, i
  
    kwota = 1000.0_dp
    stopa_procentowa = 0.02_dp
    lata = 30
  
    do i = 1, lata
      kwota = kwota + kwota * stopa_procentowa
    end do
  
    print *, 'Kwota końcowa lokaty po ', lata, ' latach wynosi: ', kwota
  
  end program Zadanie5
  