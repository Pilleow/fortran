! Proszę utworzyć edytorem tekstu plik zawierający 15 liczb naturalnych. Wczytać te
! liczby do programu, policzyć osobno sumę liczb parzystych i nieparzystych dla
! dowolnego zestawu wczytanych liczb.

program Zadanie1
    implicit none

    integer, parameter :: n = 15
    integer :: i, sum_even, sum_odd
    integer :: numbers(n)
    
    sum_even = 0
    sum_odd = 0
    
    ! odczyt liczb z pliku
    open(unit=10, file='zadanie1data.txt', status='old', action='read')
    do i = 1, n
        read(10, *) numbers(i)
    end do
    close(10)
    
    ! obliczanie sum
    do i = 1, n
        if (mod(numbers(i), 2) == 0) then
            sum_even = sum_even + numbers(i)
        else
            sum_odd = sum_odd + numbers(i)
        end if
    end do
    
    ! wyświetlanie wyniku
    print *, 'Suma liczb parzystych: ', sum_even
    print *, 'Suma liczb nieparzystych: ', sum_odd

end program Zadanie1
