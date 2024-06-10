! Program obliczający pierwiastki równania kwadratowego: ax^2 + bx + c = 0.
! Współczynnik równania a, b, c wczytujemy z klawiatury. Znak ^ to potęgowanie.

program Zadanie2
    implicit none

    real :: a, b, c
    real :: delta, root1, root2

    print *, 'Podaj współczynnik a:'
    read *, a
    print *, 'Podaj współczynnik b:'
    read *, b
    print *, 'Podaj współczynnik c:'
    read *, c

    ! obliczenie delty
    delta = b**2 - 4.0 * a * c

    ! pierwiastki i wypisanie wyniku
    if (delta > 0.0) then
        root1 = (-b + sqrt(delta)) / (2.0 * a)
        root2 = (-b - sqrt(delta)) / (2.0 * a)
        print *, 'Pierwiastki równania to: ', root1, root2
    else if (delta == 0.0) then
        root1 = -b / (2.0 * a)
        print *, 'Równanie ma jeden podwójny pierwiastek: ', root1
    else
        print *, 'Równanie ma pierwiastki zespolone.'
        print *, 'Pierwiastek 1: ', -b / (2.0 * a), ' + ', sqrt(abs(delta)) / (2.0 * a), 'i'
        print *, 'Pierwiastek 2: ', -b / (2.0 * a), ' - ', sqrt(abs(delta)) / (2.0 * a), 'i'
    end if

end program Zadanie2
