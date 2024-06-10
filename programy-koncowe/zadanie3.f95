! Program obliczający liczbę samogłosek we wczytanym z pliku słowie. Program
! powinien zawierać:
! - określenie długości wczytanego łańcucha znaków
! - moduł zawierający procedurę wyznaczającą liczbę samogłosek
! - instrukcję warunkową wielowartościową CASE

module VowelCounter
    implicit none
contains
    function count_vowels(word) result(vowel_count)
        implicit none
        character(len=*), intent(in) :: word
        integer :: vowel_count
        integer :: i
        vowel_count = 0

        do i = 1, len_trim(word)
            select case (word(i:i))
                case ('a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U')
                    vowel_count = vowel_count + 1
                case default
                    ! nic nie rób
            end select
        end do
    end function count_vowels
end module VowelCounter

program Zadanie3
    use VowelCounter
    implicit none

    character(len=100) :: word
    integer :: num_vowels, word_length

    ! odczyt słowa z pliku
    open(unit=10, file='zadanie3data.txt', status='old', action='read')
    read(10, '(A)') word
    close(10)

    ! obliczenie liczby samogłosek za pomocą modułu VowelCounter
    word_length = len_trim(word)
    num_vowels = count_vowels(word)

    ! wyświetlanie wyniku
    print *, 'Wczytane słowo: ', trim(word)
    print *, 'Długość słowa: ', word_length
    print *, 'Liczba samogłosek: ', num_vowels

end program Zadanie3
