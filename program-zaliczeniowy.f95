! Programowanie w języku Fortran 90/95
! Autor: Igor Zamojski

! ten program przyjmuje od użytkownika obliczenia do wykonania w formie infiksowej,
! następnie konwertuje tą formę na odwrotną notację polską, a następnie oblicza wynik
! zgodnie z tą notacją i wyświetla wynik na ekran.

! Przykładowe dane wejściowe poniżej:

!  IN:
!       12+2*(3*4+10/5)
! OUT:
!       12 2 3 4 * 10 5 / + * +
!       = 40

!  IN:
!       ((2+7)/3+(14-3)*4)/2
! OUT:
!       2 7 + 3 / 14 3 - 4 * + 2 /
!       = 23.5

!  IN:
!       5 + (1+2) * 4 - 3
! OUT:
!       5 1 2 + 4 * + 3 -
!       = 14

!  IN:
!       12 * 34 - 56 / 7 + 8 * 9 * 10 / 2 + 3 - 1 - 45 * 67 / 89 - 12 + 13 + 14 * 15 - 16 / 17 + 18 - 19 * 20 / 21 - 22 - 23 - 24 * 25 - 26 / 27 + 28 + 29 * 30 / 32 - 31
! OUT:
!       12 34 * 56 7 / - 8 9 * 10 * 2 / + 3 + 1 - 45 67 * 89 / - 12 - 13 + 14 15 * + 16 17 / - 18 + 19 20 * 21 / - 22 - 23 - 24 25 * - 26 27 / - 28 + 29 30 * 32 / + 31 -
!       = 316.311768

program infix_to_onp_and_evaluate
   implicit none
   character(len=256) :: infix, onp
   real :: result

   print *, 'Podaj wyrażenie w notacji zwykłej:'
   read(*, '(A)') infix

   print *, ' '
   call infix_to_onp(infix, onp)
   print *, 'ONP: ', trim(onp)

   call evaluate_onp(onp, result)
   print *, 'Wartość końcowa: ', result

contains

   subroutine infix_to_onp(infix, onp)
      ! ten podprogram przyjmuje ciąg znakowy z infiksowym zapisem obliczeń
      ! i zwraca ciąg znakowy w odwrotnej notacji polskiej przez zmienną `onp`
      character(len=*), intent(in) :: infix
      character(len=256), intent(out) :: onp
      character(len=1) :: stack(256)
      integer :: i, top

      onp = ''
      top = 0

      do i = 1, len_trim(infix)
         select case (infix(i:i))
          case ('0':'9')
            if (i > 0 .and. infix(i-1:i-1) >= '0' .and. infix(i-1:i-1) <= '9') then
               onp = trim(onp) // infix(i:i)
            else
               onp = trim(onp) // ' ' // infix(i:i)
            end if
          case ('+','-')
            do while (top > 0 .and. stack(top) /= '(')
               onp = trim(onp) // ' ' // stack(top)
               top = top - 1
            end do
            top = top + 1
            stack(top) = infix(i:i)
          case ('*','/')
            do while (top > 0 .and. (stack(top) == '*' .or. stack(top) == '/'))
               onp = trim(onp) // ' ' // stack(top)
               top = top - 1
            end do
            top = top + 1
            stack(top) = infix(i:i)
          case ('(')
            top = top + 1
            stack(top) = '('
          case (')')
            do while (top > 0 .and. stack(top) /= '(')
               onp = trim(onp) // ' ' // stack(top)
               top = top - 1
            end do
            if (top > 0 .and. stack(top) == '(') then
               top = top - 1
            end if
          case default
         end select
      end do

      do while (top > 0)
         onp = trim(onp) // ' ' // stack(top)
         top = top - 1
      end do
   end subroutine infix_to_onp

   subroutine evaluate_onp(onp, result)
      ! ten podprogram przyjmuje ciąg znakowy w odwrotnej notacji polskiej
      ! i zwraca wynik przez zmienną `result`
      character(len=*), intent(in) :: onp
      real, intent(out) :: result
      real :: stack(256)
      integer :: i, top, len_onp
      character(len=1) :: char
      integer :: num

      num = 0
      top = 0
      len_onp = len_trim(onp)
      i = 1

      do while (i <= len_onp)
         char = onp(i:i)
         select case (char)
          case ('0':'9')
            num = num * 10 + iachar(char) - iachar('0')
            if (.NOT. (i <= len_onp .and. onp(i+1:i+1) >= '0' .and. onp(i+1:i+1) <= '9')) then
               top = top + 1
               stack(top) = real(num)
               num = 0
            end if
          case ('+')
            stack(top-1) = stack(top-1) + stack(top)
            top = top - 1
          case ('-')
            stack(top-1) = stack(top-1) - stack(top)
            top = top - 1
          case ('*')
            stack(top-1) = stack(top-1) * stack(top)
            top = top - 1
          case ('/')
            stack(top-1) = stack(top-1) / stack(top)
            top = top - 1
          case default
         end select
         i = i + 1
      end do

      result = stack(1)
   end subroutine evaluate_onp

end program infix_to_onp_and_evaluate
