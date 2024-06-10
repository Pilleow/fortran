program Zadanie6
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp) :: x, dx, f_curr, f_prev
    integer :: i, n_points
    real(dp), parameter :: x_start = -3.0_dp, x_end = 4.0_dp
  
    ! liczba punktów i krok skanowania tutaj
    n_points = 7000
    dx = (x_end - x_start) / n_points
  
    x = x_start
    f_prev = f(x)
    do i = 1, n_points
      x = x + dx
      f_curr = f(x)
  
      ! sprawdzanie zmiany znaku funkcji
      if (f_curr * f_prev <= 0.0_dp) then
        print '(A, F8.3)', 'Miejsce zerowe w przyblizeniu: x = ', x
      end if
  
      f_prev = f_curr
    end do
  
  contains
  
    ! definicja funkcji
    real(dp) function f(x)
      real(dp), intent(in) :: x
      f = x**3 - 3*x**2 - 4*x + 12
    end function f
  
  end program Zadanie6
  