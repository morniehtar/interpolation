function interp(x, n, arg, func) result(res)
    implicit none

    real(4), dimension(*), intent(in) :: arg, func
    real(4), intent(in) :: x
    integer, intent(in) :: n

    real(4) :: res, store
    integer :: i, j

    if ((x-arg(1))<0. .or. (x-arg(n))>0.) stop

    res=0
    do i = 1, n
        store = 1
        do j = 1, n
            if (j/=i) then
                store=store*(x-arg(j))/(arg(i)-arg(j))
            end if
        end do
        res=res+(func(i)*store)
    end do
end function

program Lagrange
    implicit none

    integer :: n, i, m
    real(4), dimension(:), allocatable :: arg
    real(4), dimension(:), allocatable :: func
    real(4) :: min, max, x

    interface
        real(4) function interp(x, n, arg, func)
            real(4), dimension(*), intent(in) :: arg, func
            integer, intent(in) :: n
            real(4), intent(in) :: x
        end function interp
    end interface

    !Configuration
    n = 18  !Number of known points
    m = 100 !Points after interpolation 
    min = -1.
    max =  1.

    allocate(arg(n))
    allocate(func(n))

    !Choose n points at regular intervals of lenghth del
    do i = 1, n
        arg(i)=min+(i-1)*((max-min)/(n-1))
    end do

    !Write the values of the function on those points into an array
    do i = 1, n
        !func(i)=sin(arg(i))
        func(i)=1/(25*arg(i)**2+1)
    end do

    !Interpolation itself
    open (unit = 1, file = "output.dat")
    do i = 1, m
        x=min+(i-1)*((max-min)/(m-1))
        !write(1,*) x, interp(x, n, arg, func), sin(x)
        write(1,*) x, interp(x, n, arg, func), 1/(25*x**2+1)
    end do
    close(1)

    deallocate(arg)
    deallocate(func)
end program
