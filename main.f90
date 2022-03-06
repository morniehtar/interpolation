#define _pr 8

module config
    implicit none
    public
    !Границы отрезка интерполяции
    real(_pr), parameter :: min = 0, max = 10
    !Точность интерполяции
    integer, parameter :: intPrec=18
    !Количество выводимых точек полинома
    integer, parameter :: dots = 100
end module

program draw
    use config
    implicit none

    integer :: i
    real(_pr) :: x
    procedure(func), pointer :: fptr

    fptr => func

    open(unit = 1, file = "output.dat")
    do i = 1, dots
        x = min + (i-1)*((max-min)/(dots-1))
        write(1,*) x, lagr(fptr, x, min, max, intPrec), func(x)
    end do
    close(1)

contains
    function func(arg) result(res)
        implicit none

        real(_pr), intent(in) :: arg
        real(_pr) :: res

        res = sin(arg)
    end function func

    function lagr(fptr, arg, min, max, prec) result(res)
        implicit none

        procedure(func), pointer, intent(in) :: fptr
        real(_pr), intent(in) :: arg, min, max
        integer, intent(in) :: prec

        real(_pr) :: res, store
        real(_pr), dimension(:), allocatable :: arrg
        integer :: i, j

        if ((arg-min)<0. .or. (arg-max)>0.) stop

        allocate(arrg(prec))

        do i = 1, prec
            arrg(i) = min + (max-min)*(i-1)/(prec-1)
        end do

        res=0
        do i = 1, prec
            store = 1
            do j = 1, prec
                if (j/=i) then
                    store=store*(arg-arrg(j))/(arrg(i)-arrg(j))
                end if
            end do
            res=res+(fptr(arrg(i))*store)
        end do

        deallocate(arrg)
    end function lagr


end program draw
