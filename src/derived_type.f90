module shape_mod
!! To demonstrate the use of extends in derived types.
private
public :: shape, rectangle, square, initialize, isFilled
type shape
    private
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
contains
    private
    procedure, pass :: initialize
    procedure, public :: isFilled
    procedure, public :: print
end type shape

type, extends(shape) :: rectangle
    integer :: length
    integer :: width
end type rectangle

type, extends(rectangle) :: square
end type square

interface shape
    procedure constructor_shape
end interface shape

interface rectangle
    procedure constructor_rectanle
end interface rectangle

interface square
    procedure constructor_square
end interface square
contains

function constructor_shape(color, filled, x, y)
    type(shape) :: constructor_shape
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
    call constructor_shape%initialize(color, filled, x, y)
end function constructor_shape

function constructor_rectanle(color, filled, x, y, length, width)
    type(rectangle) :: constructor_rectanle
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
    integer, optional :: length
    integer, optional :: width
    call constructor_rectanle%initialize(color, filled, x, y, length, width)
end function constructor_rectanle

function constructor_square(color, filled, x, y, length, width)
    type(square) :: constructor_square
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
    integer, optional :: length
    integer, optional :: width
    call constructor_square%initialize(color, filled, x, y, length, width)
end function constructor_square

subroutine initialize(sh, color, filled, x, y, length, width)
    ! initialize shape objects
    class(shape) :: sh
    integer :: color
    logical :: filled
    integer :: x
    integer :: y
    integer, optional :: length
    integer, optional :: width

    sh%color = color
    sh%filled = filled
    sh%x = x
    sh%y = y
    
    select type (sh)
    type is (shape)
        ! no further initialization required
    class is (rectangle)
        ! rectangle specific initializations
        if (present(length))  then
           sh%length = length
        else
           sh%length = 0
        endif
        if (present(width)) then
            sh%width = width
        else
            sh%width = 0
        endif
    class is (square)
        ! square specific initializations
        if (present(length))  then
           sh%length = length
        else
           sh%length = 0
        endif
        if (present(width)) then
            sh%width = width
        else
            sh%width = 0
        endif
        if(length /= width) then
            stop 'initialize: length and width must be equal for square!'
        end if
    class default
      ! give error for unexpected/unsupported type
         stop 'initialize: unexpected type for sh object!'
    end select
end subroutine initialize

logical function isFilled(this)
    class(shape) :: this
    isFilled = this%filled
end function isFilled

subroutine print(this)
    class(shape) :: this
    select type (this)
    type is (shape)
        print *, this%color, this%filled, this%x, this%y
    type is (rectangle)
        print *, this%color, this%filled, this%x, this%y, this%length, this%width
    class is (square)
        print *, this%color, this%filled, this%x, this%y, this%length, this%width
    class default
      ! give error for unexpected/unsupported type
         stop 'print: unexpected type for this object!'
    end select
end subroutine  print
end module shape_mod