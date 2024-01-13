program ddrived_type
    !! Driver for testing the drived_type module
    use shape_mod
    implicit none
    type(shape) :: shp                                  ! declare an instance of shape
    type(rectangle) :: rect                             ! declare an instance of rectangle
    type(square) :: sq                                  ! declare an instance of square

    shp = shape(1, .true., 20, 30)                      ! initialize shp
    rect = rectangle(1, .true., 20, 30, 5, 4)           ! initialize rect
    sq = square(1, .true., 20, 30, 5, 5)                 ! initialize sq
    !! Another way to initialize sq
!    call initialize(sq, 1, .true., 20, 30, 5, 4)           ! initialize sq
    call shp%print() 
    call rect%print()                                          ! print rect
    call sq%print()                                          ! print sq
end program ddrived_type