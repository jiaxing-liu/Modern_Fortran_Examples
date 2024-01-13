program test_class
    use shape_mod
    implicit none
    type(rectangle) :: r
    interface
      subroutine init(sh)
        use shape_mod
        class(shape) :: sh
      end subroutine init
    end interface

    call init(r)
end program test_class

subroutine init(sh)
    use shape_mod
    class(shape) :: sh
    class(shape), allocatable :: als
    
    select type (sh)
    type is (shape)
      print *, "shape"
      allocate(shape::als)
      select type(als)
      type is (shape)
        als = sh   ! copy sh
      end select
    type is (rectangle)
      print *, "rectangle"
      allocate(rectangle::als)
       select type (als)
       type is (rectangle)
         als = sh ! copy sh
       end select
    type is (square)
      print *, "square"
      allocate(square::als)
      select type (als)
      type is (square)
        als = sh   ! copy sh
      end select
    end select
  end subroutine init