program dabstract_list
    use integer_list_mod
    implicit none
    integer i
    type(integerList) :: my_list
    integer values(10)
    
    do i=1, 10
       call my_list%add(i)
    enddo
    
    call my_list%printList()
    print *
    
    call my_list%reset()
    i = 1
    do while(my_list%moreValues())
       values(i) = my_list%current()
       call my_list%next()
       i = i + 1
    end do
    
    print *, values
  end program dabstract_list