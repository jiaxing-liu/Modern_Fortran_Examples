set (dderived_type_src
    dderived_type.f03
    derived_type.f03
)

set (data_polymorphism_src
    link.f90
    list.f90
    dlist.f90
)

set (dabstract_list_src
    dabstract_list.f90
    abstract_list.f90
    integer_list.f90
    link.f90
)

set (test_class_src
    test_class.f90
    derived_type.f03
)

add_executable(test_class ${test_class_src})
add_executable (dderived_type ${dderived_type_src})
add_executable (data_polymorphism ${data_polymorphism_src})
add_executable (dabstract_list ${dabstract_list_src})