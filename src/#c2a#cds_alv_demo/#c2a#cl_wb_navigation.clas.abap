CLASS /c2a/cl_wb_navigation DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS display_interface
      IMPORTING i_interface_name TYPE seoitfname.

    METHODS display_class
      IMPORTING i_class_name TYPE seoclsname.

    METHODS display_method
      IMPORTING i_class_name  TYPE seoclsname
                i_method_name TYPE seomtdname.
ENDCLASS.


CLASS /c2a/cl_wb_navigation IMPLEMENTATION.
  METHOD display_interface.
    DATA(object_name) = CONV seu_objkey( i_interface_name ).

    DATA(wb_request_set) = VALUE swbm_wb_request_set( ( NEW
                                                        cl_wb_request( p_object_type = swbm_c_type_interface
                                                                       p_object_name = object_name
                                                                       p_operation   = swbm_c_op_display ) ) ).

    cl_wb_startup=>start( EXPORTING p_wb_request_set = wb_request_set
                          IMPORTING p_wb_error       = DATA(wb_error) ).

    IF wb_error IS NOT INITIAL.
      MESSAGE ID wb_error-msgid TYPE wb_error-msgty NUMBER wb_error-msgno
              WITH wb_error-msgv1 wb_error-msgv2 wb_error-msgv3 wb_error-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD display_class.
    DATA(object_name) = CONV seu_objkey( i_class_name ).

    DATA(wb_request_set) = VALUE swbm_wb_request_set( ( NEW
                                                        cl_wb_request( p_object_type = swbm_c_type_class
                                                                       p_object_name = object_name
                                                                       p_operation   = swbm_c_op_display ) ) ).

    cl_wb_startup=>start( EXPORTING p_wb_request_set = wb_request_set
                          IMPORTING p_wb_error       = DATA(wb_error) ).

    IF wb_error IS NOT INITIAL.
      MESSAGE ID wb_error-msgid TYPE wb_error-msgty NUMBER wb_error-msgno
              WITH wb_error-msgv1 wb_error-msgv2 wb_error-msgv3 wb_error-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD display_method.
    DATA(object_name) = CONV seu_objkey( |{ i_class_name WIDTH = 30 }{ i_method_name }| ).

    DATA(wb_request_set) = VALUE swbm_wb_request_set( ( NEW
                                                        cl_wb_request( p_object_type = swbm_c_type_cls_mtd_impl
                                                                       p_object_name = object_name
                                                                       p_operation   = swbm_c_op_display ) ) ).

    cl_wb_startup=>start( EXPORTING p_wb_request_set = wb_request_set
                          IMPORTING p_wb_error       = DATA(wb_error) ).

    IF wb_error IS NOT INITIAL.
      MESSAGE ID wb_error-msgid TYPE wb_error-msgty NUMBER wb_error-msgno
              WITH wb_error-msgv1 wb_error-msgv2 wb_error-msgv3 wb_error-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
