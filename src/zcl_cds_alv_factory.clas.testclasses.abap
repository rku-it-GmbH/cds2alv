*"* use this source file for your ABAP unit test classes
CLASS ltc_unit_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: cut TYPE REF TO zif_cds_alv_factory.
    METHODS:
      setup,
      all_instantiable FOR TESTING.
ENDCLASS.


CLASS ltc_unit_tests IMPLEMENTATION.

  METHOD setup.
    cut = zcl_cds_alv_factory=>get_instance( ).
  ENDMETHOD.

  METHOD all_instantiable.
    TRY.
        DATA(interface) = cl_oo_interface=>get_instance( 'ZIF_CDS_ALV_FACTORY' ).
        LOOP AT interface->methods INTO DATA(method).
          DATA(parameter_binding_tab) = VALUE abap_parmbind_tab( ).

          IF line_exists( interface->method_parameters[ cmpname = method-cmpname sconame = 'I_CDS_VIEW' ] ).
            DATA(cds_view) = CONV ddstrucobjname( 'ZC_CDS_ALV_DEMO' ).
            INSERT VALUE #( name = 'I_CDS_VIEW' value = REF #( cds_view ) ) INTO TABLE parameter_binding_tab.
          ENDIF.

          CALL METHOD cut->(method-cmpname) PARAMETER-TABLE parameter_binding_tab.
        ENDLOOP.

      CATCH cx_class_not_existent zcx_cds_alv_message.
        cl_abap_unit_assert=>fail( msg = |{ method-cmpname }| ).
    ENDTRY.
  ENDMETHOD.



ENDCLASS.
