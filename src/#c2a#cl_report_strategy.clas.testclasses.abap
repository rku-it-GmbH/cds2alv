*"* use this source file for your ABAP unit test classes
CLASS ltcl_unit_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS cds_view TYPE /c2a/cds_view_name VALUE '/C2A/C_Demo'.

    DATA factory TYPE REF TO /c2a/if_object_factory.
    DATA cut     TYPE REF TO /c2a/if_report_strategy.

    METHODS setup.
    METHODS write_source_for_demo_view FOR TESTING.
ENDCLASS.


CLASS ltcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    factory = /c2a/cl_object_factory=>get_instance( ).
  ENDMETHOD.

  METHOD write_source_for_demo_view.
    DATA: message TYPE string,
          line    TYPE i,
          word    TYPE string.

    TRY.
        cut = factory->get_generation_strategy( i_cds_view = cds_view ).
        DATA(program) = cut->write_source( ).
        SYNTAX-CHECK FOR program-source_lines MESSAGE message LINE line WORD word PROGRAM '/C2A/CDS_ALV_DEMO'.
        cl_abap_unit_assert=>assert_initial( act = message msg = message ).
      CATCH zcx_cds_alv_message INTO DATA(exception).
        cl_abap_unit_assert=>fail( msg = exception->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
