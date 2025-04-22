*&---------------------------------------------------------------------*
*& Report /c2a/cds_alv_demo
*&---------------------------------------------------------------------*
REPORT /c2a/cds_alv_demo.

TABLES: sscrfields, /c2a/cdemo.

CONSTANTS cds_view TYPE ddstrucobjname VALUE '/C2A/C_Demo'.

DATA controller TYPE REF TO /c2a/if_report_controller.
DATA message    TYPE REF TO /c2a/cx_error_message.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK sub.
    SELECT-OPTIONS so_key FOR /c2a/cdemo-DemoKey.

    PARAMETERS: p_maxrec TYPE ddshmaxrec DEFAULT 500 MODIF ID max,
                p_no_max TYPE xfeld DEFAULT abap_false USER-COMMAND switch_max,
                p_forall TYPE xfeld DEFAULT abap_false NO-DISPLAY,
                p_mem_id TYPE /c2a/memory_id NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK sub.
SELECTION-SCREEN END OF SCREEN 1001.
SELECTION-SCREEN INCLUDE BLOCKS sub.
PARAMETERS p_split TYPE xfeld DEFAULT abap_false.

LOAD-OF-PROGRAM.
  TRY.
      controller = /c2a/cl_object_factory=>get_instance( )->get_report_controller( cds_view ).
    CATCH /c2a/cx_error_message INTO message.
      MESSAGE message TYPE 'A'.
  ENDTRY.

INITIALIZATION.
  controller->initialization( ).

AT SELECTION-SCREEN OUTPUT.
  controller->at_selection_screen_output( ).

AT SELECTION-SCREEN.
  controller->at_selection_screen( sscrfields-ucomm ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_key-low.
  controller->at_value_request( EXPORTING i_sel_name = 'SO_KEY'
                                CHANGING  c_value    = so_key-low ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_key-high.
  controller->at_value_request( EXPORTING i_sel_name = 'SO_KEY'
                                CHANGING  c_value    = so_key-high ).

START-OF-SELECTION.
  controller->start_of_selection( i_forall          = p_forall
                                  i_memory_id       = p_mem_id
                                  i_in_split_screen = p_split ).
