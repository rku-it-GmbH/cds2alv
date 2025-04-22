CLASS /c2a/cl_report_extension DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES /c2a/if_display_extension.
    INTERFACES /c2a/if_selection_extension.
    INTERFACES /c2a/if_report_extension.

    METHODS constructor
      IMPORTING i_extension_name       TYPE /c2a/report_extension_name
                i_extension_parameters TYPE /c2a/extension_parameter_tab.

  PROTECTED SECTION.
    ALIASES extension_name FOR /c2a/if_report_extension~extension_name.

    CONSTANTS: BEGIN OF main_parameters,
                 variant               TYPE rsscr_name VALUE 'P_VARI',
                 alternative_selection TYPE rsscr_name VALUE 'P_SELEXT',
                 alternative_display   TYPE rsscr_name VALUE 'P_DISEXT',
               END OF main_parameters.

    DATA extension_parameters TYPE /c2a/extension_parameter_tab.

    METHODS is_selection_active
      IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
      RETURNING VALUE(r_is_active) TYPE abap_bool.

    METHODS is_display_active
      IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
      RETURNING VALUE(r_is_active) TYPE abap_bool.

    METHODS get_attributes_from_screen
      IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen.

    METHODS put_attributes_to_screen
      IMPORTING i_selection_screen TYPE REF TO /c2a/if_selection_screen
      RAISING   /c2a/cx_error_message.

  PRIVATE SECTION.
ENDCLASS.


CLASS /c2a/cl_report_extension IMPLEMENTATION.
  METHOD constructor.
    extension_name = i_extension_name.
    extension_parameters = i_extension_parameters.
  ENDMETHOD.

  METHOD get_attributes_from_screen.
    LOOP AT extension_parameters INTO DATA(ls_parameter) WHERE attribute_name IS NOT INITIAL.
      ASSIGN me->(ls_parameter-attribute_name) TO FIELD-SYMBOL(<attribute>).
      IF sy-subrc = 0.
        i_selection_screen->get_dynpro_field( EXPORTING i_sel_name  = ls_parameter-parameter_name
                                              IMPORTING e_parameter = <attribute> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_display_active.
    DATA(alternative_display) = VALUE /c2a/report_extension_name( ).
    i_selection_screen->get_dynpro_field( EXPORTING i_sel_name  = main_parameters-alternative_display
                                          IMPORTING e_parameter = alternative_display ).
    r_is_active = xsdbool( alternative_display = extension_name ).
  ENDMETHOD.

  METHOD is_selection_active.
    DATA(alternative_selection) = VALUE /c2a/report_extension_name( ).
    i_selection_screen->get_dynpro_field( EXPORTING i_sel_name  = main_parameters-alternative_selection
                                          IMPORTING e_parameter = alternative_selection ).
    r_is_active = xsdbool( alternative_selection = extension_name ).
  ENDMETHOD.

  METHOD put_attributes_to_screen.
    LOOP AT extension_parameters INTO DATA(ls_parameter) WHERE attribute_name IS NOT INITIAL.
      ASSIGN me->(ls_parameter-attribute_name) TO FIELD-SYMBOL(<attribute>).
      IF sy-subrc = 0.
        i_selection_screen->set_dynpro_field( i_sel_name  = ls_parameter-parameter_name
                                              i_parameter = <attribute> ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
