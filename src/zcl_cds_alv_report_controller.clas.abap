CLASS zcl_cds_alv_report_controller DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_report_controller.
    INTERFACES zif_cds_alv_split_screen_cntr.

    ALIASES ok_code FOR zif_cds_alv_split_screen_cntr~ok_code.

    METHODS constructor
      IMPORTING i_cds_view           TYPE ddstrucobjname
                i_ddic_access        TYPE REF TO zif_cds_alv_ddic_access
                i_persistence        TYPE REF TO zif_cds_alv_persistence
                i_memory             TYPE REF TO zif_cds_alv_memory
                i_factory            TYPE REF TO zif_cds_alv_factory
                i_table_container    TYPE REF TO zif_cds_alv_table_container
                i_selection_screen   TYPE REF TO zif_cds_alv_selection_screen
                i_selection          TYPE REF TO zif_cds_alv_selection
                i_builder            TYPE REF TO zif_cds_alv_grid_builder
                i_extension_provider TYPE REF TO zif_cds_alv_extension_provider
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    DATA alternative_selection TYPE zcds_alv_report_extension_name.
    DATA alternative_display   TYPE zcds_alv_report_extension_name.
    DATA table_container       TYPE REF TO zif_cds_alv_table_container.
    DATA selection_screen      TYPE REF TO zif_cds_alv_selection_screen.
    DATA selection             TYPE REF TO zif_cds_alv_selection.
    DATA builder               TYPE REF TO zif_cds_alv_grid_builder.
    DATA extension_provider    TYPE REF TO zif_cds_alv_extension_provider.
    DATA extensions            TYPE zif_cds_alv_extension_provider=>ty_report_extensions.

    METHODS evaluate_annotations REDEFINITION.

  PRIVATE SECTION.
    METHODS default_selection
      IMPORTING i_forall       TYPE abap_bool
                i_memory_id    TYPE memory_id
      EXPORTING e_result_table TYPE STANDARD TABLE
      RAISING   zcx_cds_alv_message.

    METHODS default_display
      IMPORTING i_in_split_screen TYPE abap_bool
      RAISING   zcx_cds_alv_message.

    METHODS refresh
      RAISING zcx_cds_alv_message.

    METHODS select
      RAISING zcx_cds_alv_message.
ENDCLASS.



CLASS zcl_cds_alv_report_controller IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    table_container = i_table_container.
    selection_screen = i_selection_screen.
    selection = i_selection.
    builder = i_builder.
    extension_provider = i_extension_provider.

    evaluate_annotations( ).
    extensions = extension_provider->get_report_extensions( cds_view ).
  ENDMETHOD.

  METHOD default_display.
    CASE i_in_split_screen.
      WHEN abap_false.
        CALL FUNCTION 'Z_CDS_ALV_FULL_SCREEN'
          EXPORTING i_builder         = builder
                    i_table_container = table_container.

      WHEN abap_true.
        DATA(program) = persistence->get_report_for_cds_view( cds_view ).

        CALL FUNCTION 'Z_CDS_ALV_SPLIT_SCREEN'
          EXPORTING i_builder         = builder
                    i_table_container = table_container
                    i_sub_repid       = program-progname
                    i_sub_dynnr       = program-dynpro
                    i_controller      = me.
    ENDCASE.
  ENDMETHOD.

  METHOD default_selection.
    CASE i_forall.
      WHEN abap_false.
        selection->perform_selection( EXPORTING i_condition_provider = selection_screen
                                      IMPORTING e_result_table       = e_result_table ).

      WHEN abap_true.
        memory->import_forall_table( EXPORTING i_memory_id           = i_memory_id
                                     IMPORTING e_source_view         = DATA(source_view)
                                               e_association_name    = DATA(association_name)
                                               e_source_parameters   = DATA(source_parameters)
                                               e_ref_to_forall_table = DATA(ref_to_forall_table) ).

        ASSIGN ref_to_forall_table->* TO FIELD-SYMBOL(<forall_table>).

        selection->perform_selection_forall( EXPORTING i_condition_provider = selection_screen
                                                       i_source_view        = source_view
                                                       i_association_name   = association_name
                                                       i_source_parameters  = source_parameters
                                                       i_forall_table       = <forall_table>
                                             IMPORTING e_result_table       = e_result_table ).
    ENDCASE.
  ENDMETHOD.

  METHOD evaluate_annotations.
    " Nothing here yet; but the instance definitely needs to be parametrized with the CDS view
  ENDMETHOD.

  METHOD refresh.
    DATA ref_to_table TYPE REF TO data.

    DATA(table_descriptor) = table_container->get_table_descriptor( ).
    CREATE DATA ref_to_table TYPE HANDLE table_descriptor.
    ASSIGN ref_to_table->* TO FIELD-SYMBOL(<result_table>).

    TRY.
        DATA(selection_handler) = extensions[ extension = alternative_selection ]-instance.
        selection_handler->alternative_reselection( EXPORTING i_cds_view         = cds_view
                                                              i_selection_screen = selection_screen
                                                              i_table_container  = table_container
                                                    CHANGING  c_result_table     = <result_table> ).

      CATCH cx_sy_itab_line_not_found.
        selection->perform_reselection( CHANGING c_result_table = <result_table> ).
    ENDTRY.

    table_container->set_table( <result_table> ).
  ENDMETHOD.

  METHOD select.
    DATA ref_to_table TYPE REF TO data.

    DATA(table_descriptor) = table_container->get_table_descriptor( ).
    CREATE DATA ref_to_table TYPE HANDLE table_descriptor.
    ASSIGN ref_to_table->* TO FIELD-SYMBOL(<result_table>).

    selection_screen->read_selection_screen( ).

    TRY.
        DATA(selection_handler) = extensions[ extension = alternative_selection ]-instance.
        selection_handler->alternative_selection( EXPORTING i_cds_view         = cds_view
                                                            i_selection_screen = selection_screen
                                                            i_table_container  = table_container
                                                  IMPORTING e_result_table     = <result_table> ).

      CATCH cx_sy_itab_line_not_found.
        selection->perform_selection( EXPORTING i_condition_provider = selection_screen
                                      IMPORTING e_result_table       = <result_table> ).
    ENDTRY.

    table_container->set_table( <result_table> ).
  ENDMETHOD.

  METHOD zif_cds_alv_report_controller~at_help_request.
    TRY.
        LOOP AT extensions INTO DATA(extension).
          extension-instance->show_help( i_selection_screen = selection_screen
                                         i_sel_name         = i_sel_name ).
        ENDLOOP.

      CATCH zcx_cds_alv_message INTO DATA(exception).
        MESSAGE exception TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_report_controller~at_selection_screen.
    TRY.
        LOOP AT extensions INTO DATA(extension).
          extension-instance->handle_user_command( i_selection_screen = selection_screen
                                                   i_user_command     = i_ucomm ).
        ENDLOOP.

      CATCH zcx_cds_alv_message INTO DATA(exception).
        MESSAGE exception TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_report_controller~at_selection_screen_output.
    TRY.
        selection_screen->apply_restriction( ).
        selection_screen->apply_selection_texts( ).
        selection_screen->modify_screen( ).

        LOOP AT extensions INTO DATA(extension).
          extension-instance->modify_screen( selection_screen ).
        ENDLOOP.

      CATCH zcx_cds_alv_message INTO DATA(exception).
        MESSAGE exception TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_report_controller~at_value_request.
    TRY.
        selection_screen->value_help_for_field( EXPORTING i_sel_name = i_sel_name
                                                CHANGING  c_value    = c_value ).

        LOOP AT extensions INTO DATA(extension).
          extension-instance->value_help( EXPORTING i_selection_screen = selection_screen
                                                    i_sel_name         = i_sel_name
                                          CHANGING  c_value            = c_value ).
        ENDLOOP.

      CATCH zcx_cds_alv_message INTO DATA(exception).
        MESSAGE exception TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_report_controller~initialization.
    TRY.
        LOOP AT extensions INTO DATA(extension).
          extension-instance->initialization( selection_screen ).
        ENDLOOP.

      CATCH zcx_cds_alv_message INTO DATA(exception).
        MESSAGE exception TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_report_controller~start_of_selection.
    DATA ref_to_table TYPE REF TO data.

    DATA(table_descriptor) = table_container->get_table_descriptor( ).
    CREATE DATA ref_to_table TYPE HANDLE table_descriptor.
    ASSIGN ref_to_table->* TO FIELD-SYMBOL(<result_table>).

    TRY.
        selection_screen->read_selection_screen( ).

        TRY.
            DATA(no_display) = abap_false.
            DATA(selection_handler) = extensions[ extension = i_selection ]-instance.
            alternative_selection = i_selection.
            builder->register_alternative_selection( selection_handler ).
            selection_handler->alternative_selection( EXPORTING i_cds_view         = cds_view
                                                                i_selection_screen = selection_screen
                                                                i_table_container  = table_container
                                                      IMPORTING e_result_table     = <result_table>
                                                                e_no_display       = no_display ).

          CATCH cx_sy_itab_line_not_found.
            default_selection( EXPORTING i_forall       = i_forall
                                         i_memory_id    = i_memory_id
                               IMPORTING e_result_table = <result_table> ).
        ENDTRY.

        table_container->set_table( <result_table> ).

        IF no_display = abap_true.
          RETURN.
        ENDIF.

        TRY.
            DATA(display_handler) = extensions[ extension = i_display ]-instance.
            alternative_display = i_display.
            display_handler->alternative_display( i_selection_screen = selection_screen
                                                  i_table_container  = table_container
                                                  i_builder          = builder ).

          CATCH cx_sy_itab_line_not_found.
            default_display( i_in_split_screen ).
        ENDTRY.

      CATCH zcx_cds_alv_message INTO DATA(exception).
        MESSAGE exception TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_split_screen_cntr~handle_user_command.
    CASE i_ok_code.
      WHEN ok_code-select.
        select( ).

      WHEN OTHERS.
        LOOP AT extensions INTO DATA(extension).
          extension-instance->handle_user_command( i_selection_screen = selection_screen
                                                   i_user_command     = CONV #( i_ok_code ) ).
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
