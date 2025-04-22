CLASS /c2a/cl_alv_grid_event_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES /c2a/if_alv_grid_event_handler.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING i_cds_view               TYPE /c2a/cds_view_name
                i_alv_grid               TYPE REF TO cl_gui_alv_grid
                i_table_container        TYPE REF TO /c2a/if_table_container
                i_selection              TYPE REF TO /c2a/if_selection           OPTIONAL
                i_value_help             TYPE REF TO /c2a/if_value_help          OPTIONAL
                i_navigation             TYPE REF TO /c2a/if_navigation          OPTIONAL
                i_action_handler         TYPE REF TO /c2a/if_action_handler      OPTIONAL
                i_selection_screen       TYPE REF TO /c2a/if_selection_screen    OPTIONAL
                i_alternative_selection  TYPE REF TO /c2a/if_selection_extension OPTIONAL
                i_field_actions          TYPE /c2a/field_action_tab              OPTIONAL
                i_update_enabled         TYPE abap_bool                          DEFAULT abap_false
                i_delete_enabled         TYPE abap_bool                          DEFAULT abap_false
                i_editable_fields        TYPE ddfieldnames                       OPTIONAL
                i_functions_display_mode TYPE /c2a/function_display_mode         OPTIONAL.

  PROTECTED SECTION.
    CONSTANTS:
      BEGIN OF button_type,
        normal       TYPE tb_btype VALUE 0,
        menu_default TYPE tb_btype VALUE 1,
        menu         TYPE tb_btype VALUE 2,
        separator    TYPE tb_btype VALUE 3,
        radio_button TYPE tb_btype VALUE 4,
        check_box    TYPE tb_btype VALUE 5,
        menu_entry   TYPE tb_btype VALUE 6,
      END OF button_type.
    CONSTANTS:
      BEGIN OF standard_function_code,
        refresh                   TYPE ui_func VALUE 'REFRESH',
        delete                    TYPE ui_func VALUE 'DELETE',
        save                      TYPE ui_func VALUE 'SAVE',
        toggle_change_mode        TYPE ui_func VALUE 'DISPCHNG',
        additional_functions_menu TYPE ui_func VALUE 'ADD_FUNC',
      END OF standard_function_code.
    CONSTANTS:
      BEGIN OF display_mode,
        dropdown_menu TYPE /c2a/function_display_mode VALUE '0',
        buttons       TYPE /c2a/function_display_mode VALUE '1',
      END OF display_mode.

    CLASS-DATA standard_function_codes TYPE ui_functions.

    DATA cds_view               TYPE /c2a/cds_view_name.
    DATA ref_to_table           TYPE REF TO data.
    DATA alv_grid               TYPE REF TO cl_gui_alv_grid.
    DATA selection              TYPE REF TO /c2a/if_selection.
    DATA value_help             TYPE REF TO /c2a/if_value_help.
    DATA navigation             TYPE REF TO /c2a/if_navigation.
    DATA action_handler         TYPE REF TO /c2a/if_action_handler.
    DATA table_container        TYPE REF TO /c2a/if_table_container.
    DATA selection_screen       TYPE REF TO /c2a/if_selection_screen.
    DATA alternative_selection  TYPE REF TO /c2a/if_selection_extension.
    DATA field_actions          TYPE /c2a/field_action_tab.
    DATA functions              TYPE /c2a/function_tab.
    DATA standard_functions     TYPE /c2a/function_tab.
    DATA additional_functions   TYPE /c2a/function_tab.
    DATA update_enabled         TYPE abap_bool.
    DATA delete_enabled         TYPE abap_bool.
    DATA editable_fields        TYPE ddfieldnames.
    DATA functions_display_mode TYPE /c2a/function_display_mode.

    METHODS dispatch_standard_function
      IMPORTING i_function      TYPE ui_func
                i_selected_rows TYPE STANDARD TABLE.

    METHODS dispatch_single_action
      IMPORTING i_field_action TYPE /c2a/field_action
                i_selected_row TYPE any.

    METHODS dispatch_mass_action
      IMPORTING i_field_action  TYPE /c2a/field_action
                i_selected_rows TYPE STANDARD TABLE.

    METHODS build_function_table.

    METHODS toggle_change_mode
      RAISING /c2a/cx_error_message.

    METHODS refresh
      RAISING /c2a/cx_error_message.

  PRIVATE SECTION.
    METHODS call_browser
      IMPORTING i_url TYPE string
      RAISING   /c2a/cx_error_message.

    METHODS send_email
      IMPORTING i_email TYPE ad_smtpadr
      RAISING   /c2a/cx_error_message.
ENDCLASS.


CLASS /c2a/cl_alv_grid_event_handler IMPLEMENTATION.
  METHOD build_function_table.
    additional_functions = VALUE #( FOR field_action IN field_actions
                                    WHERE ( user_command IS NOT INITIAL )
                                    ( name = field_action-user_command
                                      text = field_action-label ) ).

    " Refresh; always first after standard functions
    INSERT VALUE #( name    = standard_function_code-refresh
                    icon    = icon_refresh
                    tooltip = TEXT-001 )
           INTO TABLE standard_functions.

    " standard buttons for editable grids
    IF update_enabled = abap_true.
      INSERT VALUE #( name    = standard_function_code-toggle_change_mode
                      icon    = icon_toggle_display_change
                      tooltip = TEXT-002 )
             INTO TABLE standard_functions.

      INSERT VALUE #( name    = standard_function_code-save
                      icon    = icon_system_save
                      tooltip = TEXT-003 )
             INTO TABLE standard_functions.
    ENDIF.

    IF delete_enabled = abap_true.
      INSERT VALUE #( name    = standard_function_code-delete
                      icon    = icon_delete
                      tooltip = TEXT-004 )
             INTO TABLE standard_functions.
    ENDIF.

    IF additional_functions IS NOT INITIAL AND functions_display_mode = display_mode-dropdown_menu.
      INSERT VALUE #( name        = standard_function_code-additional_functions_menu
                      icon        = icon_context_menu
                      tooltip     = TEXT-005
                      button_type = button_type-menu )
             INTO TABLE standard_functions.
    ENDIF.
  ENDMETHOD.

  METHOD call_browser.
    IF i_url IS INITIAL.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>execute( EXPORTING  document = i_url
                                       EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD class_constructor.
    standard_function_codes = VALUE #( ( standard_function_code-delete  )
                                       ( standard_function_code-refresh )
                                       ( standard_function_code-save    )
                                       ( standard_function_code-toggle_change_mode )
                                       ( standard_function_code-additional_functions_menu ) ).
  ENDMETHOD.

  METHOD constructor.
    cds_view = i_cds_view.
    alv_grid = i_alv_grid.
    selection = i_selection.
    value_help = i_value_help.
    navigation = i_navigation.
    action_handler = i_action_handler.
    selection_screen = i_selection_screen.
    table_container = i_table_container.
    field_actions = i_field_actions.
    update_enabled = i_update_enabled.
    delete_enabled = i_delete_enabled.
    editable_fields = i_editable_fields.
    alternative_selection = i_alternative_selection.
    functions_display_mode = i_functions_display_mode.

    ref_to_table = table_container->get_ref_to_table( ).
    build_function_table( ).
  ENDMETHOD.

  METHOD dispatch_mass_action.
    TRY.
        DATA(refresh_after) = abap_false.

        IF     i_field_action-semantic_object IS NOT INITIAL
           AND i_field_action-semantic_action IS NOT INITIAL.
          IF navigation IS BOUND.
            navigation->navigate_to_object_mass( EXPORTING i_object        = i_field_action-semantic_object
                                                           i_action        = i_field_action-semantic_action
                                                           i_cds_view      = i_field_action-cds_view
                                                           i_key_field     = i_field_action-fieldname
                                                           i_selected_rows = i_selected_rows
                                                 IMPORTING e_refresh_after = refresh_after ).
          ENDIF.

        ELSEIF i_field_action-associationname IS NOT INITIAL.
          IF selection_screen IS BOUND.
            DATA(source_parameters) = selection_screen->get_parameters( ).
          ENDIF.

          IF navigation IS BOUND.
            navigation->navigate_via_association( i_source_view       = i_field_action-cds_view
                                                  i_association_name  = i_field_action-associationname
                                                  i_source_parameters = source_parameters
                                                  i_selected_rows     = i_selected_rows ).
          ENDIF.

        ELSEIF i_field_action-data_action IS NOT INITIAL.
          IF action_handler IS BOUND.
            action_handler->execute_action( EXPORTING i_action        = i_field_action-data_action
                                                    i_selected_rows = i_selected_rows
                                          IMPORTING e_refresh_after = refresh_after ).
          ENDIF.
        ENDIF.

        IF refresh_after = abap_true.
          refresh( ).
        ENDIF.

      CATCH /c2a/cx_error_message INTO DATA(message).
        MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD dispatch_single_action.
    TRY.
        DATA(refresh_after) = abap_false.

        IF     i_field_action-semantic_object IS NOT INITIAL
           AND i_field_action-semantic_action IS NOT INITIAL.
          IF navigation IS BOUND.
            navigation->navigate_to_object_single( EXPORTING i_object        = i_field_action-semantic_object
                                                             i_action        = i_field_action-semantic_action
                                                             i_cds_view      = i_field_action-cds_view
                                                             i_key_field     = i_field_action-fieldname
                                                             i_selected_row  = i_selected_row
                                                   IMPORTING e_refresh_after = refresh_after ).
          ENDIF.

        ELSEIF i_field_action-url_fieldname IS NOT INITIAL.
          ASSIGN COMPONENT i_field_action-url_fieldname OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<url>).
          call_browser( to_lower( <url> ) ).
        ELSEIF i_field_action-send_mail IS NOT INITIAL.
          ASSIGN COMPONENT i_field_action-fieldname OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<email>).
          send_email( CONV #( <email> ) ).
        ENDIF.

        IF refresh_after = abap_true.
          refresh( ).
        ENDIF.

      CATCH /c2a/cx_error_message INTO DATA(message).
        MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD dispatch_standard_function.
    TRY.
        CASE i_function.
          WHEN standard_function_code-refresh.
            refresh( ).

          WHEN standard_function_code-toggle_change_mode.
            toggle_change_mode( ).

          WHEN standard_function_code-delete.
            IF action_handler IS BOUND.
              action_handler->delete( i_selected_rows ).
              refresh( ).
            ENDIF.

          WHEN standard_function_code-save.
            IF action_handler IS BOUND.
              action_handler->update( i_selected_rows ).
              refresh( ).
            ENDIF.
        ENDCASE.

      CATCH /c2a/cx_error_message INTO DATA(message).
        MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

  METHOD refresh.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    IF alternative_selection IS BOUND.
      alternative_selection->alternative_reselection( EXPORTING i_cds_view         = cds_view
                                                                i_selection_screen = selection_screen
                                                                i_table_container  = table_container
                                                      CHANGING  c_result_table     = <table> ).

    ELSEIF selection IS BOUND.
      selection->perform_reselection( CHANGING c_result_table = <table> ).
    ENDIF.

    table_container->set_table( <table> ).

    alv_grid->refresh_table_display( ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD send_email.
    IF i_email IS INITIAL.
      RETURN.
    ENDIF.

    cl_gui_frontend_services=>execute( EXPORTING  document = |mailto:{ i_email }|
                                       EXCEPTIONS OTHERS   = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD toggle_change_mode.
    table_container->toggle_change_mode( alv_grid ).
    alv_grid->refresh_table_display( ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_after_refresh.
    " not yet used
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_after_user_command.
    " not yet used
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_before_user_command.
    " not yet used
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_button_click.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    ASSIGN <table>[ es_row_no-row_id ] TO FIELD-SYMBOL(<selected_row>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        DATA(field_action) = field_actions[ fieldname = es_col_id-fieldname
                                            hotspot   = abap_true ].
        dispatch_single_action( i_field_action = field_action
                                i_selected_row = <selected_row> ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_context_menu_request.
    " no additional context menu functions yet
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_data_changed.
    " not yet used
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_data_changed_finished.
    " not yet used
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_double_click.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    ASSIGN <table>[ es_row_no-row_id ] TO FIELD-SYMBOL(<selected_row>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        DATA(field_action) = field_actions[ fieldname = e_column-fieldname
                                            hotspot   = abap_true ].
        dispatch_single_action( i_field_action = field_action
                                i_selected_row = <selected_row> ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_help_request.
    " not yet used
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_hotspot_click.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    ASSIGN <table>[ es_row_no-row_id ] TO FIELD-SYMBOL(<selected_row>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        DATA(field_action) = field_actions[ fieldname = e_column_id-fieldname
                                            hotspot   = abap_true ].
        dispatch_single_action( i_field_action = field_action
                                i_selected_row = <selected_row> ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_menu_button.
    IF e_ucomm = standard_function_code-additional_functions_menu.
      LOOP AT additional_functions INTO DATA(function).
        e_object->add_function( fcode = function-name
                                text  = CONV #( function-text )
                                icon  = CONV #( function-icon ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_toolbar.
    LOOP AT standard_functions INTO DATA(function).
      IF NOT line_exists( e_object->mt_toolbar[ function = function-name ] ).
        INSERT VALUE #( function  = function-name
                        icon      = function-icon
                        text      = function-text
                        quickinfo = function-tooltip
                        butn_type = function-button_type )
               INTO TABLE e_object->mt_toolbar.
      ENDIF.
    ENDLOOP.

    IF additional_functions IS INITIAL.
      DELETE e_object->mt_toolbar WHERE function = standard_function_code-additional_functions_menu.
    ENDIF.

    IF functions_display_mode = display_mode-buttons.
      LOOP AT additional_functions INTO DATA(additional_function).
        INSERT VALUE #( function  = additional_function-name
                        icon      = additional_function-icon
                        text      = additional_function-text
                        quickinfo = additional_function-tooltip
                        butn_type = additional_function-button_type )
               INTO TABLE e_object->mt_toolbar.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_user_command.
    DATA ref_to_selected_rows TYPE REF TO data.
    FIELD-SYMBOLS <selected_rows> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <table>         TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    CREATE DATA ref_to_selected_rows LIKE <table>.
    ASSIGN ref_to_selected_rows->* TO <selected_rows>.

    alv_grid->get_selected_rows( IMPORTING et_row_no = DATA(lt_row_no) ).
    LOOP AT lt_row_no INTO DATA(row_no) WHERE row_id <> 0.
      INSERT <table>[ row_no-row_id ] INTO TABLE <selected_rows>.
    ENDLOOP.

    IF line_exists( standard_function_codes[ table_line = e_ucomm ] ).
      dispatch_standard_function( i_function      = e_ucomm
                                  i_selected_rows = <selected_rows> ).
      RETURN.
    ENDIF.

    TRY.
        DATA(field_action) = field_actions[ user_command = e_ucomm ].
        dispatch_mass_action( i_field_action  = field_action
                              i_selected_rows = <selected_rows> ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD /c2a/if_alv_grid_event_handler~on_value_request.
    FIELD-SYMBOLS <modification> TYPE lvc_t_modi.
    FIELD-SYMBOLS <table>        TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    ASSIGN <table>[ es_row_no-row_id ] TO FIELD-SYMBOL(<selected_row>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        IF value_help IS BOUND.
          DATA(in_edit_mode) = table_container->is_in_edit_mode( ).
          DATA(field_is_editable) = xsdbool( line_exists( editable_fields[ table_line = e_fieldname ] ) ).
          DATA(display) = xsdbool( in_edit_mode = abap_false OR field_is_editable = abap_false ).

          value_help->value_help_for_element( EXPORTING i_fieldname    = e_fieldname
                                                        i_selected_row = <selected_row>
                                                        i_display      = display
                                              IMPORTING e_processed    = DATA(processed)
                                              CHANGING  c_value        = e_fieldvalue ).

          IF processed = abap_true AND display = abap_false.
            ASSIGN er_event_data->m_data->* TO <modification>.
            INSERT VALUE #( row_id    = es_row_no-row_id
                            fieldname = e_fieldname
                            value     = e_fieldvalue )
                   INTO TABLE <modification>.
          ENDIF.

          er_event_data->m_event_handled = processed.
        ENDIF.

      CATCH /c2a/cx_error_message INTO DATA(message).
        MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
