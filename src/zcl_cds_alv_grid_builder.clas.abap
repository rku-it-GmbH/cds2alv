CLASS zcl_cds_alv_grid_builder DEFINITION PUBLIC CREATE PUBLIC INHERITING FROM zcl_cds_alv_base.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_grid_builder.

    METHODS constructor
      IMPORTING i_cds_view         TYPE ddstrucobjname
                i_ddic_access      TYPE REF TO zif_cds_alv_ddic_access
                i_persistence      TYPE REF TO zif_cds_alv_persistence
                i_memory           TYPE REF TO zif_cds_alv_memory
                i_factory          TYPE REF TO zif_cds_alv_factory
                i_selection_screen TYPE REF TO zif_cds_alv_selection_screen
                i_selection        TYPE REF TO zif_cds_alv_selection
                i_value_help       TYPE REF TO zif_cds_alv_value_help
                i_bopf_handler     TYPE REF TO zif_cds_alv_bopf_handler
                i_navigation       TYPE REF TO zif_cds_alv_navigation
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    TYPES: BEGIN OF ty_field_properties,
             fieldname           TYPE fieldname,
             fieldtype           TYPE zcds_alv_field_type,
             is_key              TYPE abap_bool,
             label               TYPE string,
             quickinfo           TYPE string,
             is_email            TYPE abap_bool,
             hidden              TYPE abap_bool,
             technical           TYPE abap_bool,
             default_aggregation TYPE string,
             position            TYPE i,
             is_hotspot          TYPE abap_bool,
             has_value_help      TYPE abap_bool,
             is_editable         TYPE abap_bool,
           END OF ty_field_properties.
    TYPES ty_field_properties_table TYPE STANDARD TABLE OF ty_field_properties WITH EMPTY KEY.

    DATA field_properties_table TYPE ty_field_properties_table.
    DATA event_handlers         TYPE zcds_alv_grid_event_handlers.
    DATA table_container        TYPE REF TO zif_cds_alv_table_container.
    DATA selection              TYPE REF TO zif_cds_alv_selection.
    DATA value_help             TYPE REF TO zif_cds_alv_value_help.
    DATA bopf_handler           TYPE REF TO zif_cds_alv_bopf_handler.
    DATA navigation             TYPE REF TO zif_cds_alv_navigation.
    DATA selection_screen       TYPE REF TO zif_cds_alv_selection_screen.
    DATA alternative_selection  TYPE REF TO zif_cds_alv_select_extension.
    DATA alv_grid               TYPE REF TO cl_gui_alv_grid.
    DATA field_actions          TYPE zcds_alv_field_actions.
    DATA editable_fields        TYPE ddfieldnames.
    DATA variant                TYPE disvariant.
    DATA layout                 TYPE lvc_s_layo.
    DATA exclude_functions      TYPE ui_functions.
    DATA fieldcatalog           TYPE lvc_t_fcat.
    DATA sort_order             TYPE lvc_t_sort.
    DATA filter                 TYPE lvc_t_filt.
    DATA value_help_fields      TYPE lvc_t_f4.

    METHODS evaluate_annotations REDEFINITION.

    METHODS build_fieldcatalog
      RAISING zcx_cds_alv_message.

    METHODS build_variant.
    METHODS build_layout.
    METHODS build_f4.
    METHODS build_exclude_functions.
    METHODS build_event_handler.
    METHODS register_event_handlers.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_ui_annotation,
             index TYPE sytabix,
             key   TYPE string,
             value TYPE string,
           END OF ty_ui_annotation.
    TYPES ty_ui_annotations TYPE SORTED TABLE OF ty_ui_annotation WITH NON-UNIQUE KEY index.

    CONSTANTS function_code_prefix TYPE string VALUE 'ZZ_CDS_ALV_FC'.

    DATA update_enabled TYPE xsdboolean.
    DATA delete_enabled TYPE xsdboolean.

    METHODS sort_columns.
ENDCLASS.



CLASS ZCL_CDS_ALV_GRID_BUILDER IMPLEMENTATION.
  METHOD build_event_handler.
    DATA(event_handler) = NEW zcl_cds_alv_grid_event_handler( i_cds_view              = cds_view
                                                              i_alv_grid              = alv_grid
                                                              i_selection             = selection
                                                              i_value_help            = value_help
                                                              i_navigation            = navigation
                                                              i_bopf_handler          = bopf_handler
                                                              i_table_container       = table_container
                                                              i_selection_screen      = selection_screen
                                                              i_alternative_selection = alternative_selection
                                                              i_field_actions         = field_actions
                                                              i_update_enabled        = update_enabled
                                                              i_delete_enabled        = delete_enabled
                                                              i_editable_fields       = editable_fields ).
    INSERT event_handler INTO TABLE event_handlers.
  ENDMETHOD.

  METHOD build_exclude_functions.
    exclude_functions = VALUE #( ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                                 ( cl_gui_alv_grid=>mc_fc_loc_copy )
                                 ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                                 ( cl_gui_alv_grid=>mc_fc_loc_cut )
                                 ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                                 ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                                 ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                                 ( cl_gui_alv_grid=>mc_fc_loc_paste )
                                 ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                                 ( cl_gui_alv_grid=>mc_fc_loc_undo ) ).
  ENDMETHOD.

  METHOD build_f4.
    value_help_fields = VALUE #( FOR field_properties IN field_properties_table
                                 WHERE
                                 ( has_value_help = abap_true )
                                 ( fieldname  = field_properties-fieldname
                                   register   = abap_true
                                   getbefore  = abap_true
                                   chngeafter = abap_true ) ).
  ENDMETHOD.

  METHOD build_fieldcatalog.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = cds_view
      CHANGING
        ct_fieldcat            = fieldcatalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DELETE fieldcatalog WHERE fieldname = '.NODE1'.

    " Apply Annotations
    LOOP AT fieldcatalog ASSIGNING FIELD-SYMBOL(<field>).
      READ TABLE field_properties_table INTO DATA(field_properties)
           WITH KEY fieldname = <field>-fieldname.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE ddfields INTO DATA(ddfield)
           WITH KEY fieldname = <field>-fieldname.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF field_properties-label IS NOT INITIAL.
        <field>-reptext = field_properties-label.

        IF strlen( field_properties-label ) <= 40.
          <field>-scrtext_l = field_properties-label.
        ENDIF.
        IF strlen( field_properties-label ) <= 20.
          <field>-scrtext_m = field_properties-label.
        ENDIF.
        IF strlen( field_properties-label ) <= 10.
          <field>-scrtext_s = field_properties-label.
        ENDIF.
      ENDIF.

      IF field_properties-quickinfo IS NOT INITIAL.
        <field>-tooltip = field_properties-quickinfo.
      ENDIF.

      IF ddfield-domname = 'XFELD' OR ddfield-rollname = 'XFELD'.
        <field>-checkbox = abap_true.
      ENDIF.

      <field>-tech    = field_properties-technical.
      <field>-no_out  = field_properties-hidden.
      <field>-hotspot = field_properties-is_hotspot.
      <field>-edit    = field_properties-is_editable.
      <field>-do_sum  = SWITCH #( field_properties-default_aggregation
                                  WHEN '#MAX'  THEN 'A'
                                  WHEN '#MIN'  THEN 'B'
                                  WHEN '#AVG'  THEN 'C'
                                  WHEN '#SUM'  THEN 'X'
                                  WHEN '#NONE' THEN ' ' ).

      IF field_properties-has_value_help = abap_true.
        <field>-f4availabl = abap_true.
      ENDIF.

      " CFIELDNAME from Semantics.amount.currencyCode
      " QFIELDNAME from Semantics.quantity.unitOfMeasure
      " SP_GROUP map to UI.lineItem.qualifier
    ENDLOOP.

    IF table_container IS BOUND.
      DATA(special_columns) = table_container->get_special_columns( ).
      IF special_columns-index_fieldname IS NOT INITIAL.
        IF NOT line_exists( fieldcatalog[ fieldname = special_columns-index_fieldname ] ).
          INSERT VALUE #( fieldname = special_columns-index_fieldname
                          no_out    = abap_true
                          tech      = abap_true
                          ref_table = 'SE16N_REF'
                          ref_field = 'SE16N_LONG_LINES' )
                 INTO TABLE fieldcatalog.
        ENDIF.
      ENDIF.

      IF special_columns-count_fieldname IS NOT INITIAL.
        IF NOT line_exists( fieldcatalog[ fieldname = special_columns-count_fieldname ] ).
          INSERT VALUE #( fieldname = special_columns-count_fieldname
                          no_out    = abap_true
                          tech      = abap_true
                          ref_table = 'SE16N_REF'
                          ref_field = 'SE16N_LONG_LINES' )
                 INTO TABLE fieldcatalog.
        ENDIF.
      ENDIF.

      IF special_columns-system_fieldname IS NOT INITIAL.
        IF NOT line_exists( fieldcatalog[ fieldname = special_columns-system_fieldname ] ).
          INSERT VALUE #( fieldname = special_columns-system_fieldname
                          no_out    = abap_true
                          key       = abap_true
                          ref_table = 'SYST'
                          ref_field = 'SYSID' )
                 INTO TABLE fieldcatalog.
        ENDIF.
      ENDIF.

      IF special_columns-client_fieldname IS NOT INITIAL.
        IF NOT line_exists( fieldcatalog[ fieldname = special_columns-client_fieldname ] ).
          INSERT VALUE #( fieldname = special_columns-client_fieldname
                          no_out    = abap_true
                          key       = abap_true
                          ref_table = 'SYST'
                          ref_field = 'MANDT' )
                 INTO TABLE fieldcatalog.
        ENDIF.
      ENDIF.
    ENDIF.

    sort_columns( ).
  ENDMETHOD.

  METHOD build_layout.
    CONSTANTS row_column TYPE lvc_libox VALUE 'A' ##NO_TEXT.

    layout-col_opt    = abap_true.
    layout-cwidth_opt = abap_true.
    layout-grid_title = description.
    layout-zebra      = abap_true.

    IF table_container IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(special_columns) = table_container->get_special_columns( ).
    IF special_columns-count_fieldname IS NOT INITIAL.
      layout-countfname = special_columns-count_fieldname.
    ENDIF.

    IF special_columns-style_fieldname IS NOT INITIAL.
      layout-stylefname = special_columns-style_fieldname.
      layout-edit       = abap_true.
    ENDIF.

    IF special_columns-box_fieldname IS NOT INITIAL.
      layout-box_fname = special_columns-box_fieldname.
      layout-sel_mode  = row_column.
    ENDIF.

    IF special_columns-info_fieldname IS NOT INITIAL.
      layout-info_fname = special_columns-info_fieldname.
    ENDIF.

    IF special_columns-color_fieldname IS NOT INITIAL.
      layout-ctab_fname = special_columns-color_fieldname.
    ENDIF.

    IF special_columns-excp_fieldname IS NOT INITIAL.
      layout-excp_fname = special_columns-excp_fieldname.
      layout-excp_led   = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD build_variant.
    variant-report   = sy-cprog.
    variant-username = sy-uname.

    IF selection_screen IS BOUND.
      selection_screen->get_dynpro_field( EXPORTING i_sel_name  = 'P_VARI'
                                          IMPORTING e_parameter = variant-variant ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    selection = i_selection.
    value_help = i_value_help.
    bopf_handler = i_bopf_handler.
    navigation = i_navigation.
    selection_screen = i_selection_screen.

    evaluate_annotations( ).
  ENDMETHOD.

  METHOD evaluate_annotations.
    DATA(action_counter) = VALUE numc3( ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(keys) = VALUE ddfieldnames( ).

    " key fields from semantic key
    LOOP AT entity_annotations ASSIGNING FIELD-SYMBOL(<entity_annotation>)
         WHERE annoname CS 'OBJECTMODEL.SEMANTICKEY'.
      INSERT CONV #( to_upper( remove_quotes( <entity_annotation>-value ) ) )
             INTO TABLE keys.
    ENDLOOP.

    " Field Annotations; ReadOnly is evaluated in the end
    " UI.LineItem can be repeated, therefore it needs an inner loop
    LOOP AT element_annotations ASSIGNING FIELD-SYMBOL(<element_annotation_group>)
         GROUP BY ( elementname = <element_annotation_group>-elementname ).

      DATA(fieldname) = CONV fieldname( <element_annotation_group>-elementname ).
      DATA(field_properties) = VALUE ty_field_properties( fieldname = fieldname ).
      DATA(ui_annotations) = VALUE ty_ui_annotations( ).
      DATA(semantic_object) = VALUE zcds_alv_semantic_object( ).
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(function) = VALUE zcds_alv_function( ).

      LOOP AT GROUP <element_annotation_group> ASSIGNING FIELD-SYMBOL(<element_annotation>).
        CASE <element_annotation>-annoname.
          WHEN 'CONSUMPTION.HIDDEN'.
            field_properties-technical = abap_true.

          WHEN 'CONSUMPTION.VALUEHELP'
            OR 'CONSUMPTION.VALUEHELPDEFINITION.ASSOCIATION'
            OR 'CONSUMPTION.VALUEHELPDEFINITION.ENTITY.NAME'.
            field_properties-has_value_help = abap_true.

          WHEN 'DEFAULTAGGREGATION'.
            field_properties-default_aggregation = <element_annotation>-value.

          WHEN 'UI.HIDDEN'.
            field_properties-hidden = abap_true.

          WHEN 'SEMANTICS.EMAIL.ADDRESS'.
            field_properties-is_email = abap_true.

          WHEN 'CONSUMPTION.SEMANTICOBJECT'.
            semantic_object = remove_quotes( <element_annotation>-value ).

          WHEN 'ENDUSERTEXT.LABEL'.
            field_properties-label = <element_annotation>-value.

          WHEN 'ENUSERTEXT.QUICKINFO'.
            field_properties-quickinfo = <element_annotation>-value.
        ENDCASE.

        IF <element_annotation>-annoname CP 'UI.LINEITEM.*'.
          INSERT VALUE #( index = 0
                          key   = <element_annotation>-annoname
                          value = <element_annotation>-value )
                 INTO TABLE ui_annotations.
        ELSEIF <element_annotation>-annoname CP 'UI.LINEITEM$*$.*'.
          SPLIT <element_annotation>-annoname AT '$' INTO
                DATA(ui_line_item) DATA(index) DATA(property).
          INSERT VALUE #( index = index
                          key   = |{ ui_line_item }{ property }|
                          value = <element_annotation>-value )
                 INTO TABLE ui_annotations.
        ENDIF.
      ENDLOOP.

      LOOP AT ui_annotations INTO DATA(ui_index) GROUP BY ui_index-index.
        DATA(field_action) = VALUE zcds_alv_field_action( cds_view  = cds_view
                                                          fieldname = fieldname ).

        LOOP AT GROUP ui_index INTO DATA(ui_annotation).
          CASE ui_annotation-key.
            WHEN 'UI.LINEITEM.POSITION'.
              field_properties-position = CONV i( ui_annotation-value ).

            WHEN 'UI.LINEITEM.TYPE'.
              field_action-fieldtype = ui_annotation-value.

            WHEN 'UI.LINEITEM.SEMANTICOBJECT'.
              semantic_object = remove_quotes( ui_annotation-value ).

            WHEN 'UI.LINEITEM.SEMANTICOBJECTACTION'.
              field_action-semantic_action = remove_quotes( ui_annotation-value ).

            WHEN 'UI.LINEITEM.TARGETELEMENT'.
              field_action-associationname = remove_quotes( ui_annotation-value ).

            WHEN 'UI.LINEITEM.DATAACTION'.
              field_action-data_action = substring_after( val = remove_quotes( ui_annotation-value ) sub = 'BOPF:' ).

            WHEN 'UI.LINEITEM.LABEL'.
              field_action-label = remove_quotes( ui_annotation-value ).

            WHEN 'UI.LINEITEM.URL'.
              field_action-url_fieldname = remove_quotes( ui_annotation-value ).

          ENDCASE.
        ENDLOOP.

        " Navigation and actions
        CASE field_action-fieldtype.
          WHEN '#STANDARD' OR space. " Hotspot for mail
            IF field_properties-is_email = abap_true.
              field_properties-is_hotspot = abap_true.
              field_action-hotspot   = abap_true.
              field_action-send_mail = abap_true.
              INSERT field_action INTO TABLE field_actions.
            ENDIF.

          WHEN '#WITH_INTENT_BASED_NAVIGATION'. " Hyperlink
            IF semantic_object IS NOT INITIAL AND field_action-semantic_action IS NOT INITIAL.
              field_properties-is_hotspot = abap_true.
              field_action-hotspot         = abap_true.
              field_action-semantic_object = semantic_object.
              INSERT field_action INTO TABLE field_actions.
            ENDIF.

          WHEN '#WITH_URL'. " Hyperlink
            IF field_action-url_fieldname IS NOT INITIAL.
              field_properties-is_hotspot = abap_true.
              field_action-hotspot = abap_true.
              INSERT field_action INTO TABLE field_actions.
            ENDIF.

          WHEN '#FOR_INTENT_BASED_NAVIGATION'. " Button
            IF semantic_object IS NOT INITIAL AND field_action-semantic_action IS NOT INITIAL.
              action_counter = action_counter + 1.
              field_action-user_command    = |{ function_code_prefix }{ action_counter }|.
              field_action-semantic_object = semantic_object.

              IF field_action-label IS INITIAL.
                field_action-label = |{ field_action-semantic_object }.{ field_action-semantic_action }|.
              ENDIF.

              INSERT field_action INTO TABLE field_actions.
            ENDIF.

          WHEN '#WITH_NAVIGATION_PATH'. " Button
            IF field_action-associationname IS NOT INITIAL.
              action_counter = action_counter + 1.
              field_action-user_command = |{ function_code_prefix }{ action_counter }|.

              IF field_action-label IS INITIAL.
                field_action-label = field_action-associationname.
              ENDIF.

              INSERT field_action INTO TABLE field_actions.
            ENDIF.

          WHEN '#FOR_ACTION'. " Button
            IF field_action-data_action IS NOT INITIAL.
              action_counter = action_counter + 1.
              field_action-user_command = |{ function_code_prefix }{ action_counter }|.

              IF field_action-label IS INITIAL.
                field_action-label = field_action-data_action.
              ENDIF.

              INSERT field_action INTO TABLE field_actions.
            ENDIF.
        ENDCASE.
      ENDLOOP.

      " Headers
      IF field_properties-quickinfo IS INITIAL AND field_properties-label IS NOT INITIAL.
        field_properties-quickinfo = field_properties-label.
      ENDIF.

      INSERT field_properties INTO TABLE field_properties_table.
    ENDLOOP.

    " Editable fields
    update_enabled = xsdbool( line_exists( entity_annotations[ annoname = 'OBJECTMODEL.UPDATEENABLED' value = 'true' ] ) ).
    delete_enabled = xsdbool( line_exists( entity_annotations[ annoname = 'OBJECTMODEL.DELETEENABLED' value = 'true' ] ) ).

    IF update_enabled = abap_true.
      LOOP AT ddfields ASSIGNING FIELD-SYMBOL(<ddfield>).
        IF line_exists( element_annotations[ elementname = <ddfield>-fieldname
                                             annoname    = 'OBJECTMODEL.READONLY'
                                             value       = 'true' ] ).
          CONTINUE.
        ENDIF.

        READ TABLE field_properties_table ASSIGNING FIELD-SYMBOL(<field_properties>)
             WITH KEY fieldname = <ddfield>-fieldname.
        IF sy-subrc = 0.
          <field_properties>-is_editable = abap_true.
          INSERT <ddfield>-fieldname INTO TABLE editable_fields.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD register_event_handlers.
    LOOP AT event_handlers INTO DATA(event_handler).
      SET HANDLER event_handler->on_help_request FOR alv_grid.
      SET HANDLER event_handler->on_value_request FOR alv_grid.
      SET HANDLER event_handler->on_data_changed FOR alv_grid.
      SET HANDLER event_handler->on_before_user_command FOR alv_grid.
      SET HANDLER event_handler->on_user_command FOR alv_grid.
      SET HANDLER event_handler->on_after_user_command FOR alv_grid.
      SET HANDLER event_handler->on_double_click FOR alv_grid.
      SET HANDLER event_handler->on_context_menu_request FOR alv_grid.
      SET HANDLER event_handler->on_menu_button FOR alv_grid.
      SET HANDLER event_handler->on_toolbar FOR alv_grid.
      SET HANDLER event_handler->on_hotspot_click FOR alv_grid.
      SET HANDLER event_handler->on_after_refresh FOR alv_grid.
      SET HANDLER event_handler->on_button_click FOR alv_grid.
      SET HANDLER event_handler->on_data_changed_finished FOR alv_grid.
    ENDLOOP.
  ENDMETHOD.

  METHOD sort_columns.
    TYPES: BEGIN OF ty_field_position,
             fieldname TYPE fieldname,
             position  TYPE lvc_colpos,
           END OF ty_field_position.

    DATA field_positions TYPE STANDARD TABLE OF ty_field_position.

    SORT fieldcatalog BY col_pos ASCENDING.
    SORT field_properties_table BY position ASCENDING.
    DATA(position) = 1.

    LOOP AT field_properties_table INTO DATA(field_properties) WHERE position IS NOT INITIAL.
      INSERT VALUE #( fieldname = field_properties-fieldname position = position )
             INTO TABLE field_positions.
      position = position + 1.
    ENDLOOP.

    LOOP AT fieldcatalog ASSIGNING FIELD-SYMBOL(<field>).
      TRY.
          DATA(field_position) = field_positions[ fieldname = <field>-fieldname ].
          <field>-col_pos = field_position-position.
        CATCH cx_sy_itab_line_not_found.
          <field>-col_pos = position.
          position = position + 1.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cds_alv_grid_builder~create_alv_grid.
    CONSTANTS save_all TYPE char01 VALUE 'A' ##NO_TEXT.

    table_container = i_table_container.

    CREATE OBJECT alv_grid
      EXPORTING
        i_parent = i_container
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    build_variant( ).
    build_layout( ).
    build_fieldcatalog( ).
    build_f4( ).
    build_exclude_functions( ).
    build_event_handler( ).

    DATA(ref_to_table) = table_container->get_ref_to_table( ).
    ASSIGN ref_to_table->* TO FIELD-SYMBOL(<table>).

    register_event_handlers( ).

    alv_grid->register_f4_for_fields( value_help_fields ).
    alv_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).

    alv_grid->set_table_for_first_display( EXPORTING  is_variant           = variant
                                                      i_save               = save_all
                                                      i_default            = xsdbool( variant-variant IS INITIAL )
                                                      is_layout            = layout
                                                      it_toolbar_excluding = exclude_functions
                                           CHANGING   it_outtab            = <table>
                                                      it_fieldcatalog      = fieldcatalog
                                                      it_sort              = sort_order
                                                      it_filter            = filter
                                           EXCEPTIONS OTHERS               = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF update_enabled = abap_true.
      alv_grid->set_ready_for_input( ).
    ENDIF.

    alv_grid->set_toolbar_interactive( ).

    e_alv_grid = alv_grid.
  ENDMETHOD.

  METHOD zif_cds_alv_grid_builder~get_gui_title.
    r_title = description.
  ENDMETHOD.

  METHOD zif_cds_alv_grid_builder~get_metadata.
    table_container = i_table_container.

    build_variant( ).
    build_layout( ).
    build_fieldcatalog( ).

    e_variant = variant.
    e_layout = layout.
    e_fieldcatalog = fieldcatalog.
    e_sort_order = sort_order.
    e_filters = filter.
  ENDMETHOD.

  METHOD zif_cds_alv_grid_builder~register_alternative_selection.
    alternative_selection = i_selection_handler.
  ENDMETHOD.
ENDCLASS.
