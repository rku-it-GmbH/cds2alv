CLASS zcl_cds_alv_selection_screen DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_condition_provider.
    INTERFACES zif_cds_alv_selection_screen.

    ALIASES read_selection_screen FOR zif_cds_alv_selection_screen~read_selection_screen.
    ALIASES get_dynpro_field      FOR zif_cds_alv_selection_screen~get_dynpro_field.
    ALIASES get_selections        FOR zif_cds_alv_selection_screen~get_selections.
    ALIASES get_parameters        FOR zif_cds_alv_selection_screen~get_parameters.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
                i_value_help  TYPE REF TO zif_cds_alv_value_help OPTIONAL
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    TYPES ty_rsparamsl_255_tab TYPE STANDARD TABLE OF rsparamsl_255 WITH EMPTY KEY.
    TYPES ty_selection_texts   TYPE STANDARD TABLE OF rsseltexts WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF kind,
                 parameter     TYPE rsscr_kind VALUE 'P' ##NO_TEXT,
                 select_option TYPE rsscr_kind VALUE 'S' ##NO_TEXT,
               END OF kind.
    CONSTANTS: BEGIN OF sign,
                 no_extensions TYPE raldb_sign VALUE 'N' ##NO_TEXT,
                 all           TYPE raldb_sign VALUE '*' ##NO_TEXT,
                 include       TYPE raldb_sign VALUE 'I' ##NO_TEXT,
                 exclude       TYPE raldb_sign VALUE 'E' ##NO_TEXT,
               END OF sign.
    CONSTANTS: BEGIN OF selection_type,
                 interval TYPE rsrest_opl VALUE '#INTERVAL' ##NO_TEXT,
                 range    TYPE rsrest_opl VALUE '#RANGE' ##NO_TEXT,
                 single   TYPE rsrest_opl VALUE '#SINGLE' ##NO_TEXT,
               END OF selection_type.

    CLASS-DATA st_options_tab TYPE sscr_opt_list_tab.

    DATA title              TYPE sytitle.
    DATA progname           TYPE progname.
    DATA selection_table    TYPE ty_rsparamsl_255_tab.
    DATA selection_mappings TYPE zcds_alv_selopts_mapping_tab.
    DATA parameter_mappings TYPE zcds_alv_parameter_mapping_tab.
    DATA restriction        TYPE sscr_restrict.
    DATA selection_texts    TYPE ty_selection_texts.
    DATA value_help         TYPE REF TO zif_cds_alv_value_help.

    METHODS evaluate_annotations REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF fixed_parameters,
                 max_records TYPE rsscr_name VALUE 'P_MAXREC',
                 no_max      TYPE rsscr_name VALUE 'P_NO_MAX',
                 selection   TYPE rsscr_name VALUE 'P_SELEXT',
                 display     TYPE rsscr_name VALUE 'P_DISEXT',
                 split       TYPE rsscr_name VALUE 'P_SPLIT',
               END OF fixed_parameters.

    CONSTANTS: BEGIN OF modif_id,
                 max TYPE screen-group1 VALUE 'MAX' ##NO_TEXT,
                 sel TYPE screen-group1 VALUE 'SEL' ##NO_TEXT,
                 dis TYPE screen-group1 VALUE 'DIS' ##NO_TEXT,
               END OF modif_id.

    CONSTANTS default_maxrec TYPE ddshmaxrec VALUE 500 ##NO_TEXT.

    METHODS set_new_selections
      RAISING zcx_cds_alv_message.
ENDCLASS.



CLASS zcl_cds_alv_selection_screen IMPLEMENTATION.
  METHOD class_constructor.
    st_options_tab = VALUE #( ( name    = selection_type-single
                                options = VALUE #( eq = abap_true ) )

                              ( name    = selection_type-interval
                                options = VALUE #( eq = abap_true
                                                   bt = abap_true ) )

                              ( name    = selection_type-range
                                options = VALUE #( bt = abap_true
                                                   cp = abap_true
                                                   eq = abap_true
                                                   ge = abap_true
                                                   gt = abap_true
                                                   le = abap_true
                                                   lt = abap_true
                                                   nb = abap_true
                                                   ne = abap_true
                                                   np = abap_true )  )  ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    value_help = i_value_help.

    DATA(program) = persistence->get_report_for_cds_view( cds_view ).

    progname = program-progname.
    parameter_mappings = program-parameters.
    selection_mappings = program-select_options.

    evaluate_annotations( ).
  ENDMETHOD.

  METHOD evaluate_annotations.
    title = description.

    LOOP AT element_annotations ASSIGNING FIELD-SYMBOL(<element_annotation_group>)
         GROUP BY <element_annotation_group>-elementname.

      TYPES: BEGIN OF ty_field_properties,
               selection_type      TYPE string,
               multiple_selections TYPE abap_bool,
               mandatory           TYPE abap_bool,
               default_value       TYPE string,
               label               TYPE string,
             END OF ty_field_properties.

      DATA(fieldname) = CONV fieldname( <element_annotation_group>-elementname ).
      DATA(properties) = VALUE ty_field_properties( ).

      LOOP AT GROUP <element_annotation_group> ASSIGNING FIELD-SYMBOL(<element_annotation>).
        CASE <element_annotation>-annoname.
          WHEN 'CONSUMPTION.FILTER.SELECTIONTYPE'.
            properties-selection_type = <element_annotation>-value.

          WHEN 'CONSUMPTION.FILTER.MULTIPLESELECTIONS'.
            properties-multiple_selections = xsdbool( <element_annotation>-value = 'true' ).

          WHEN 'CONSUMPTION.FILTER.MANDATORY'.
            properties-mandatory = xsdbool( <element_annotation>-value = 'true' ).

          WHEN 'CONSUMPTION.FILTER.DEFAULTVALUE'.
            properties-default_value = <element_annotation>-value.

          WHEN 'ENDUSERTEXT.LABEL'.
            properties-label = remove_quotes( <element_annotation>-value ).
        ENDCASE.
      ENDLOOP.

      TRY.
          DATA(sel_name) = selection_mappings[ cds_view = cds_view fieldname = fieldname ]-sel_name. "#EC CI_HASHSEQ

          IF properties-selection_type IS NOT INITIAL.
            INSERT VALUE #( name = sel_name kind = kind-select_option )
                   INTO TABLE restriction-ass_tab ASSIGNING FIELD-SYMBOL(<restriction_ass>).

            " The options list are named after the annotation values
            <restriction_ass>-op_addy = properties-selection_type.
            <restriction_ass>-op_main = properties-selection_type.

            IF properties-selection_type = selection_type-range.
              <restriction_ass>-sg_addy = sign-all.
              <restriction_ass>-sg_main = sign-all.
            ELSE.
              <restriction_ass>-sg_addy = sign-include.
              <restriction_ass>-sg_main = sign-include.
            ENDIF.

            IF properties-multiple_selections = abap_false.
              <restriction_ass>-sg_addy = sign-no_extensions.
            ENDIF.
          ENDIF.

          " Labels (from annotation EndUserText.Label)
          IF properties-label IS NOT INITIAL.
            INSERT VALUE #( kind = 'S'
                            name = sel_name
                            text = condense( properties-label ) )
                   INTO TABLE selection_texts.
          ELSE.
            READ TABLE ddfields INTO DATA(ddfield)
                 WITH KEY fieldname = fieldname.
            IF sy-subrc = 0.
              INSERT VALUE #( kind = 'S'
                              name = sel_name
                              text = condense( ddfield-scrtext_m ) )
                     INTO TABLE selection_texts.
            ENDIF.
          ENDIF.

        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_new_selections.
    CALL FUNCTION 'SELTAB_2_SELOPTS_255'
      EXPORTING  program = progname
      TABLES     seltab  = selection_table
      EXCEPTIONS OTHERS  = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_condition_provider~get_parameters.
    LOOP AT parameter_mappings INTO DATA(parameter_mapping).
      READ TABLE selection_table INTO DATA(selection)
           WITH KEY selname = parameter_mapping-sel_name.
      IF sy-subrc = 0.
        INSERT VALUE #( cds_view = cds_view
                        parname  = parameter_mapping-parname
                        value    = selection-low )
               INTO TABLE r_parameters.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cds_alv_condition_provider~get_selections.
    CLEAR: e_where_tab,
           e_field_ranges,
           e_maxrec,
           e_no_max.

    LOOP AT selection_table INTO DATA(selection) WHERE kind = kind-select_option.
      READ TABLE selection_mappings INTO DATA(selection_mapping)
           WITH TABLE KEY progname = progname sel_name = selection-selname.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE e_field_ranges ASSIGNING FIELD-SYMBOL(<field_ranges>)
           WITH KEY fieldname = selection_mapping-fieldname.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO e_field_ranges ASSIGNING <field_ranges>.
        <field_ranges>-fieldname = selection_mapping-fieldname.
      ENDIF.

      IF selection-sign IS NOT INITIAL AND selection-option IS NOT INITIAL.
        APPEND CORRESPONDING #( selection ) TO <field_ranges>-selopt_t.
      ENDIF.
    ENDLOOP.

    DATA(field_ranges)  = VALUE rsds_trange( ( tablename = cds_view frange_t = e_field_ranges ) ).
    DATA(where_clauses) = VALUE rsds_twhere( ).

    CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
      EXPORTING field_ranges  = field_ranges
      IMPORTING where_clauses = where_clauses.

    TRY.
        e_where_tab = where_clauses[ tablename = cds_view ]-where_tab.
      CATCH cx_sy_itab_line_not_found.
        CLEAR e_where_tab.
    ENDTRY.

    TRY.
        e_no_max = selection_table[ kind = kind-parameter selname = fixed_parameters-no_max ]-low.
      CATCH cx_sy_itab_line_not_found.
        e_no_max = abap_false.
    ENDTRY.

    IF e_no_max = abap_true.
      e_maxrec = 0.
    ELSE.
      TRY.
          e_maxrec = selection_table[ kind = kind-parameter selname = fixed_parameters-max_records ]-low.
        CATCH cx_sy_itab_line_not_found.
          e_maxrec = default_maxrec.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~apply_restriction.
    CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
      EXPORTING  program                = progname
                 restriction            = restriction
      EXCEPTIONS too_late               = 0
                 repeated               = 0
                 selopt_without_options = 1
                 selopt_without_signs   = 2
                 invalid_sign           = 3
                 empty_option_list      = 4
                 invalid_kind           = 5
                 repeated_kind_a        = 6
                 OTHERS                 = 7.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~apply_selection_texts.
    CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
      EXPORTING  program                     = progname
      TABLES     seltexts                    = selection_texts
      EXCEPTIONS program_not_found           = 1
                 program_cannot_be_generated = 2
                 OTHERS                      = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~get_dynpro_field.
    CLEAR: e_parameter,
           e_select_options.

    LOOP AT selection_table INTO DATA(selection)
         WHERE selname = i_sel_name.

      CASE selection-kind.
        WHEN kind-parameter.
          e_parameter = selection-low.
        WHEN kind-select_option.
          APPEND CORRESPONDING #( selection ) TO e_select_options.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~modify_screen.
    read_selection_screen( ).

    DATA(selection_values) = VALUE vrm_values( FOR x_selection IN persistence->get_report_extensions( i_cds_view = cds_view i_only_selection = abap_true )
                                               ( key = x_selection-extension_name text = x_selection-selection_text ) ).

    IF selection_values IS NOT INITIAL.
      INSERT VALUE #( key = space text = text-sel ) INTO selection_values INDEX 1.
    ENDIF.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING  id     = CONV vrm_id( fixed_parameters-selection )
                 values = selection_values
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      CLEAR selection_values.
    ENDIF.

    DATA(display_values) = VALUE vrm_values( FOR x_display IN persistence->get_report_extensions( i_cds_view = cds_view i_only_display = abap_true )
                                             ( key = x_display-extension_name text = x_display-display_text ) ).

    IF display_values IS NOT INITIAL.
      INSERT VALUE #( key = space text = text-dis ) INTO display_values INDEX 1.
    ENDIF.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING  id     = CONV vrm_id( fixed_parameters-display )
                 values = display_values
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      CLEAR display_values.
    ENDIF.

    TRY.
        DATA(no_max) = CONV abap_bool( selection_table[ selname = fixed_parameters-no_max ]-low ).
      CATCH cx_sy_itab_line_not_found.
        no_max = abap_false.
    ENDTRY.

    DATA(alternative_display) = VALUE zcds_alv_report_extension_name( ).
    get_dynpro_field( EXPORTING i_sel_name  = fixed_parameters-display
                      IMPORTING e_parameter = alternative_display ).

    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN modif_id-max.
          screen-input = SWITCH #( no_max
                                   WHEN abap_true  THEN '0'
                                   WHEN abap_false THEN '1' ).
        WHEN modif_id-sel.
          screen-active = COND #( WHEN selection_values IS INITIAL THEN '0' ELSE '1' ).
        WHEN modif_id-dis.
          screen-active = COND #( WHEN display_values IS INITIAL THEN '0' ELSE '1' ).
      ENDCASE.

      IF screen-name = fixed_parameters-split.
        screen-input = COND #( WHEN alternative_display IS INITIAL THEN '1' ELSE '0' ).
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~read_selection_screen.
    DATA rsparams_tab TYPE rsparams_tt.

    rsparams_tab = CORRESPONDING #( selection_table ).
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING  curr_report         = progname
      TABLES     selection_table     = rsparams_tab
                 selection_table_255 = selection_table
      EXCEPTIONS OTHERS              = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~set_dynpro_field.
    IF i_parameter IS NOT INITIAL.
      selection_table[ selname = i_sel_name ]-low = i_parameter.
    ELSEIF i_select_options IS NOT INITIAL.
      DELETE selection_table WHERE selname = i_sel_name.
      LOOP AT i_select_options INTO DATA(select_option).
        INSERT CORRESPONDING #( BASE ( VALUE #( selname = i_sel_name kind = 'S' ) ) select_option )
               INTO TABLE selection_table.
      ENDLOOP.
    ENDIF.

    set_new_selections( ).
  ENDMETHOD.

  METHOD zif_cds_alv_selection_screen~value_help_for_field.
    IF value_help IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(fieldname) = selection_mappings[ progname = progname sel_name = i_sel_name ]-fieldname.

        get_selections( IMPORTING e_field_ranges = DATA(field_ranges) ).
        DATA(parameters) = get_parameters( ).

        value_help->value_help_for_element( EXPORTING i_fieldname    = fieldname
                                                      i_parameters   = parameters
                                                      i_field_ranges = field_ranges
                                            CHANGING  c_value        = c_value ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    TRY.
        DATA(parameter_name) = parameter_mappings[ progname = progname sel_name = i_sel_name ]-parname.

        value_help->value_help_for_parameter( EXPORTING i_parname = parameter_name
                                              CHANGING  c_value   = c_value ).

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
