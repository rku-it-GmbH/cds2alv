CLASS zcl_cds_alv_value_help DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_value_help.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    DATA value_helps TYPE zcds_alv_valuehelp_definitions.

    METHODS evaluate_annotations REDEFINITION.

    METHODS value_help_by_association
      IMPORTING i_fieldname        TYPE fieldname
                i_association_name TYPE ddassociationname
                i_parameters       TYPE zcds_alv_parameters OPTIONAL
                i_field_ranges     TYPE rsds_frange_t       OPTIONAL
                i_selected_row     TYPE any                 OPTIONAL
                i_display          TYPE abap_bool
      CHANGING  c_value            TYPE any
      RAISING   zcx_cds_alv_message.

    METHODS value_help_by_target_entity
      IMPORTING i_target_entity      TYPE ddstrucobjname
                i_target_element     TYPE fieldname
                i_additional_binding TYPE zcds_alv_valuehelp_binding_tab OPTIONAL
                i_parameters         TYPE zcds_alv_parameters            OPTIONAL
                i_field_ranges       TYPE rsds_frange_t                  OPTIONAL
                i_selected_row       TYPE any                            OPTIONAL
                i_display            TYPE abap_bool
      CHANGING  c_value              TYPE any
      RAISING   zcx_cds_alv_message.

    METHODS get_value_from_dialog
      IMPORTING i_fieldname TYPE fieldname
                i_table     TYPE STANDARD TABLE
                i_display   TYPE abap_bool
      CHANGING  c_value     TYPE any
      RAISING   zcx_cds_alv_message.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_cds_alv_value_help IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    evaluate_annotations( ).
  ENDMETHOD.

  METHOD evaluate_annotations.
    LOOP AT element_annotations ASSIGNING FIELD-SYMBOL(<element_annotation_group>)
         GROUP BY <element_annotation_group>-elementname.

      DATA(fieldname) = CONV fieldname( <element_annotation_group>-elementname ).
      DATA(value_help) = VALUE zcds_alv_valuehelp_definition( cds_view = cds_view fieldname = fieldname ).

      LOOP AT GROUP <element_annotation_group> ASSIGNING FIELD-SYMBOL(<element_annotation>).
        CASE <element_annotation>-annoname.
          WHEN 'CONSUMPTION.VALUEHELP' OR 'CONSUMPTION.VALUEHELPDEFINITION.ASSOCIATION'.
            value_help-association_name = remove_quotes( <element_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ENTITY.NAME'.
            value_help-target_entity = remove_quotes( <element_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ENTITY.ELEMENT'.
            value_help-target_element = remove_quotes( <element_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.DISTINCTVALUES'.
            value_help-distinct_values = xsdbool( <element_annotation>-value = 'true' ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.LABEL'.
            value_help-label = remove_quotes( <element_annotation>-value ).
        ENDCASE.

        IF <element_annotation>-annoname NP 'CONSUMPTION.VALUEHELPDEFINITION.ADDITIONALBINDING$*$.*'.
          CONTINUE.
        ENDIF.

        SPLIT <element_annotation>-annoname AT '$' INTO DATA(prefix) DATA(index) DATA(property).
        READ TABLE value_help-additional_binding ASSIGNING FIELD-SYMBOL(<additional_binding>)
             WITH KEY index = index.
        IF sy-subrc <> 0.
          INSERT VALUE #( index = index ) INTO TABLE value_help-additional_binding ASSIGNING <additional_binding>.
        ENDIF.

        CASE |{ prefix }{ property }|.
          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ADDITIONALBINDING.LOCALPARAMETER'.
            <additional_binding>-source_parameter = remove_quotes( <element_annotation>-value ).
          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ADDITIONALBINDING.LOCALELEMENT'.
            <additional_binding>-source_element = remove_quotes( <element_annotation>-value ).
          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ADDITIONALBINDING.PARAMETER'.
            <additional_binding>-target_parameter = remove_quotes( <element_annotation>-value ).
          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ADDITIONALBINDING.ELEMENT'.
            <additional_binding>-target_element = remove_quotes( <element_annotation>-value ).
          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ADDITIONALBINDING.USAGE'.
            <additional_binding>-usage = <element_annotation>-value.
        ENDCASE.
      ENDLOOP.

      IF    value_help-association_name IS NOT INITIAL
         OR (     value_help-target_entity  IS NOT INITIAL
              AND value_help-target_element IS NOT INITIAL ).
        INSERT value_help INTO TABLE value_helps.
      ENDIF.
    ENDLOOP.

    LOOP AT parameter_annotations ASSIGNING FIELD-SYMBOL(<parameter_annotation_group>)
         GROUP BY <parameter_annotation_group>-parametername.

      DATA(parameter_name) = CONV ddparname( <parameter_annotation_group>-parametername ).
      value_help = VALUE zcds_alv_valuehelp_definition( cds_view = cds_view parameter_name = parameter_name ).

      LOOP AT GROUP <parameter_annotation_group> ASSIGNING FIELD-SYMBOL(<parameter_annotation>).
        CASE <parameter_annotation>-annoname.
          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ENTITY.NAME'.
            value_help-target_entity = remove_quotes( <parameter_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.ENTITY.ELEMENT'.
            value_help-target_element = remove_quotes( <parameter_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.DISTINCTVALUES'.
            value_help-distinct_values = xsdbool( <parameter_annotation>-value = 'true' ).

          WHEN 'CONSUMPTION.VALUEHELPDEFINITION.LABEL'.
            value_help-label = remove_quotes( <parameter_annotation>-value ).
        ENDCASE.
      ENDLOOP.

      IF     value_help-target_entity  IS NOT INITIAL
         AND value_help-target_element IS NOT INITIAL.
        INSERT value_help INTO TABLE value_helps.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_value_from_dialog.
    DATA value  TYPE dynfieldvalue.
    DATA return TYPE STANDARD TABLE OF ddshretval.

    value = c_value.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING  retfield        = i_fieldname
                 value           = value
                 value_org       = 'S'
                 display         = i_display
      TABLES     value_tab       = i_table
                 return_tab      = return
      EXCEPTIONS parameter_error = 1
                 no_values_found = 2
                 OTHERS          = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF return IS NOT INITIAL.
      c_value = return[ 1 ]-fieldval.
    ENDIF.
  ENDMETHOD.

  METHOD value_help_by_association.
    DATA table TYPE REF TO data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    TRY.
        DATA(target_view) = ddic_access->get_target_for_association( i_source_view      = cds_view
                                                                     i_association_name = i_association_name ).

        CREATE DATA table TYPE TABLE OF (target_view).
        ASSIGN table->* TO <table>.

        DATA(sadl_runtime) = ddic_access->get_sadl_runtime( target_view ).

        DATA(requested) = VALUE if_sadl_query_engine_types=>ty_requested(
                                    elements  = VALUE #( ( CONV #( i_fieldname ) ) )
                                    fill_data = abap_true ).

        " Sadly I know of no way to provide target parameters, e.g. by propagating source parameters at this point
        DATA(parameters) = VALUE if_sadl_query_engine_types=>ty_parameters( ).
        " TODO: variable is assigned but never used (ABAP cleaner)
        APPEND VALUE #( entity_alias = target_view ) TO parameters-entity ASSIGNING FIELD-SYMBOL(<entity>).

        " The association's join conditions are used to derive a where condition.
        " There are certain limits to this that may be adressed in future versions.
        " Field ranges from the selection screen are not used in this
        " as the logic for the where condition would quickly become way too messy.
        DATA(sadl_condition_provider) = NEW lcl_sadl_cond_provider_assoc( i_ddic_access       = ddic_access
                                                                          i_source_view       = cds_view
                                                                          i_association_name  = i_association_name
                                                                          i_source_parameters = i_parameters
                                                                          i_selected_row      = i_selected_row ).

        sadl_runtime->if_sadl_query_fetch~register_condition_provider( sadl_condition_provider ).

        sadl_runtime->fetch( EXPORTING is_requested  = requested
                                       is_parameters = parameters
                             IMPORTING et_data_rows  = <table> ).

        get_value_from_dialog( EXPORTING i_fieldname = i_fieldname
                                         i_table     = <table>
                                         i_display   = i_display
                               CHANGING  c_value     = c_value ).

      CATCH cx_sadl_static cx_sadl_contract_violation INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD value_help_by_target_entity.
    DATA table TYPE REF TO data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA(target_entity)  = to_upper( i_target_entity  ).
    DATA(target_element) = to_upper( i_target_element ).

    TRY.
        CREATE DATA table TYPE TABLE OF (target_entity).
        ASSIGN table->* TO <table>.

        DATA(sadl_runtime) = ddic_access->get_sadl_runtime( CONV #( target_entity ) ).

        DATA(ddfields) = ddic_access->get_ddic_fields_for_cds_view( CONV #( target_entity ) ).
        DATA(elements) = VALUE if_sadl_query_engine_types=>tt_requested_elements( FOR ddfield IN ddfields
                                                                                  ( CONV #( ddfield-fieldname ) ) ).
        DATA(requested) = VALUE if_sadl_query_engine_types=>ty_requested( elements  = elements
                                                                          fill_data = abap_true ).

        DATA(parameters) = VALUE if_sadl_query_engine_types=>ty_parameters( ).
        APPEND VALUE #( entity_alias = cl_sadl_entity_util=>convert( CONV #( target_entity ) ) )
               TO parameters-entity ASSIGNING FIELD-SYMBOL(<entity>).

        LOOP AT i_additional_binding INTO DATA(binding).
          DATA(target_field_range) = VALUE rsds_frange( fieldname = binding-target_element ).
          DATA(source_field_range) = VALUE rsds_frange( fieldname = binding-source_element ).

          " Element -> Element und Parameter -> Parameter
          IF binding-source_element IS NOT INITIAL.
            IF i_selected_row IS NOT INITIAL.
              ASSIGN COMPONENT binding-source_element OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<source_element>).
              IF sy-subrc = 0.
                DATA(select_options) = VALUE rsds_selopt_t( (  sign = 'I' option = 'EQ' low = <source_element> ) ).
                source_field_range = VALUE #( fieldname = binding-source_element selopt_t = select_options ).
              ENDIF.
            ELSEIF i_field_ranges IS NOT INITIAL.
              READ TABLE i_field_ranges INTO source_field_range
                   WITH KEY fieldname = binding-source_element.
            ENDIF.
            target_field_range-selopt_t = source_field_range-selopt_t.
          ELSEIF binding-source_parameter IS NOT INITIAL.
            READ TABLE i_parameters INTO DATA(source_parameter)
                 WITH TABLE KEY cds_view = cds_view parname = binding-source_parameter.
          ENDIF.

          IF binding-target_element IS NOT INITIAL.
            IF binding-usage <> '#RESULT'.
              IF target_field_range-selopt_t IS NOT INITIAL.
                sadl_runtime->if_sadl_query_fetch~register_condition_provider(
                                                                               cl_sadl_cond_prov_factory_pub=>create_basic_condition_factory(
                                                                               )->in_range(
                                                                                   name     = CONV #( target_field_range-fieldname )
                                                                                   t_ranges = CORRESPONDING #( target_field_range-selopt_t ) ) ).
              ENDIF.
            ENDIF.

            IF binding-usage <> '#FILTER'.
              APPEND CONV #( binding-target_element ) TO requested-elements.
            ENDIF.

          ELSEIF binding-target_parameter IS NOT INITIAL.
            INSERT VALUE #( name  = binding-target_parameter
                            value = source_parameter-value )
                   INTO TABLE <entity>-parameters.
          ENDIF.
        ENDLOOP.

        sadl_runtime->fetch( EXPORTING is_requested  = requested
                                       is_parameters = parameters
                             IMPORTING et_data_rows  = <table> ).

        get_value_from_dialog( EXPORTING i_fieldname = CONV #( target_element )
                                         i_table     = <table>
                                         i_display   = i_display
                               CHANGING  c_value     = c_value ).

      CATCH cx_sadl_static cx_sadl_contract_violation INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_value_help~value_help_for_element.
    TRY.
        e_processed = abap_false.
        DATA(value_help) = value_helps[ fieldname = i_fieldname ].

        IF     value_help-target_entity  IS NOT INITIAL
           AND value_help-target_element IS NOT INITIAL.
          value_help_by_target_entity( EXPORTING i_target_entity      = value_help-target_entity
                                                 i_target_element     = value_help-target_element
                                                 i_additional_binding = value_help-additional_binding
                                                 i_parameters         = i_parameters
                                                 i_field_ranges       = i_field_ranges
                                                 i_selected_row       = i_selected_row
                                                 i_display            = i_display
                                       CHANGING  c_value              = c_value ).

          e_processed = abap_true.

        ELSEIF value_help-association_name IS NOT INITIAL.
          value_help_by_association( EXPORTING i_fieldname        = i_fieldname
                                               i_association_name = value_help-association_name
                                               i_parameters       = i_parameters
                                               i_field_ranges     = i_field_ranges
                                               i_selected_row     = i_selected_row
                                               i_display          = i_display
                                     CHANGING  c_value            = c_value ).

          e_processed = abap_true.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        e_processed = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_value_help~value_help_for_parameter.
    TRY.
        e_processed = abap_false.
        DATA(value_help) = value_helps[ parameter_name = i_parname ].

        IF     value_help-target_entity  IS NOT INITIAL
           AND value_help-target_element IS NOT INITIAL.
          value_help_by_target_entity( EXPORTING i_target_entity  = value_help-target_entity
                                                 i_target_element = value_help-target_element
                                                 i_display        = i_display
                                       CHANGING  c_value          = c_value ).

          e_processed = abap_true.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        e_processed = abap_false.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
