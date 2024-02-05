CLASS zcl_cds_alv_bopf_handler DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_bopf_handler.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    METHODS: evaluate_annotations REDEFINITION.

  PRIVATE SECTION.
    DATA sadl_definition     TYPE if_sadl_types=>ty_sadl_definition.
    DATA entity              TYPE REF TO if_sadl_entity.
    DATA runtime             TYPE REF TO if_sadl_entity_transactional.
    DATA transaction_manager TYPE REF TO cl_sadl_transaction_manager.

    METHODS ask_for_parameters
      CHANGING c_parameters TYPE any
      RAISING  zcx_cds_alv_message.

    METHODS map_attributes_single
      IMPORTING i_entity_key TYPE any OPTIONAL
                i_attributes TYPE if_sadl_types=>tt_sadl_attributes
      EXPORTING e_source_key TYPE any
      RAISING   zcx_cds_alv_message.

    METHODS map_attributes_table
      IMPORTING i_entity_keys TYPE STANDARD TABLE OPTIONAL
                i_attributes  TYPE if_sadl_types=>tt_sadl_attributes
      EXPORTING e_source_keys TYPE STANDARD TABLE
      RAISING   zcx_cds_alv_message.

    METHODS instantiate
      RAISING zcx_cds_alv_message.
ENDCLASS.



CLASS ZCL_CDS_ALV_BOPF_HANDLER IMPLEMENTATION.


  METHOD ask_for_parameters.
    DATA fields TYPE STANDARD TABLE OF sval.

    DATA(descriptor) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( c_parameters ) ).
    IF descriptor->is_ddic_type( ).
      fields = VALUE #( FOR x_ddic_field IN descriptor->get_ddic_field_list( )
                        ( tabname   = x_ddic_field-tabname
                          fieldname = x_ddic_field-fieldname
                          fieldtext = x_ddic_field-scrtext_m ) ).
    ELSE.
      fields = VALUE #( FOR x_component IN descriptor->components
                        ( tabname   = descriptor->get_relative_name( )
                          fieldname = x_component-name
                          fieldtext = x_component-name ) ).
    ENDIF.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING  popup_title     = 'Parameter eingeben'
      TABLES     fields          = fields
      EXCEPTIONS error_in_fields = 1
                 OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT fields INTO DATA(field).
      ASSIGN COMPONENT field-fieldname OF STRUCTURE c_parameters TO FIELD-SYMBOL(<parameter>).
      IF sy-subrc = 0.
        <parameter> = field-value.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    evaluate_annotations( ).
  ENDMETHOD.


  METHOD evaluate_annotations.
    " not used yet
  ENDMETHOD.


  METHOD instantiate.
    TRY.
        DATA(entity_id) = CONV sadl_entity_id( cds_view ).
        entity = cl_sadl_entity_factory=>get_instance( )->get_entity( iv_type = cl_sadl_entity_factory=>co_type-cds
                                                                      iv_id   = entity_id ).

        " This gets the correct entity, when ObjectModel.transactionalProcessingDelegated is used!
        DATA(source_id) = ddic_access->get_source_id( entity ).

        runtime = cl_sadl_crud_runtime_util=>get_ta_runtime(
                      iv_entity_id   = COND #( WHEN source_id IS NOT INITIAL THEN source_id ELSE entity_id )
                      iv_entity_type = cl_sadl_entity_factory=>co_type-cds ).

        transaction_manager = cl_sadl_transaction_manager=>get_transaction_manager( ).

        IF source_id IS NOT INITIAL.
          DATA(mp) = ddic_access->get_mp_for_entity( entity ).

          DATA(entity_type_trans_manager) =
            cl_sadl_entity_trans_factory=>get_transactional_provider( cl_sadl_entity_factory=>co_type-bopf
              )->get_transaction_manager( cl_sadl_entity_factory=>co_type-bopf ).

          transaction_manager->register_transaction_framework( io_transaction_manager = entity_type_trans_manager
                                                               iv_entity_type         = cl_sadl_entity_factory=>co_type-bopf
                                                               io_mdp                 = cl_sadl_mdp_factory=>get_mdp_for_mp( mp ) ).
        ENDIF.

      CATCH cx_sadl_static cx_sadl_contract_violation INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING
            previous = previous.
    ENDTRY.
  ENDMETHOD.


  METHOD map_attributes_single.
    CLEAR e_source_key.

    " Use Mapping, if it exists, else MOVE-CORRESPONDING
    LOOP AT i_attributes ASSIGNING FIELD-SYMBOL(<attribute>).
      ASSIGN COMPONENT <attribute>-name OF STRUCTURE i_entity_key TO FIELD-SYMBOL(<source>).
      ASSIGN COMPONENT <attribute>-binding OF STRUCTURE e_source_key TO FIELD-SYMBOL(<target>).

      IF <source> IS ASSIGNED AND <target> IS ASSIGNED.
        <target> = <source>.
      ENDIF.

      UNASSIGN: <source>, <target>.
    ENDLOOP.
    IF sy-subrc <> 0.
      MOVE-CORRESPONDING i_entity_key TO e_source_key.
    ENDIF.
  ENDMETHOD.


  METHOD map_attributes_table.
    CLEAR e_source_keys.

    LOOP AT i_entity_keys ASSIGNING FIELD-SYMBOL(<entity_key>).
      APPEND INITIAL LINE TO e_source_keys ASSIGNING FIELD-SYMBOL(<source_key>).
      map_attributes_single( EXPORTING i_entity_key = <entity_key>
                                       i_attributes = i_attributes
                             IMPORTING e_source_key = <source_key> ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_cds_alv_bopf_handler~delete.
    DATA key_values TYPE REF TO data.

    TRY.
        instantiate( ).
        key_values = runtime->create_entity_table_ref( ).
        ASSIGN key_values->* TO FIELD-SYMBOL(<key_values>).
        map_attributes_table( EXPORTING i_entity_keys = i_selected_rows
                                        i_attributes  = sadl_definition-attributes
                              IMPORTING e_source_keys = <key_values> ).

        runtime->delete( EXPORTING it_key_values = <key_values>
                         IMPORTING et_failed     = DATA(failed_keys) ).

        DATA(failed) = xsdbool( failed_keys IS NOT INITIAL ).

        IF failed = abap_false.
          failed = transaction_manager->if_sadl_transaction_manager~save( ).
        ENDIF.

        IF failed = abap_true.
          transaction_manager->if_sadl_transaction_manager~discard_changes( ).
        ENDIF.

      CATCH cx_sadl_contract_violation cx_sadl_static INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cds_alv_bopf_handler~execute_action.
    DATA parameters     TYPE REF TO data.
    DATA key_values     TYPE REF TO data.
    DATA key_values_tab TYPE REF TO data.
    DATA return         TYPE REF TO data.
    DATA failed         TYPE abap_bool.

    FIELD-SYMBOLS <parameters> TYPE any.
    FIELD-SYMBOLS <return>     TYPE STANDARD TABLE.

    TRY.
        instantiate( ).
        DATA(action) = entity->get_action( to_upper( i_action ) ).

        " Parameters
        IF action-data_type IS NOT INITIAL.
          parameters = runtime->create_action_parameter_ref( action-name ).
          ASSIGN parameters->* TO <parameters>.
          ask_for_parameters( CHANGING c_parameters = <parameters> ).
        ELSE.
          CREATE DATA parameters TYPE char1.
          ASSIGN parameters->* TO <parameters>.
        ENDIF.

        " Return Type
        IF action-return_entity_id IS NOT INITIAL.
          return = cl_sadl_crud_runtime_util=>get_ta_runtime(
                       iv_entity_id   = CONV sadl_entity_id( action-return_entity_id )
                       iv_entity_type = cl_sadl_entity_factory=>co_type-cds )->create_entity_table_ref( ).
          ASSIGN return->* TO <return>.
        ELSEIF action-return_table_type IS NOT INITIAL.
          CREATE DATA return TYPE STANDARD TABLE OF (action-return_table_type).
          ASSIGN return->* TO <return>.
        ELSE.
          CREATE DATA return TYPE STANDARD TABLE OF char1.
          ASSIGN return->* TO <return>.
        ENDIF.

        " Execution
*        CASE action-static.
*          WHEN abap_false.
*            LOOP AT i_selected_rows ASSIGNING FIELD-SYMBOL(<selected_row>).
*              key_values = runtime->create_entity_structure_ref( ).
*              ASSIGN key_values->* TO FIELD-SYMBOL(<key_values>).
*              map_attributes_single( EXPORTING i_entity_key = <selected_row>
*                                               i_attributes = sadl_definition-attributes
*                                     IMPORTING e_source_key = <key_values> ).
*
*              runtime->execute_single( EXPORTING iv_action_name      = action-name
*                                                 i_action_parameters = <parameters>
*                                                 is_key_values       = <key_values>
*                                       IMPORTING ev_failed           = failed
*                                                 et_data             = <return> ).
*
*              IF failed = abap_false.
*                failed = transaction_manager->if_sadl_transaction_manager~save( ).
*              ENDIF.
*
*              IF failed = abap_true.
*                transaction_manager->if_sadl_transaction_manager~discard_changes( ).
*              ENDIF.
*            ENDLOOP.
*
*          WHEN abap_true.
        key_values_tab = runtime->create_entity_table_ref( ).
        ASSIGN key_values_tab->* TO FIELD-SYMBOL(<key_values_tab>).
        map_attributes_table( EXPORTING i_entity_keys = i_selected_rows
                                        i_attributes  = sadl_definition-attributes
                              IMPORTING e_source_keys = <key_values_tab> ).

        runtime->execute( EXPORTING iv_action_name          = action-name
                                    i_action_parameters     = <parameters>
                                    it_key_values           = <key_values_tab>
                          IMPORTING ev_static_action_failed = failed
                                    et_data                 = <return> ).

        IF failed = abap_false.
          failed = transaction_manager->if_sadl_transaction_manager~save( ).
        ENDIF.

        IF failed = abap_true.
          transaction_manager->if_sadl_transaction_manager~discard_changes( ).
        ENDIF.
*        ENDCASE.

        e_refresh_after = abap_true.

      CATCH cx_sadl_contract_violation cx_sadl_static INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING
            previous = previous.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cds_alv_bopf_handler~update.
    DATA entity_data TYPE REF TO data.

    TRY.
        instantiate( ).
        entity_data = runtime->create_entity_table_ref( ).
        ASSIGN entity_data->* TO FIELD-SYMBOL(<entity_data>).
        map_attributes_table( EXPORTING i_entity_keys = i_selected_rows
                                        i_attributes  = sadl_definition-attributes
                              IMPORTING e_source_keys = <entity_data> ).

        runtime->update( EXPORTING it_entity_data = <entity_data>
                         IMPORTING et_failed      = DATA(failed_keys) ).

        DATA(failed) = xsdbool( failed_keys IS NOT INITIAL ).

        IF failed = abap_false.
          failed = transaction_manager->if_sadl_transaction_manager~save( ).
        ENDIF.

        IF failed = abap_true.
          transaction_manager->if_sadl_transaction_manager~discard_changes( ).
        ENDIF.

      CATCH cx_sadl_contract_violation cx_sadl_static INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
