CLASS zcl_cds_alv_selection DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_selection.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    TYPES ty_sadl_condition_providers TYPE STANDARD TABLE OF REF TO if_sadl_condition_provider WITH EMPTY KEY.

    DATA: BEGIN OF last_request,
            exists                   TYPE abap_bool,
            parameters               TYPE if_sadl_query_engine_types=>ty_parameters,
            paging                   TYPE if_sadl_query_engine_types=>ty_paging,
            requested                TYPE if_sadl_query_engine_types=>ty_requested,
            sadl_condition_providers TYPE ty_sadl_condition_providers,
          END OF last_request.

    METHODS evaluate_annotations REDEFINITION.

    METHODS save_request
      IMPORTING i_parameters               TYPE if_sadl_query_engine_types=>ty_parameters OPTIONAL
                i_paging                   TYPE if_sadl_query_engine_types=>ty_paging     OPTIONAL
                i_requested                TYPE if_sadl_query_engine_types=>ty_requested  OPTIONAL
                i_sadl_condition_providers TYPE ty_sadl_condition_providers               OPTIONAL.

    METHODS get_last_request
      EXPORTING e_exists                   TYPE abap_bool
                e_parameters               TYPE if_sadl_query_engine_types=>ty_parameters
                e_paging                   TYPE if_sadl_query_engine_types=>ty_paging
                e_requested                TYPE if_sadl_query_engine_types=>ty_requested
                e_sadl_condition_providers TYPE ty_sadl_condition_providers.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cds_alv_selection IMPLEMENTATION.
  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    evaluate_annotations( ).
  ENDMETHOD.

  METHOD evaluate_annotations.
    " TODO: Custom implementation for virtual elements
  ENDMETHOD.

  METHOD get_last_request.
    e_exists = last_request-exists.
    e_parameters = last_request-parameters.
    e_paging = last_request-paging.
    e_requested = last_request-requested.
    e_sadl_condition_providers = last_request-sadl_condition_providers.
  ENDMETHOD.

  METHOD save_request.
    last_request-exists                   = abap_true.
    last_request-parameters               = i_parameters.
    last_request-paging                   = i_paging.
    last_request-requested                = i_requested.
    last_request-sadl_condition_providers = i_sadl_condition_providers.
  ENDMETHOD.

  METHOD zif_cds_alv_selection~perform_reselection.
    get_last_request( IMPORTING e_exists                   = DATA(exists)
                                e_parameters               = DATA(parameters)
                                e_paging                   = DATA(paging)
                                e_requested                = DATA(requested)
                                e_sadl_condition_providers = DATA(sadl_condition_providers) ).

    IF exists = abap_false.
      RETURN.
    ENDIF.

    TRY.
        DATA(sadl_runtime) = ddic_access->get_sadl_runtime( cds_view ).

        LOOP AT sadl_condition_providers INTO DATA(sadl_condition_provider).
          sadl_runtime->if_sadl_query_fetch~register_condition_provider( sadl_condition_provider ).
        ENDLOOP.

        sadl_runtime->fetch( EXPORTING is_requested  = requested
                                       is_paging     = paging
                                       is_parameters = parameters
                             IMPORTING et_data_rows  = c_result_table ).

      CATCH cx_sadl_static cx_sadl_contract_violation INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_selection~perform_selection.
    CLEAR: e_result_table,
           e_number_all_hits.

    TRY.
        DATA(alv_parameters) = i_condition_provider->get_parameters( ).
        IF alv_parameters IS NOT INITIAL.
          DATA(parameters) = VALUE if_sadl_query_engine_types=>ty_parameters(
              entity = VALUE #( ( entity_alias = cl_sadl_entity_util=>convert( CONV #( cds_view ) )
                                  parameters   = CORRESPONDING #( alv_parameters MAPPING name = parname ) ) ) ).
        ENDIF.

        i_condition_provider->get_selections( IMPORTING e_field_ranges = DATA(field_ranges)
                                                        e_maxrec       = DATA(max_records) ).

        DATA(sadl_condition_providers) = VALUE ty_sadl_condition_providers(
                                                   FOR field_range IN field_ranges
                                                   ( cl_sadl_cond_prov_factory_pub=>create_basic_condition_factory(
                                                       )->in_range(
                                                           name     = CONV #( field_range-fieldname )
                                                           t_ranges = CORRESPONDING #( field_range-selopt_t ) ) )  ).

        DATA(paging) = VALUE if_sadl_query_engine_types=>ty_paging( maximum_rows = max_records ).

        DATA(ddfields) = ddic_access->get_ddic_fields_for_cds_view( cds_view ).
        DATA(elements) = VALUE if_sadl_query_engine_types=>tt_requested_elements( FOR ddfield IN ddfields
                                                                                  ( CONV #( ddfield-fieldname ) ) ).

        DATA(requested) = VALUE if_sadl_query_engine_types=>ty_requested( elements             = elements
                                                                          fill_data            = abap_true
                                                                          fill_number_all_hits = abap_true ).

        " Selection via Runtime
        DATA(sadl_runtime) = ddic_access->get_sadl_runtime( cds_view ).

        LOOP AT sadl_condition_providers INTO DATA(sadl_condition_provider).
          sadl_runtime->if_sadl_query_fetch~register_condition_provider( sadl_condition_provider ).
        ENDLOOP.

        sadl_runtime->fetch( EXPORTING is_paging          = paging
                                       is_parameters      = parameters
                                       is_requested       = requested
                             IMPORTING et_data_rows       = e_result_table
                                       ev_number_all_hits = e_number_all_hits ).

        " save parameters for reselection
        save_request( i_parameters               = parameters
                      i_paging                   = paging
                      i_requested                = requested
                      i_sadl_condition_providers = sadl_condition_providers ).

      CATCH cx_sadl_static cx_sadl_contract_violation INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_selection~perform_selection_forall.
    TRY.
        DATA(parameters) = VALUE if_sadl_query_engine_types=>ty_parameters(
            entity = VALUE #(
                ( entity_alias = cl_sadl_entity_util=>convert( CONV #( cds_view ) )
                  parameters   = CORRESPONDING #( i_condition_provider->get_parameters( ) MAPPING name = parname ) ) ) ).

        DATA(sadl_condition_provider) = NEW lcl_sadl_cond_provider_forall( i_ddic_access       = ddic_access
                                                                           i_source_view       = i_source_view
                                                                           i_association_name  = i_association_name
                                                                           i_source_parameters = i_source_parameters
                                                                           i_forall_table      = i_forall_table ).

        DATA(sadl_condition_providers) = VALUE ty_sadl_condition_providers( ( sadl_condition_provider ) ).

        DATA(ddfields) = ddic_access->get_ddic_fields_for_cds_view( cds_view ).
        DATA(elements) = VALUE if_sadl_query_engine_types=>tt_requested_elements( FOR ddfield IN ddfields
                                                                                  ( CONV #( ddfield-fieldname ) ) ).

        DATA(requested) = VALUE if_sadl_query_engine_types=>ty_requested( elements             = elements
                                                                          fill_data            = abap_true
                                                                          fill_number_all_hits = abap_true ).

        " Selection via Runtime
        DATA(sadl_runtime) = ddic_access->get_sadl_runtime( cds_view ).

        sadl_runtime->if_sadl_query_fetch~register_condition_provider( sadl_condition_provider ).

        sadl_runtime->fetch( EXPORTING is_parameters      = parameters
                                       is_requested       = requested
                             IMPORTING et_data_rows       = e_result_table
                                       ev_number_all_hits = e_number_all_hits  ).

        " save parameters for reselection
        save_request( i_parameters               = parameters
                      i_requested                = requested
                      i_sadl_condition_providers = sadl_condition_providers ).

      CATCH cx_sadl_static cx_sadl_contract_violation INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
