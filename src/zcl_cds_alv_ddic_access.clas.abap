CLASS zcl_cds_alv_ddic_access DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_ddic_access.

  PROTECTED SECTION.

  PRIVATE SECTION.
    ALIASES exists_view                    FOR zif_cds_alv_ddic_access~exists_view.
    ALIASES get_annotations_for_cds_view   FOR zif_cds_alv_ddic_access~get_annotations_for_cds_view.
    ALIASES get_conditions_for_association FOR zif_cds_alv_ddic_access~get_conditions_for_association.
    ALIASES get_ddic_fields_for_cds_view   FOR zif_cds_alv_ddic_access~get_ddic_fields_for_cds_view.
    ALIASES get_last_modified_at           FOR zif_cds_alv_ddic_access~get_last_modified_at.
    ALIASES get_parameters_for_cds_view    FOR zif_cds_alv_ddic_access~get_parameters_for_cds_view.
    ALIASES get_sadl_runtime               FOR zif_cds_alv_ddic_access~get_sadl_runtime.
    ALIASES get_target_for_association     FOR zif_cds_alv_ddic_access~get_target_for_association.
    ALIASES get_mp_for_entity              FOR zif_cds_alv_ddic_access~get_mp_for_entity.

ENDCLASS.



CLASS zcl_cds_alv_ddic_access IMPLEMENTATION.
  METHOD zif_cds_alv_ddic_access~exists_view.
    SELECT SINGLE @abap_true FROM dd02b
     WHERE strucobjn = @i_cds_view
      INTO @r_exists.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_annotations_for_cds_view.
    cl_dd_ddl_annotation_service=>get_annos( EXPORTING entityname      = to_upper( i_cds_view )
                                             IMPORTING entity_annos    = e_entity_annotations
                                                       element_annos   = e_element_annotations
                                                       parameter_annos = e_parameter_annotations ).
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_conditions_for_association.
    TRY.
        DATA dd_object TYPE REF TO if_dd_sobject.

        dd_object = NEW cl_dd_sobject( ).
        dd_object->read( EXPORTING sobjnames  = VALUE #( ( i_source_view ) )
                         IMPORTING dd08bv_tab = e_dd08bv_tab
                                   dd05bv_tab = e_dd05bv_tab ).

        READ TABLE e_dd08bv_tab INTO DATA(dd08bv)
             WITH KEY assocname_raw = i_association_name.

        e_target_view = dd08bv-strucobjn_t.
        DELETE e_dd08bv_tab WHERE associationname <> dd08bv-associationname.
        DELETE e_dd05bv_tab WHERE associationname <> dd08bv-associationname.

        " This removes conditions on the source and target view to allow
        " Navigation via association in cases where no FORALL table is available.
        " The implementation is somewhat ad hoc and may well need future adjustment
        " for complex conditions that are not correctly rearranged by it.
        IF i_only_constants = abap_true.
          SORT e_dd05bv_tab BY fdposition DESCENDING.
          LOOP AT e_dd05bv_tab ASSIGNING FIELD-SYMBOL(<curr_dd05bv>).
            DATA(curr_tabix) = sy-tabix.

            IF <curr_dd05bv>-side_info CO 'LPS'.
              CONTINUE.
            ENDIF.

            READ TABLE e_dd05bv_tab INDEX ( curr_tabix + 1 ) ASSIGNING FIELD-SYMBOL(<prev_dd05bv>).
            IF sy-subrc = 0.
              IF <curr_dd05bv>-and_or = 'AND' AND <prev_dd05bv>-and_or = 'OR'.
                <prev_dd05bv>-and_or = 'AND'.
              ELSEIF <curr_dd05bv>-and_or IS INITIAL.
                CLEAR <prev_dd05bv>-and_or.
              ENDIF.
            ENDIF.

            DELETE e_dd05bv_tab INDEX curr_tabix.
            CONTINUE.
          ENDLOOP.
        ENDIF.

      CATCH cx_dd_sobject_get INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_ddic_fields_for_cds_view.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING  tabname   = i_cds_view
      TABLES     dfies_tab = r_ddfields
      EXCEPTIONS OTHERS    = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    DELETE r_ddfields WHERE fieldname = '.NODE1'.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_last_modified_at.
    SELECT SINGLE chgdate, chgtime FROM dd02b
     WHERE strucobjn = @i_cds_view
      INTO @DATA(dd_date_time).                         "#EC CI_NOORDER
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e007(zisu_salv) WITH i_cds_view.
    ENDIF.

    CONVERT DATE dd_date_time-chgdate TIME dd_date_time-chgtime
            INTO TIME STAMP r_modified_at TIME ZONE sy-zonlo.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_mp_for_entity.
    TRY.
        r_mp = NEW zcl_cds_alv_sadl_mapping_prov( i_entity ).

      CATCH cx_bsa_compile_time INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_parameters_for_cds_view.
    TRY.
        DATA dd_object TYPE REF TO if_dd_sobject.

        dd_object = NEW cl_dd_sobject( ).
        dd_object->read( EXPORTING sobjnames  = VALUE #( ( i_cds_view ) )
                         IMPORTING dd10bv_tab = r_dd10bv_tab ).

      CATCH cx_dd_sobject_get INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_sadl_runtime.
    TRY.
        DATA(sadl_entity) = cl_sadl_entity_factory=>get_instance( )->get_entity(
                                iv_type = cl_sadl_entity_factory=>co_type-cds
                                iv_id   = CONV sadl_entity_id( i_cds_view ) ).

        DATA(mp) = get_mp_for_entity( sadl_entity ).
        DATA(api_factory) = cl_sadl_entity_api_factory=>create( CAST cl_sadl_mp_entity( mp ) ).
        r_sadl_runtime = api_factory->get_runtime( ).

      CATCH cx_sadl_contract_violation cx_sadl_static cx_uuid_error INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_source_id.
    TRY.
        " This gets the correct entity ID, when ObjectModel.transactionalProcessingDelegated is used!
        " Parameter type is changed to RETURNING in newer releases!
        CAST cl_sadl_entity_cds( i_entity )->get_consumption_view_def( IMPORTING ev_source_id = r_source_id ).

      CATCH cx_sadl_static INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_ddic_access~get_target_for_association.
    TRY.
        DATA dd_object TYPE REF TO if_dd_sobject.

        dd_object = NEW cl_dd_sobject( ).
        dd_object->read( EXPORTING sobjnames  = VALUE #( ( i_source_view ) )
                         IMPORTING dd08bv_tab = DATA(dd08bv_tab) ).

        READ TABLE dd08bv_tab INTO DATA(dd08bv)
             WITH KEY assocname_raw = i_association_name.

        r_target_view = dd08bv-strucobjn_t.

      CATCH cx_dd_sobject_get INTO DATA(previous).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
