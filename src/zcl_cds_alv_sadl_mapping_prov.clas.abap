CLASS zcl_cds_alv_sadl_mapping_prov DEFINITION PUBLIC INHERITING FROM cl_sadl_mp_entity FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_entity TYPE REF TO if_sadl_entity
      RAISING   cx_bsa_compile_time.

  PROTECTED SECTION.
    METHODS get_sadl_definition REDEFINITION.

  PRIVATE SECTION.
    DATA entity TYPE REF TO if_sadl_entity.

    METHODS build_sadl_definition
      RAISING cx_bsa_compile_time.

    METHODS convert_id
      IMPORTING i_name        TYPE string
      RETURNING VALUE(r_name) TYPE string.
ENDCLASS.



CLASS ZCL_CDS_ALV_SADL_MAPPING_PROV IMPLEMENTATION.


  METHOD build_sadl_definition.
    TRY.
        " The Mapping Provider is the key to gaining access to the SADL framework.
        " This method encapsulates the definition of the mapping provider.
        " It is meant to be the single point of truth that needs to be maintained to ensure access th the SADL framework.
        " This particular implementation was initially created as copy of the method CL_SADL_ENTITY_UTIL=>GET_SADL_FOR_ENTITY.
        " That method served as a reliable way to access the SADL framework in SAP Release 7.50, but does not exist in later releases.
        DATA(entity_id) = entity->get_id( ).
        DATA(entity_type) = entity->get_type( ).
        DATA(id) = convert_id( CONV #( entity_id ) ).

*        " There is a known incompatibility between CDS View with parameters and virtual elements
*        " Therefore the property 'exposure' is only set when the CDS view has no parameters
*        entity->get_parameters( IMPORTING et_parameters = DATA(parameters) ).
*        IF lines( parameters ) = 0.
*          DATA(exposure) = 'TRUE'.
*        ENDIF.

        " SADL definition according to CL_SADL_ENTITY_UTIL=>GET_SADL_FOR_ENTITY.
        INSERT VALUE #( name = id  type = entity_type  binding = entity_id ) INTO TABLE ms_sadl_definition-data_sources.
        INSERT VALUE #( name = id  anchor = abap_true  data_source = id  max_edit_mode = 'RO'  id = id ) INTO TABLE ms_sadl_definition-structures.
        INSERT VALUE #( name = 'SADL_QUERY'  structure_id = id  id = 'QUERY' ) INTO TABLE ms_sadl_definition-mapped_queries ASSIGNING FIELD-SYMBOL(<query>).
        DATA(entity_consumption_info) = cl_sadl_entity_factory=>get_instance( )->get_entity_consumption_info( iv_id = entity_id  iv_type = entity_type ).
        entity_consumption_info->get_elements( IMPORTING et_elements = DATA(elements) ).
        LOOP AT elements ASSIGNING FIELD-SYMBOL(<element>).
          DATA(tabix) = sy-tabix.
          INSERT VALUE #( name = <element>-name  binding = <element>-name  structure_id = id  id = 'X' && tabix ) INTO TABLE ms_sadl_definition-attributes.
          IF <element>-text_search-allowed = abap_true.
            READ TABLE <query>-input_parameters WITH KEY type = 'SEARCH_TEXT' ASSIGNING FIELD-SYMBOL(<search>).
            IF sy-subrc <> 0.
              APPEND VALUE #( id = 'SEARCH'  name = 'SEARCH_TEXT'  type = 'SEARCH_TEXT' ) TO <query>-input_parameters ASSIGNING <search>.
            ENDIF.
            APPEND VALUE #( ip_id = 'SEARCH'  binding = <element>-name ) TO <search>-attributes.
          ENDIF.
        ENDLOOP.

      CATCH cx_sadl_static INTO DATA(previous).
        RAISE EXCEPTION TYPE cx_bsa_compile_time
          EXPORTING
            textid   = cx_bsa_compile_time=>cx_bsa_sadl_xml
            previous = previous.
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    GET TIME STAMP FIELD DATA(timestamp).
    DATA(uuid) = CONV if_sadl_types=>ty_uuid( |ZCDS_ALV:{ i_entity->get_id( ) }| ).
    super->constructor( iv_uuid = uuid iv_timestamp = timestamp ).
    entity = i_entity.
  ENDMETHOD.


  METHOD convert_id.
    r_name = i_name.
    REPLACE '~' IN r_name WITH '_'.
    REPLACE ALL OCCURRENCES OF '/' IN r_name WITH 'x'.
  ENDMETHOD.


  METHOD get_sadl_definition.
    IF ms_sadl_definition IS INITIAL.
      build_sadl_definition( ).
    ENDIF.

    rs_sadl_definition = ms_sadl_definition.
  ENDMETHOD.
ENDCLASS.
