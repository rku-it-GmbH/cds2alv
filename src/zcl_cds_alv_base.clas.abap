CLASS zcl_cds_alv_base DEFINITION PUBLIC ABSTRACT CREATE PUBLIC.
  PUBLIC SECTION.
    DATA cds_view TYPE ddstrucobjname READ-ONLY.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    CONSTANTS: BEGIN OF field_type,
                 standard                     TYPE zcds_alv_field_type VALUE '#STANDARD',
                 for_action                   TYPE zcds_alv_field_type VALUE '#FOR_ACTION',
                 for_intent_based_navigation  TYPE zcds_alv_field_type VALUE '#FOR_INTENT_BASED_NAVIGATION',
                 with_intent_based_navigation TYPE zcds_alv_field_type VALUE '#WITH_INTENT_BASED_NAVIGATION',
                 with_navigation_path         TYPE zcds_alv_field_type VALUE '#WITH_NAVIGATION_PATH',
                 with_url                     TYPE zcds_alv_field_type VALUE '#WITH_URL',
               END OF field_type.

    DATA description TYPE ddtext.
    DATA ddfields TYPE ddfields.
    DATA parameter_annotations TYPE cl_dd_ddl_annotation_service=>ty_t_para_anno_val_src_dtel.
    DATA element_annotations TYPE cl_dd_ddl_annotation_service=>ty_t_elmnt_anno_val_src_dtel.
    DATA entity_annotations TYPE cl_dd_ddl_annotation_service=>ty_t_anno_value.
    DATA ddic_access TYPE REF TO zif_cds_alv_ddic_access.
    DATA persistence TYPE REF TO zif_cds_alv_persistence.
    DATA memory TYPE REF TO zif_cds_alv_memory.
    DATA factory TYPE REF TO zif_cds_alv_factory.

    METHODS evaluate_annotations ABSTRACT
      RAISING
        zcx_cds_alv_message.

    METHODS remove_quotes
      IMPORTING
        i_string        TYPE clike
      RETURNING
        VALUE(r_string) TYPE string.
		
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CDS_ALV_BASE IMPLEMENTATION.
  METHOD constructor.
    cds_view = to_upper( i_cds_view ).
    ddic_access = i_ddic_access.
    persistence = i_persistence.
    memory = i_memory.
    factory = i_factory.

    ddic_access->get_annotations_for_cds_view( EXPORTING i_cds_view              = cds_view
                                               IMPORTING e_entity_annotations    = entity_annotations
                                                         e_element_annotations   = element_annotations
                                                         e_parameter_annotations = parameter_annotations ).

    TRY.
        description = remove_quotes( entity_annotations[ annoname = 'ENDUSERTEXT.LABEL' ]-value ).
      CATCH cx_sy_itab_line_not_found.
        description = cds_view.
    ENDTRY.

    ddfields = ddic_access->get_ddic_fields_for_cds_view( cds_view ).
  ENDMETHOD.

  METHOD remove_quotes.
    r_string = replace( val = i_string sub = `'` with = `` occ = 0 ).
  ENDMETHOD.
ENDCLASS.
