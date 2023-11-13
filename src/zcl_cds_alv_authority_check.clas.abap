CLASS zcl_cds_alv_authority_check DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_authority_check.

    METHODS constructor
      IMPORTING i_ddic_access TYPE REF TO zif_cds_alv_ddic_access.

  PROTECTED SECTION.
    DATA ddic_access TYPE REF TO zif_cds_alv_ddic_access.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CDS_ALV_AUTHORITY_CHECK IMPLEMENTATION.
  METHOD constructor.
    ddic_access = i_ddic_access.
  ENDMETHOD.

  METHOD zif_cds_alv_authority_check~check_authority_for_assoc.
    " No implementation yet; might be added at a later date...
  ENDMETHOD.

  METHOD zif_cds_alv_authority_check~check_authority_for_tcode.
    DATA tcode TYPE sytcode.

    CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD tcode.         "#EC CI_CCALL

    IF tcode <> 'ZCDS_ALV_START'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING  tcode  = 'ZCDS_ALV_START'
      EXCEPTIONS ok     = 0
                 not_ok = 1
                 OTHERS = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_authority_check~check_authority_for_view.
    " No implementation yet; might be added at a later date...
  ENDMETHOD.
ENDCLASS.
