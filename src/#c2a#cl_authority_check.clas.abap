CLASS /c2a/cl_authority_check DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES /c2a/if_authority_check.

    METHODS constructor
      IMPORTING i_ddic_access TYPE REF TO /c2a/if_ddic_access.

  PROTECTED SECTION.
    DATA ddic_access TYPE REF TO /c2a/if_ddic_access.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF activity,
                 execute TYPE activ_auth VALUE '16',
               END OF activity.

    ALIASES check_authority_for_view FOR /c2a/if_authority_check~check_authority_for_view.
ENDCLASS.


CLASS /c2a/cl_authority_check IMPLEMENTATION.
  METHOD constructor.
    ddic_access = i_ddic_access.
  ENDMETHOD.

  METHOD /c2a/if_authority_check~check_authority_for_tcode.
    DATA tcode TYPE sytcode.
    CALL FUNCTION 'GET_PARAMETER_TCOD'
      IMPORTING
        ptcod = tcode.

    IF tcode <> '/C2A/CDS_ALV_START'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
      EXPORTING
        tcode  = '/C2A/CDS_ALV_START'
      EXCEPTIONS
        ok     = 0
        not_ok = 1
        OTHERS = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_authority_check~check_authority_for_view.
    AUTHORITY-CHECK OBJECT '/C2A/VIEW' ID 'ACTVT'     FIELD activity-execute
                                       ID '/C2A/VIEW' FIELD i_cds_view.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message MESSAGE e037(/c2a/cds_alv) WITH i_cds_view.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_authority_check~check_authority_for_assoc.
    DATA(target_view) = ddic_access->get_target_for_association( i_source_view      = i_cds_view
                                                                 i_association_name = i_association ).
    check_authority_for_view( target_view ).
  ENDMETHOD.
ENDCLASS.
