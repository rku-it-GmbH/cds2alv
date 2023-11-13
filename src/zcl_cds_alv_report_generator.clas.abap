CLASS zcl_cds_alv_report_generator DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_report_generator.

    DATA factory     TYPE REF TO zif_cds_alv_factory.
    DATA ddic_access TYPE REF TO zif_cds_alv_ddic_access.
    DATA persistence TYPE REF TO zif_cds_alv_persistence.
    DATA strategy    TYPE REF TO zif_cds_alv_report_strategy.

    METHODS constructor
      IMPORTING i_factory     TYPE REF TO zif_cds_alv_factory
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cds_alv_report_generator IMPLEMENTATION.
  METHOD constructor.
    factory = i_factory.
    ddic_access = i_ddic_access.
    persistence = i_persistence.
  ENDMETHOD.

  METHOD zif_cds_alv_report_generator~generate_report.
    strategy = factory->get_generation_strategy( i_cds_view ).

    DATA(program)  = strategy->write_source( ).
    DATA(textpool) = strategy->write_textpool( ).

    CALL FUNCTION 'RS_PROGRAM_CHECK_NAME'
      EXPORTING  progname = program-progname
      EXCEPTIONS OTHERS   = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING  program            = program-progname
                 with_cua           = abap_false
                 with_documentation = abap_false
                 with_dynpro        = abap_true
                 with_includes      = abap_false
                 with_textpool      = abap_false
                 with_variants      = abap_false
                 suppress_checks    = abap_true
                 suppress_popup     = abap_true
      EXCEPTIONS object_not_found   = 0
                 OTHERS             = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING  inctoo = abap_false
      TABLES     ntext  = program-source_lines
                 otext  = program-source_lines
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA message TYPE string.
    DATA line    TYPE i.
    DATA word    TYPE string.

    SYNTAX-CHECK FOR program-source_lines MESSAGE message LINE line WORD word PROGRAM 'ZCDS_ALV_DEMO'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e006(zcds_alv) WITH i_cds_view line word.
    ENDIF.

    INSERT REPORT program-progname FROM program-source_lines.
    UPDATE progdir SET occurs = '1' WHERE name = @program-progname AND state = 'A'.
    INSERT TEXTPOOL program-progname FROM textpool LANGUAGE sy-langu.
    GET TIME STAMP FIELD program-generated_at.
    program-author = cl_abap_syst=>get_user_name( ).

    persistence->save_report_for_cds_view( i_cds_view     = i_cds_view
                                           i_program_info = program ).

    COMMIT WORK AND WAIT.
  ENDMETHOD.
ENDCLASS.
