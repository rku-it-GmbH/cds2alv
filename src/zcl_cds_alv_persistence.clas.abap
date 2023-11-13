CLASS zcl_cds_alv_persistence DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_persistence.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cds_alv_persistence IMPLEMENTATION.
  METHOD zif_cds_alv_persistence~exists_report_for_cds_view.
    SELECT SINGLE @abap_true FROM zcds_alv_program
     WHERE cds_view = @i_cds_view
      INTO @r_exists.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~get_extension_parameters.
    SELECT * FROM zcds_alv_extpar  AS par
             JOIN zcds_alv_extpart AS text ON par~extension_name = text~extension_name
                                          AND par~parameter_name = text~parameter_name
     WHERE par~extension_name = @i_extension_name
       AND text~spras = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @r_parameters.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~get_intent_based_navigation.
    SELECT * FROM zcds_alv_nav INTO CORRESPONDING FIELDS OF TABLE @r_navigation_table.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~get_navigation_exits.
    SELECT * FROM zcds_alv_navexit INTO CORRESPONDING FIELDS OF TABLE @r_navigation_exits.
  ENDMETHOD.


  METHOD zif_cds_alv_persistence~get_next_program_number.
    DATA(object) =  'ZCDS_ALV_N'.
    DATA(nr_range_nr) = '00'.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING  nr_range_nr = nr_range_nr
                 object      = object
      IMPORTING  number      = r_number
      EXCEPTIONS OTHERS      = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~get_report_extensions.
    SELECT * FROM zcds_alv_exthdr  AS hdr
             JOIN zcds_alv_exthdrt AS text ON hdr~extension_name = text~extension_name
     WHERE text~spras = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @r_extensions.

    LOOP AT r_extensions ASSIGNING FIELD-SYMBOL(<extension>).
      <extension>-cds_view = i_cds_view.
      SELECT SINGLE active, activated_on
        FROM zcds_alv_progext
       WHERE cds_view       = @i_cds_view
         AND extension_name = @<extension>-extension_name
        INTO CORRESPONDING FIELDS OF @<extension>.
      IF sy-subrc <> 0.
        CLEAR: <extension>-active,
               <extension>-activated_on.
      ENDIF.
    ENDLOOP.

    IF i_only_active = abap_true.
      DELETE r_extensions WHERE active = abap_false.
    ENDIF.

    IF i_only_display = abap_true.
      DELETE r_extensions WHERE alternative_display = abap_false.
    ENDIF.

    IF i_only_selection = abap_true.
      DELETE r_extensions WHERE alternative_selection = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~get_report_for_cds_view.
    SELECT SINGLE *
      FROM zcds_alv_program
     WHERE cds_view = @i_cds_view
      INTO CORRESPONDING FIELDS OF @r_program_info.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e008(zcds_alv) WITH i_cds_view.
    ENDIF.

    IF r_program_info-progname IS NOT INITIAL.
      SELECT * FROM zcds_alv_selopts
       WHERE progname = @r_program_info-progname
        INTO CORRESPONDING FIELDS OF TABLE @r_program_info-select_options.

      SELECT * FROM zcds_alv_params
       WHERE progname = @r_program_info-progname
        INTO CORRESPONDING FIELDS OF TABLE @r_program_info-parameters.

      READ REPORT r_program_info-progname INTO r_program_info-source_lines.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~save_report_for_cds_view.
    DATA(program) = CORRESPONDING zcds_alv_program( i_program_info ).
    program-cds_view = i_cds_view.

    MODIFY zcds_alv_program FROM @program.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e014(zcds_alv) WITH i_cds_view.
    ENDIF.

    DELETE FROM zcds_alv_params WHERE progname = @i_program_info-progname.
    INSERT zcds_alv_params FROM TABLE @i_program_info-parameters.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e014(zcds_alv) WITH i_cds_view.
    ENDIF.

    DELETE FROM zcds_alv_selopts WHERE progname = @i_program_info-progname.
    INSERT zcds_alv_selopts FROM TABLE @i_program_info-select_options.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e014(zcds_alv) WITH i_cds_view.
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_persistence~set_report_extensions.
    LOOP AT i_extensions INTO DATA(extension).
      DATA(db_data) = CORRESPONDING zcds_alv_progext( extension ).
      GET TIME STAMP FIELD db_data-activated_on.
      MODIFY zcds_alv_progext FROM @db_data.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE e014(zcds_alv) WITH i_cds_view.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
