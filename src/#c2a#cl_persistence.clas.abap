CLASS /c2a/cl_persistence DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES /c2a/if_persistence.

  PROTECTED SECTION.

  PRIVATE SECTION.
    ALIASES exists_report_for_cds_view FOR /c2a/if_persistence~exists_report_for_cds_view.
    ALIASES get_report_header          FOR /c2a/if_persistence~get_report_header.
ENDCLASS.


CLASS /c2a/cl_persistence IMPLEMENTATION.
  METHOD /c2a/if_persistence~exists_report_for_cds_view.
    SELECT SINGLE @abap_true FROM /c2a/report
     WHERE cds_view = @i_cds_view
      INTO @r_exists.
  ENDMETHOD.

  METHOD /c2a/if_persistence~get_extension_parameters.
    SELECT *
      FROM /c2a/ext_par AS par
             JOIN
               /c2a/ext_part AS text ON  par~extension_name = text~extension_name
                                     AND par~parameter_name = text~parameter_name
      WHERE par~extension_name = @i_extension_name
        AND text~language      = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @r_parameters.
  ENDMETHOD.

  METHOD /c2a/if_persistence~get_intent_based_navigation.
    SELECT * FROM /c2a/navigation INTO CORRESPONDING FIELDS OF TABLE @r_navigation_table.
  ENDMETHOD.

  METHOD /c2a/if_persistence~get_navigation_exits.
    SELECT * FROM /c2a/nav_exit INTO CORRESPONDING FIELDS OF TABLE @r_navigation_exits.
  ENDMETHOD.

  METHOD /c2a/if_persistence~get_next_program_number.
    DATA(object) = '/C2A/VIEW'.
    DATA(nr_range_nr) = '00'.

    " Create Number Range Interval, if it does not yet exist
    DATA(interval) = VALUE inriv_tt( ).
    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
      EXPORTING
        nr_range_nr1 = nr_range_nr
        object       = object
      TABLES
        interval     = interval
      EXCEPTIONS
        OTHERS       = 0.
    IF interval IS INITIAL.
      interval = VALUE #( ( procind    = 'I'
                            nrrangenr  = nr_range_nr
                            fromnumber = '0000000000000001'
                            tonumber   = '0999999999999999' ) ).

      DATA(error_occured) = abap_false.
      DATA(error_iv) = VALUE inriv_tt( ).
      CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
        EXPORTING
          object        = object
        IMPORTING
          error_occured = error_occured
        TABLES
          error_iv      = error_iv
          interval      = interval
        EXCEPTIONS
          OTHERS        = 1.
      IF sy-subrc <> 0 OR error_occured = abap_true.
        RAISE EXCEPTION TYPE /c2a/cx_error_message
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
        EXPORTING
          object          = object
        EXCEPTIONS
          no_changes_made = 0
          OTHERS          = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /c2a/cx_error_message
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = nr_range_nr
        object      = object
      IMPORTING
        number      = r_number
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_persistence~get_report_extensions.
    SELECT *
      FROM /c2a/ext_hdr AS hdr
             JOIN
               /c2a/ext_hdrt AS text ON hdr~extension_name = text~extension_name
     WHERE text~language = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @r_extensions.

    LOOP AT r_extensions ASSIGNING FIELD-SYMBOL(<extension>).
      <extension>-cds_view = i_cds_view.
      SELECT SINGLE active, activated_on
        FROM /c2a/rep_ext
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

  METHOD /c2a/if_persistence~get_report_for_cds_view.
    SELECT SINGLE * FROM /c2a/report
     WHERE cds_view = @i_cds_view
      INTO CORRESPONDING FIELDS OF @r_program_info.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE e008(/c2a/cds_alv) WITH i_cds_view.
    ENDIF.

    IF r_program_info-progname IS NOT INITIAL.
      SELECT * FROM /c2a/rep_selopts
       WHERE progname = @r_program_info-progname
        INTO CORRESPONDING FIELDS OF TABLE @r_program_info-select_options.

      SELECT * FROM /c2a/rep_params
       WHERE progname = @r_program_info-progname
        INTO CORRESPONDING FIELDS OF TABLE @r_program_info-parameters.

      READ REPORT r_program_info-progname INTO r_program_info-source_lines.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_persistence~get_report_header.
    SELECT SINGLE * FROM /c2a/report
     WHERE cds_view = @i_cds_view
      INTO CORRESPONDING FIELDS OF @r_program_header.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE e008(/c2a/cds_alv) WITH i_cds_view.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_persistence~save_report_for_cds_view.
    DATA(program) = CORRESPONDING /c2a/report( i_program_info ).
    program-cds_view = i_cds_view.

    IF exists_report_for_cds_view( i_cds_view = i_cds_view ).
      DATA(program_header) = get_report_header( i_cds_view = i_cds_view ).

      " These can be maintained via a view and need to be carried over
      program-no_generation              = program_header-no_generation.
      program-add_functions_display_mode = program_header-add_functions_display_mode.
    ENDIF.

    MODIFY /c2a/report FROM @program.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE e014(/c2a/cds_alv) WITH i_cds_view.
    ENDIF.

    DELETE FROM /c2a/rep_params WHERE progname = @i_program_info-progname.
    INSERT /c2a/rep_params FROM TABLE @i_program_info-parameters.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE e014(/c2a/cds_alv) WITH i_cds_view.
    ENDIF.

    DELETE FROM /c2a/rep_selopts WHERE progname = @i_program_info-progname.
    INSERT /c2a/rep_selopts FROM TABLE @i_program_info-select_options.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /c2a/cx_error_message
            MESSAGE e014(/c2a/cds_alv) WITH i_cds_view.
    ENDIF.
  ENDMETHOD.

  METHOD /c2a/if_persistence~set_report_extensions.
    LOOP AT i_extensions INTO DATA(extension).
      DATA(db_data) = CORRESPONDING /c2a/rep_ext( extension ).
      GET TIME STAMP FIELD db_data-activated_on.
      MODIFY /c2a/rep_ext FROM @db_data.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE /c2a/cx_error_message
              MESSAGE e014(/c2a/cds_alv) WITH i_cds_view.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
