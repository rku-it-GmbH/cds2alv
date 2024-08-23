CLASS zcl_cds_alv_report_strategy DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_report_strategy.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    METHODS evaluate_annotations REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF selection_type,
                 interval TYPE string VALUE '#INTERVAL' ##NO_TEXT,
                 range    TYPE string VALUE '#RANGE' ##NO_TEXT,
                 single   TYPE string VALUE '#SINGLE' ##NO_TEXT,
               END OF selection_type.

    TYPES:
      BEGIN OF ty_field_properties,
        qualifier           TYPE string,
        position            TYPE i,
        fieldname           TYPE fieldname,
        sel_name            TYPE rsscr_name,
        selection_type      TYPE string,
        multiple_selections TYPE abap_bool,
        mandatory           TYPE abap_bool,
        default_value       TYPE string,
        no_display          TYPE abap_bool,
        label               TYPE string,
        value_help          TYPE abap_bool,
      END OF ty_field_properties.
    TYPES ty_field_properties_tab TYPE STANDARD TABLE OF ty_field_properties WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_param_properties,
        parname       TYPE ddparname,
        sel_name      TYPE rsscr_name,
        data_type     TYPE rollname,
        system_field  TYPE fieldname,
        default_value TYPE string,
        label         TYPE string,
        value_help    TYPE abap_bool,
      END OF ty_param_properties.
    TYPES ty_param_properties_tab TYPE STANDARD TABLE OF ty_param_properties WITH EMPTY KEY.
    TYPES:
      BEGIN OF ty_ui_annotation,
        index TYPE sytabix,
        key   TYPE string,
        value TYPE string,
      END OF ty_ui_annotation.
    TYPES ty_ui_annotations TYPE SORTED TABLE OF ty_ui_annotation WITH NON-UNIQUE KEY index.

    DATA db_view              TYPE viewname16.
    DATA progname             TYPE progname.
    DATA field_properties_tab TYPE ty_field_properties_tab.
    DATA param_properties_tab TYPE ty_param_properties_tab.

    METHODS get_extension_parameters
      RETURNING VALUE(r_extension_parameters) TYPE zcds_alv_extension_parameters.

    METHODS get_program_name
      RETURNING VALUE(r_progname) TYPE progname
      RAISING   zcx_cds_alv_message.
ENDCLASS.



CLASS ZCL_CDS_ALV_REPORT_STRATEGY IMPLEMENTATION.


  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).

    evaluate_annotations( ).
  ENDMETHOD.


  METHOD evaluate_annotations.
    TRY.
        db_view = to_upper( remove_quotes( entity_annotations[ annoname = 'ABAPCATALOG.SQLVIEWNAME' ]-value ) ).
      CATCH cx_sy_itab_line_not_found.
        CLEAR db_view.
    ENDTRY.

    progname = get_program_name( ).

    " Parameter Annotations
    LOOP AT ddic_access->get_parameters_for_cds_view( cds_view ) ASSIGNING FIELD-SYMBOL(<dd10bv>).
      DATA(param_properties) = VALUE ty_param_properties( ).
      param_properties-parname      = <dd10bv>-parametername.
      param_properties-data_type    = <dd10bv>-rollname.
      param_properties-system_field = <dd10bv>-systfield.

      LOOP AT parameter_annotations ASSIGNING FIELD-SYMBOL(<param_annotation>)
           WHERE parametername = <dd10bv>-parametername.
        CASE <param_annotation>-annoname.
          WHEN 'CONSUMPTION.DEFAULTVALUE'.
            param_properties-default_value = remove_quotes( <param_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELP'.
            param_properties-value_help = abap_true.

          WHEN 'ENDUSERTEXT.LABEL'.
            param_properties-label = remove_quotes( <param_annotation>-value ).
        ENDCASE.

        IF <param_annotation>-annoname CP 'CONSUMPTION.VALUEHELPDEFINITION$*$.*'.
          param_properties-value_help = abap_true.
        ENDIF.
      ENDLOOP.

      " Selection Texts (from CDS-View-Annotation EndUser.Label or DDIC)
      IF param_properties-label IS INITIAL.
        CALL FUNCTION 'DDIF_FIELDLABEL_GET'
          EXPORTING  tabname = param_properties-data_type
          IMPORTING  label   = param_properties-label
          EXCEPTIONS OTHERS  = 0.
      ENDIF.

      APPEND param_properties TO param_properties_tab.
    ENDLOOP.

    " Field Annotations
    LOOP AT ddfields INTO DATA(ddfield) WHERE fieldname <> '.NODE1'.
      " Erlaubte Datentypen für Selektionsbilder
      " C    Zeichenfolge  (Character)
      " N    Zeichenfolge nur mit Ziffern
      " D    Datum (Date: JJJJMMTT)
      " T    Zeitpunkt (Time: HHMMSS)
      " X    Bytefolge (heXadecimal), in Ddic-Metadaten auch für INT1/2/4
      " I    Ganze Zahl (4-Byte Integer mit Vorzeichen)
      " b    1-Byte Integer, ganze Zahl <= 254
      " s    2-Byte Integer, nur für Längenfeld vor LCHR oder LRAW
      " P    Gepackte Zahl (Packed)
      " 8    Ganze Zahl (8-Byte Integer mit Vorzeichen)
      CONSTANTS allowed_types TYPE c LENGTH 10 VALUE 'CNDTXIbsP8'.

      IF ddfield-inttype NA allowed_types.
        CONTINUE.
      ENDIF.

      DATA(field_properties) = VALUE ty_field_properties( fieldname = ddfield-fieldname ).

      LOOP AT element_annotations ASSIGNING FIELD-SYMBOL(<element_annotation>)
           WHERE elementname = ddfield-fieldname.
        " only remove the index; should be sufficient for now
        IF <element_annotation>-annoname CP 'UI.SELECTIONFIELD$*$.*'.
          SPLIT <element_annotation>-annoname AT '$' INTO
                DATA(before) DATA(index) DATA(after).
          <element_annotation>-annoname = |{ before }{ after }|.
        ENDIF.

        CASE <element_annotation>-annoname.
          WHEN 'CONSUMPTION.FILTER.SELECTIONTYPE'.
            field_properties-selection_type = <element_annotation>-value.

          WHEN 'CONSUMPTION.FILTER.MULTIPLESELECTIONS'.
            field_properties-multiple_selections = xsdbool( <element_annotation>-value = 'true' ).

          WHEN 'CONSUMPTION.FILTER.MANDATORY'.
            field_properties-mandatory = xsdbool( <element_annotation>-value = 'true' ).

          WHEN 'CONSUMPTION.FILTER.DEFAULTVALUE'.
            field_properties-default_value = remove_quotes( <element_annotation>-value ).

          WHEN 'CONSUMPTION.VALUEHELP'.
            field_properties-value_help = abap_true.

          WHEN 'CONSUMPTION.HIDDEN'.
            field_properties-no_display = xsdbool( <element_annotation>-value = 'true' ).

          WHEN 'UI.SELECTIONFIELD.POSITION'.
            field_properties-position = <element_annotation>-value.

          WHEN 'UI.SELECTIONFIELD.QUALIFIER'.
            field_properties-qualifier = <element_annotation>-value.

          WHEN 'ENDUSERTEXT.LABEL'.
            field_properties-label = remove_quotes( <element_annotation>-value ).
        ENDCASE.

        IF <element_annotation>-annoname CP 'CONSUMPTION.VALUEHELPDEFINITION$*$.*'.
          field_properties-value_help = abap_true.
        ENDIF.
      ENDLOOP.

      IF field_properties-selection_type IS INITIAL.
        field_properties-no_display = abap_true.
      ENDIF.

      IF field_properties-label IS INITIAL.
        field_properties-label = ddfield-scrtext_m.
      ENDIF.

      APPEND field_properties TO field_properties_tab.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_extension_parameters.
    r_extension_parameters = VALUE #( FOR x_extension IN persistence->get_report_extensions( cds_view )
                                      ( LINES OF persistence->get_extension_parameters( x_extension-extension_name ) ) ).
  ENDMETHOD.


  METHOD get_program_name.
    TRY.
        " Vorhandenes Programm suchen
        r_progname = persistence->get_report_for_cds_view( cds_view )-progname.

      CATCH zcx_cds_alv_message.
        " Noch kein Programm vorhanden
        IF db_view IS NOT INITIAL.
          r_progname = replace( val = |ZCDS_ALV_V_{ db_view }| sub = '/' with = '_' occ = 0 ).
        ELSE.
          DATA(number) = persistence->get_next_program_number( cds_view ).
          r_progname = |ZCDS_ALV_N_{ sy-mandt }{ number }|.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cds_alv_report_strategy~write_source.
    DEFINE append_initial_line.
      APPEND | | TO r_program-source_lines.
    END-OF-DEFINITION.

    DATA source_line   LIKE LINE OF r_program-source_lines.
    DATA template_name TYPE fieldname.

    " Program Header Info
    r_program-progname = progname.
    r_program-dynpro   = '1001'.
    template_name = |_{ cds_view }|.

    " TOP Section
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |REPORT { progname }.| )
                                                 ( |TABLES: sscrfields.| )
                                                 ( |DATA: { template_name } TYPE { cds_view }.|  ) )
           TO r_program-source_lines.
    append_initial_line.

    APPEND LINES OF VALUE zcds_alv_source_lines( ( |CONSTANTS cds_view TYPE ddstrucobjname VALUE '{ cds_view }'.| )
                                                 ( |CONSTANTS title TYPE sytitle VALUE '{ description }'.| ) ) TO r_program-source_lines.
    append_initial_line.

    APPEND LINES OF VALUE zcds_alv_source_lines( ( |DATA factory TYPE REF TO zif_cds_alv_factory.             | )
                                                 ( |DATA controller TYPE REF TO zif_cds_alv_report_controller.| )
                                                 ( |DATA message TYPE REF TO zcx_cds_alv_message.             | ) ) TO r_program-source_lines.
    append_initial_line.

    APPEND LINES OF VALUE zcds_alv_source_lines(
                              ( |SELECTION-SCREEN BEGIN OF SCREEN { r_program-dynpro } AS SUBSCREEN.| )
                              ( |SELECTION-SCREEN BEGIN OF BLOCK sub.                               | ) ) TO r_program-source_lines.

    " View Parameters
    DATA parameter_counter TYPE n LENGTH 5.
    LOOP AT param_properties_tab ASSIGNING FIELD-SYMBOL(<param_properties>).
      parameter_counter = parameter_counter + 1.
      <param_properties>-sel_name = CONV rsscr_name( |P__{ parameter_counter }| ).

      source_line = |PARAMETERS { <param_properties>-sel_name } TYPE { <param_properties>-data_type } OBLIGATORY|.

      IF <param_properties>-default_value IS NOT INITIAL.
        source_line = |{ source_line } DEFAULT '{ <param_properties>-default_value }'|.
      ELSEIF <param_properties>-system_field IS NOT INITIAL.
        source_line = |{ source_line } DEFAULT sy-{ <param_properties>-system_field }|.
      ENDIF.

      source_line = |{ source_line }.|.
      APPEND source_line TO r_program-source_lines.

      INSERT VALUE #( progname = progname
                      sel_name = <param_properties>-sel_name
                      cds_view = cds_view
                      parname  = <param_properties>-parname )
             INTO TABLE r_program-parameters.
    ENDLOOP.

    " Selection Criteria (from CDS-View-Annotation Consumption.Filter)
    DATA(skip) = abap_false.
    DATA selection_counter TYPE n LENGTH 5.
    SORT field_properties_tab STABLE BY qualifier DESCENDING
                                        position ASCENDING.
    LOOP AT field_properties_tab ASSIGNING FIELD-SYMBOL(<field_properties>).
      selection_counter = selection_counter + 1.
      <field_properties>-sel_name = CONV rsscr_name( |SO_{ selection_counter }| ).

      source_line = |SELECT-OPTIONS { <field_properties>-sel_name } FOR { template_name }-{ <field_properties>-fieldname }|.

      IF <field_properties>-selection_type = selection_type-single.
        source_line = |{ source_line } NO INTERVALS|.
      ENDIF.

      IF <field_properties>-mandatory = abap_true.
        source_line = |{ source_line } OBLIGATORY|.
      ENDIF.

      IF <field_properties>-default_value IS NOT INITIAL.
        source_line = |{ source_line } DEFAULT '{ <field_properties>-default_value }'|.
      ENDIF.

      IF <field_properties>-no_display = abap_true.
        source_line = |{ source_line } NO-DISPLAY|.
      ELSE.
        skip = abap_true.
      ENDIF.

      source_line = |{ source_line }.|.
      APPEND source_line TO r_program-source_lines.

      AT END OF qualifier.
        IF skip = abap_true.
          source_line = |SELECTION-SCREEN SKIP.|.
          APPEND source_line TO r_program-source_lines.
          skip = abap_false.
        ENDIF.
      ENDAT.

      INSERT VALUE #( progname  = progname
                      sel_name  = <field_properties>-sel_name
                      cds_view  = cds_view
                      fieldname = <field_properties>-fieldname )
             INTO TABLE r_program-select_options.
    ENDLOOP.

    " Default parameters
    APPEND LINES OF VALUE zcds_alv_source_lines(
        ( |PARAMETERS p_maxrec TYPE ddshmaxrec DEFAULT 500 MODIF ID max.| )
        ( |PARAMETERS p_no_max TYPE zcds_alv_no_max USER-COMMAND no_max.| )
        ( |SELECTION-SCREEN SKIP.                                       | )
        ( |PARAMETERS p_selext TYPE zcds_alv_report_extension_name AS LISTBOX visible length 40 USER-COMMAND switch_sel MODIF ID sel.| )
        ( |PARAMETERS p_disext TYPE zcds_alv_report_extension_name AS LISTBOX visible length 40 USER-COMMAND switch_dis MODIF ID dis.| )
        ( |SELECTION-SCREEN SKIP.                                       | )
        ( |PARAMETERS p_vari   TYPE slis_vari.                          | )
        ( |PARAMETERS p_split  TYPE xfeld DEFAULT abap_false.           | )
        ( |PARAMETERS p_forall TYPE xfeld NO-DISPLAY.                   | )
        ( |PARAMETERS p_mem_id TYPE zcds_alv_memory_id NO-DISPLAY.      | )
        ( |SELECTION-SCREEN SKIP.                                       | ) ) TO r_program-source_lines.

    " Extension parameters
    LOOP AT get_extension_parameters( ) INTO DATA(ext_parameter).
      APPEND |PARAMETERS { ext_parameter-parameter_name } TYPE { ext_parameter-db_field }.| TO r_program-source_lines.
    ENDLOOP.

    " End of Subscreen
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |SELECTION-SCREEN END OF BLOCK sub.                  | )
                                                 ( |SELECTION-SCREEN END OF SCREEN { r_program-dynpro }.| ) ) TO r_program-source_lines.
    append_initial_line.

    " Include in main selection screen
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |SELECTION-SCREEN BEGIN OF BLOCK main WITH FRAME TITLE text-sub.| )
                                                 ( |SELECTION-SCREEN INCLUDE BLOCKS sub.                           | )
                                                 ( |SELECTION-SCREEN END OF BLOCK main.                            | ) ) TO r_program-source_lines.
    append_initial_line.

    " LOAD-OF-PROGRAM
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |LOAD-OF-PROGRAM.                                               | )
                                                 ( |  TRY.                                                         | )
                                                 ( |      factory = zcl_cds_alv_factory=>get_instance( ).          | )
                                                 ( |      controller = factory->get_report_controller( cds_view ). | )
                                                 ( |    CATCH zcx_cds_alv_message INTO message.                    | )
                                                 ( |      MESSAGE message TYPE 'A'.                                | )
                                                 ( |  ENDTRY.                                                      | ) ) TO r_program-source_lines.
    append_initial_line.

    " INITIALIZATION
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |INITIALIZATION.                                     | )
                                                 ( |  sy-title = title.                                 | )
                                                 ( |                                                    | )
                                                 ( |  GET PARAMETER ID 'ZCDS_ALV_NO_MAX' FIELD p_no_max.| )
                                                 ( |                                                    | )
                                                 ( |  TRY.                                              | )
                                                 ( |      controller->initialization( ).                | )
                                                 ( |    CATCH zcx_cds_alv_message INTO message.         | )
                                                 ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.    | )
                                                 ( |  ENDTRY.                                           | ) ) TO r_program-source_lines.
    append_initial_line.

    " AT SELECTION-SCREEN OUTPUT
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |AT SELECTION-SCREEN OUTPUT.                      | )
                                                 ( |  TRY.                                           | )
                                                 ( |      controller->at_selection_screen_output( ). | )
                                                 ( |    CATCH zcx_cds_alv_message INTO message.      | )
                                                 ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'. | )
                                                 ( |  ENDTRY.                                        | ) ) TO r_program-source_lines.
    append_initial_line.

    " AT-SELECTION-SCREEN
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |AT SELECTION-SCREEN.                                       | )
                                                 ( |  TRY.                                                     | )
                                                 ( |      controller->at_selection_screen( sscrfields-ucomm ). | )
                                                 ( |    CATCH zcx_cds_alv_message INTO message.                | )
                                                 ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.           | )
                                                 ( |  ENDTRY.                                                  | ) ) TO r_program-source_lines.
    append_initial_line.

    " Value help for the initial layout
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.     | )
                                                 ( |  p_vari = cl_salv_layout_service=>f4_layouts(       | )
                                                 ( |               s_key  = VALUE #( report = sy-repid ) | )
                                                 ( |               layout = p_vari )-layout.             | ) ) TO r_program-source_lines.

    " Value help (from CDS-View-Annotation Consumption.ValueHelp)
    LOOP AT param_properties_tab INTO DATA(param_properties)
         WHERE value_help = abap_true.

      APPEND LINES OF VALUE zcds_alv_source_lines(
                                ( |AT SELECTION-SCREEN ON VALUE-REQUEST FOR { param_properties-sel_name }. | )
                                ( |  TRY.                                                                  | )
                                ( |      controller->at_value_request(                                     | )
                                ( |        EXPORTING                                                       | )
                                ( |          i_sel_name = '{ param_properties-sel_name }'                  | )
                                ( |        CHANGING                                                        | )
                                ( |          c_value    = { param_properties-sel_name } ).                 | )
                                ( |    CATCH zcx_cds_alv_message INTO message.                             | )
                                ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.                        | )
                                ( |  ENDTRY.                                                               | ) ) TO r_program-source_lines.
      append_initial_line.
    ENDLOOP.

    LOOP AT field_properties_tab INTO DATA(field_properties)
         WHERE value_help = abap_true AND no_display = abap_false.

      APPEND LINES OF VALUE zcds_alv_source_lines(
                                ( |AT SELECTION-SCREEN ON VALUE-REQUEST FOR { field_properties-sel_name }-low. | )
                                ( |  TRY.                                                                      | )
                                ( |      controller->at_value_request(                                         | )
                                ( |        EXPORTING                                                           | )
                                ( |          i_sel_name = '{ field_properties-sel_name }'                      | )
                                ( |        CHANGING                                                            | )
                                ( |          c_value    = { field_properties-sel_name }-low ).                 | )
                                ( |    CATCH zcx_cds_alv_message INTO message.                                 | )
                                ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.                            | )
                                ( |  ENDTRY.                                                                   | )
                                ( |                                                                            | )
                                ( |AT SELECTION-SCREEN ON VALUE-REQUEST FOR { field_properties-sel_name }-high.| )
                                ( |  TRY.                                                                      | )
                                ( |      controller->at_value_request(                                         | )
                                ( |        EXPORTING                                                           | )
                                ( |          i_sel_name = '{ field_properties-sel_name }'                      | )
                                ( |        CHANGING                                                            | )
                                ( |          c_value    = { field_properties-sel_name }-high ).                | )
                                ( |    CATCH zcx_cds_alv_message INTO message.                                 | )
                                ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.                            | )
                                ( |  ENDTRY.                                                                   | ) ) TO r_program-source_lines.
      append_initial_line.
    ENDLOOP.

    " Help and value help for extension parameters
    LOOP AT get_extension_parameters( ) INTO ext_parameter WHERE has_value_help = abap_true.
      APPEND LINES OF VALUE zcds_alv_source_lines(
                                ( |AT SELECTION-SCREEN ON VALUE-REQUEST FOR { ext_parameter-parameter_name }.| )
                                ( |  TRY.                                                                    | )
                                ( |      controller->at_value_request(                                       | )
                                ( |        EXPORTING                                                         | )
                                ( |          i_sel_name = '{ ext_parameter-parameter_name }'                 | )
                                ( |        CHANGING                                                          | )
                                ( |          c_value    = { ext_parameter-parameter_name } ).                | )
                                ( |    CATCH zcx_cds_alv_message INTO message.                               | )
                                ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.                          | )
                                ( |  ENDTRY.                                                                 | ) ) TO r_program-source_lines.
      append_initial_line.
    ENDLOOP.

    LOOP AT get_extension_parameters( ) INTO ext_parameter WHERE has_help = abap_true.
      APPEND LINES OF VALUE zcds_alv_source_lines(
                                ( |AT SELECTION-SCREEN ON HELP-REQUEST FOR { ext_parameter-parameter_name }.| )
                                ( |  TRY.                                                                   | )
                                ( |      controller->at_help_request(                                       | )
                                ( |          i_sel_name = '{ ext_parameter-parameter_name }'                | )
                                ( |    CATCH zcx_cds_alv_message INTO message.                              | )
                                ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.                         | )
                                ( |  ENDTRY.                                                                | ) ) TO r_program-source_lines.
      append_initial_line.
    ENDLOOP.

    " START-OF-SELECTION
    APPEND LINES OF VALUE zcds_alv_source_lines( ( |START-OF-SELECTION.                              | )
                                                 ( |  TRY.                                           | )
                                                 ( |      controller->start_of_selection(            | )
                                                 ( |          i_selection       = p_selext           | )
                                                 ( |          i_display         = p_disext           | )
                                                 ( |          i_forall          = p_forall           | )
                                                 ( |          i_memory_id       = p_mem_id           | )
                                                 ( |          i_in_split_screen = p_split ).         | )
                                                 ( |                                                 | )
                                                 ( |    CATCH zcx_cds_alv_message INTO message.      | )
                                                 ( |      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'. | )
                                                 ( |  ENDTRY.                                        | ) ) TO r_program-source_lines.
  ENDMETHOD.


  METHOD zif_cds_alv_report_strategy~write_textpool.
    LOOP AT field_properties_tab INTO DATA(field_properties).
      APPEND VALUE #( id    = 'S'
                      key   = field_properties-sel_name
                      entry = |        { condense( field_properties-label ) }| )
             TO r_textpool.
    ENDLOOP.

    LOOP AT param_properties_tab INTO DATA(param_properties).
      APPEND VALUE #( id    = 'S'
                      key   = param_properties-sel_name
                      entry = |        { condense( param_properties-label ) }| )
             TO r_textpool.
    ENDLOOP.

    APPEND VALUE #( id = 'I' key = 'SUB'      entry = text-i01 ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_MAXREC' entry = |        { text-s01 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_NO_MAX' entry = |        { text-s02 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_FORALL' entry = |        { text-s03 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_MEM_ID' entry = |        { text-s04 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_SPLIT'  entry = |        { text-s05 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_SELEXT' entry = |        { text-s06 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_DISEXT' entry = |        { text-s07 }| ) TO r_textpool.
    APPEND VALUE #( id = 'S' key = 'P_VARI'   entry = |        { text-s08 }| ) TO r_textpool.

    LOOP AT get_extension_parameters( ) INTO DATA(parameter).
      INSERT VALUE #( id = 'S' key = parameter-parameter_name entry = |        { parameter-parameter_text }| )
             INTO TABLE r_textpool.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
