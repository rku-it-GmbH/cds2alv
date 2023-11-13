CLASS zcl_cds_alv_table_container DEFINITION PUBLIC INHERITING FROM zcl_cds_alv_base CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_table_container.

    METHODS constructor
      IMPORTING i_cds_view    TYPE ddstrucobjname
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_persistence TYPE REF TO zif_cds_alv_persistence
                i_memory      TYPE REF TO zif_cds_alv_memory
                i_factory     TYPE REF TO zif_cds_alv_factory
      RAISING   zcx_cds_alv_message.

  PROTECTED SECTION.
    CONSTANTS: BEGIN OF default_fieldname,
                 box    TYPE fieldname VALUE 'ALV_MARK',
                 style  TYPE fieldname VALUE 'ALV_STYLE',
                 color  TYPE fieldname VALUE 'ALV_COLOR',
                 count  TYPE fieldname VALUE 'ALV_COUNT',
                 index  TYPE fieldname VALUE 'ALV_INDEX',
                 system TYPE fieldname VALUE 'ALV_SYSTEM',
                 client TYPE fieldname VALUE 'ALV_CLIENT',
               END OF default_fieldname.

    CONSTANTS: BEGIN OF criticality,
                 neutral  TYPE c LENGTH 1 VALUE '0', " no LED
                 negative TYPE c LENGTH 1 VALUE '1', " red LED
                 critical TYPE c LENGTH 1 VALUE '2', " yellow LED
                 positive TYPE c LENGTH 1 VALUE '3', " green LED
               END OF criticality.

    TYPES: BEGIN OF ty_criticality_mapping,
             data_field TYPE fieldname,
             crit_field TYPE fieldname,
           END OF ty_criticality_mapping.
    TYPES ty_criticality_mapping_table TYPE STANDARD TABLE OF ty_criticality_mapping WITH EMPTY KEY.

    DATA in_edit_mode              TYPE abap_bool.
    DATA ref_to_table              TYPE REF TO data.
    DATA line_descriptor           TYPE REF TO cl_abap_structdescr.
    DATA table_descriptor          TYPE REF TO cl_abap_tabledescr.
    DATA ddic_structure_descriptor TYPE REF TO cl_abap_structdescr.
    DATA special_columns           TYPE zcds_alv_special_columns.
    DATA criticality_mapping_table TYPE ty_criticality_mapping_table.
    DATA read_only_fields          TYPE ddfieldnames.

    METHODS evaluate_annotations REDEFINITION.

    METHODS get_unique_fieldname
      IMPORTING i_components       TYPE abap_component_tab
                i_fieldname        TYPE fieldname
      RETURNING VALUE(r_fieldname) TYPE fieldname.

    METHODS adjust_table
      RAISING zcx_cds_alv_message.

    METHODS update_style_info.

  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_cds_alv_table_container IMPLEMENTATION.
  METHOD adjust_table.
    FIELD-SYMBOLS <table>       TYPE STANDARD TABLE.
    FIELD-SYMBOLS <color_table> TYPE lvc_t_scol.

    ASSIGN ref_to_table->* TO <table>.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
      DATA(index) = sy-tabix.
      ASSIGN COMPONENT special_columns-index_fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<index>).
      IF sy-subrc = 0.
        <index> = index.
      ENDIF.

      ASSIGN COMPONENT special_columns-system_fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<system>).
      IF sy-subrc = 0 AND <system> IS INITIAL.
        <system> = sy-sysid.
      ENDIF.

      ASSIGN COMPONENT special_columns-client_fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<client>).
      IF sy-subrc = 0 AND <client> IS INITIAL.
        <client> = sy-mandt.
      ENDIF.

      " Setting the cell colors for criticality
      ASSIGN COMPONENT special_columns-color_fieldname OF STRUCTURE <line> TO <color_table>.
      IF sy-subrc = 0.
        LOOP AT criticality_mapping_table INTO DATA(mapping).
          ASSIGN COMPONENT mapping-crit_field OF STRUCTURE <line> TO FIELD-SYMBOL(<criticality>).
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.

          READ TABLE <color_table> ASSIGNING FIELD-SYMBOL(<color>)
               WITH KEY fname = mapping-data_field.
          IF sy-subrc <> 0.
            INSERT VALUE #( fname = mapping-data_field ) INTO TABLE <color_table> ASSIGNING <color>.
          ENDIF.

          <color>-nokeycol  = abap_true.
          <color>-color-col = SWITCH #( <criticality>
                                        WHEN criticality-neutral  THEN cl_gui_resources=>list_col_normal
                                        WHEN criticality-negative THEN cl_gui_resources=>list_col_negative
                                        WHEN criticality-critical THEN cl_gui_resources=>list_col_total
                                        WHEN criticality-positive THEN cl_gui_resources=>list_col_positive ).
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    update_style_info( ).
  ENDMETHOD.

  METHOD constructor.
    super->constructor( i_cds_view    = i_cds_view
                        i_ddic_access = i_ddic_access
                        i_persistence = i_persistence
                        i_memory      = i_memory
                        i_factory     = i_factory ).
    evaluate_annotations( ).
  ENDMETHOD.

  METHOD evaluate_annotations.
    " Create Table: All elements of the CDS view + special fields (box, style, color, etc.)
    ddic_structure_descriptor = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( cds_view ) ).
    DATA(components) = ddic_structure_descriptor->get_components( ).

    special_columns = VALUE zcds_alv_special_columns( ).
    criticality_mapping_table = VALUE ty_criticality_mapping_table( ).

    special_columns-index_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-index ).
    INSERT VALUE #( name = special_columns-index_fieldname type = CAST #( cl_abap_elemdescr=>get_i( ) ) ) INTO TABLE components.

    special_columns-count_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-count ).
    INSERT VALUE #( name = special_columns-count_fieldname type = CAST #( cl_abap_elemdescr=>get_i( ) ) ) INTO TABLE components.

    special_columns-box_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-box ).
    INSERT VALUE #( name = special_columns-box_fieldname type = CAST #( cl_abap_typedescr=>describe_by_name( 'XFELD' ) ) ) INTO TABLE components.

    special_columns-system_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-system ).
    INSERT VALUE #( name = special_columns-system_fieldname type = CAST #( cl_abap_typedescr=>describe_by_name( 'SYST_SYSID' ) ) ) INTO TABLE components.

    special_columns-client_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-client ).
    INSERT VALUE #( name = special_columns-client_fieldname type = CAST #( cl_abap_typedescr=>describe_by_name( 'MANDT' ) ) ) INTO TABLE components.

    DATA(update_enabled) = xsdbool( line_exists( entity_annotations[ annoname = 'OBJECTMODEL.UPDATEENABLED' value = 'true' ] ) ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(delete_enabled) = xsdbool( line_exists( entity_annotations[ annoname = 'OBJECTMODEL.DELETEENABLED' value = 'true' ] ) ).

    IF update_enabled = abap_true.
      special_columns-style_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-style ).
      INSERT VALUE #( name = special_columns-style_fieldname type = CAST #( cl_abap_typedescr=>describe_by_name( 'LVC_T_STYL' ) ) ) INTO TABLE components.
    ENDIF.

    LOOP AT element_annotations INTO DATA(element_annotation).
      IF element_annotation-annoname CP 'UI.LINEITEM*.CRITICALITY'.
        IF special_columns-color_fieldname IS INITIAL.
          special_columns-color_fieldname = get_unique_fieldname( i_components = components i_fieldname = default_fieldname-color ).
          INSERT VALUE #( name = special_columns-color_fieldname type = CAST #( cl_abap_typedescr=>describe_by_name( 'LVC_T_SCOL' ) ) ) INTO TABLE components.
        ENDIF.

        INSERT INITIAL LINE INTO TABLE criticality_mapping_table ASSIGNING FIELD-SYMBOL(<criticality_mapping>).
        <criticality_mapping>-data_field = element_annotation-elementname.
        <criticality_mapping>-crit_field = remove_quotes( element_annotation-value ).
      ENDIF.

      IF element_annotation-annoname = 'OBJECTMODEL.READONLY' AND element_annotation-value = 'true'.
        INSERT CONV #( element_annotation-elementname ) INTO TABLE read_only_fields.
      ENDIF.
    ENDLOOP.

    DATA ref_to_line TYPE REF TO data.
    line_descriptor = cl_abap_structdescr=>get( components ).
    CREATE DATA ref_to_line TYPE HANDLE line_descriptor.
    ASSIGN ref_to_line->* TO FIELD-SYMBOL(<line>).

    CREATE DATA ref_to_table LIKE STANDARD TABLE OF <line> WITH EMPTY KEY.
    table_descriptor = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data_ref( ref_to_table ) ).
  ENDMETHOD.

  METHOD get_unique_fieldname.
    IF line_exists( i_components[ name = i_fieldname ] ).
      r_fieldname = get_unique_fieldname( i_components = i_components i_fieldname = |_{ i_fieldname }| ).
    ELSE.
      r_fieldname = i_fieldname.
    ENDIF.
  ENDMETHOD.

  METHOD update_style_info.
    FIELD-SYMBOLS <style_table> TYPE lvc_t_styl.
    FIELD-SYMBOLS <table>       TYPE STANDARD TABLE.

    ASSIGN ref_to_table->* TO <table>.

    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT special_columns-style_fieldname OF STRUCTURE <row> TO <style_table>.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      LOOP AT ddfields INTO DATA(ddfield).
        ASSIGN <style_table>[ fieldname = ddfield-fieldname ] TO FIELD-SYMBOL(<style>).
        IF sy-subrc <> 0.
          INSERT VALUE lvc_s_styl( fieldname = ddfield-fieldname ) INTO TABLE <style_table> ASSIGNING <style>.
        ENDIF.

        IF line_exists( read_only_fields[ table_line = ddfield-fieldname ] ).
          <style>-style = cl_gui_alv_grid=>mc_style_disabled.
        ELSE.
          <style>-style = SWITCH #( in_edit_mode
                                    WHEN abap_false THEN cl_gui_alv_grid=>mc_style_disabled
                                    WHEN abap_true  THEN cl_gui_alv_grid=>mc_style_enabled ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~get_line_descriptor.
    r_line_descriptor = line_descriptor.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~get_ref_to_table.
    r_ref_to_table = ref_to_table.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~get_special_columns.
    r_special_columns = special_columns.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~get_table.
    ASSIGN ref_to_table->* TO FIELD-SYMBOL(<table>).
    e_table = <table>.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~get_table_descriptor.
    r_table_descriptor = table_descriptor.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~is_in_edit_mode.
    r_in_edit_mode = in_edit_mode.
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~set_ref_to_table.
    IF NOT table_descriptor->applies_to_data_ref( i_ref_to_table ).
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e018(zcds_alv) WITH cds_view.
    ENDIF.

    ref_to_table = i_ref_to_table.
    adjust_table( ).
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~set_table.
    IF NOT table_descriptor->applies_to_data( i_table ).
      " Allow tables of the CDS line type without special columns
      READ TABLE i_table INDEX 1 ASSIGNING FIELD-SYMBOL(<line>).
      IF sy-subrc <> 0 OR NOT ddic_structure_descriptor->applies_to_data( <line> ).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE e018(zcds_alv) WITH cds_view.
      ENDIF.
    ENDIF.

    ASSIGN ref_to_table->* TO FIELD-SYMBOL(<table>).
    <table> = CORRESPONDING #( i_table ).
    adjust_table( ).
  ENDMETHOD.

  METHOD zif_cds_alv_table_container~toggle_change_mode.
    in_edit_mode = xsdbool( in_edit_mode = abap_false ).
    update_style_info( ).

    i_alv_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = DATA(fieldcatalog) ).

    LOOP AT fieldcatalog ASSIGNING FIELD-SYMBOL(<field>).
      IF line_exists( read_only_fields[ table_line = <field>-fieldname ] ).
        <field>-edit = abap_false.
      ELSE.
        <field>-edit = in_edit_mode.
      ENDIF.
    ENDLOOP.

    i_alv_grid->set_frontend_fieldcatalog( fieldcatalog ).
  ENDMETHOD.
ENDCLASS.
