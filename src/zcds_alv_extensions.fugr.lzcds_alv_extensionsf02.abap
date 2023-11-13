*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_EXTENSIONSF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  PREPARE_GRID
*&---------------------------------------------------------------------*
FORM prepare_grid RAISING zcx_cds_alv_message.
  layout = VALUE #(
    cwidth_opt = abap_true
    col_opt    = abap_true
    zebra      = abap_true
    edit       = abap_true ).

  excluding = VALUE #(
    ( cl_gui_alv_grid=>mc_fc_loc_append_row )
    ( cl_gui_alv_grid=>mc_fc_loc_copy )
    ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
    ( cl_gui_alv_grid=>mc_fc_loc_cut )
    ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
    ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
    ( cl_gui_alv_grid=>mc_fc_loc_move_row )
    ( cl_gui_alv_grid=>mc_fc_loc_paste )
    ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
    ( cl_gui_alv_grid=>mc_fc_loc_undo ) ).

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZCDS_ALV_PROGRAM_EXTENSION'
    CHANGING
      ct_fieldcat      = fieldcatalog
    EXCEPTIONS
      OTHERS           = 1.
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_cds_alv_message
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT fieldcatalog ASSIGNING FIELD-SYMBOL(<field>).
    <field>-tech = xsdbool( <field>-fieldname = 'ACTIVATED_ON' ).
    <field>-edit = <field>-checkbox = xsdbool( <field>-fieldname = 'ACTIVE' ).
    <field>-key  = <field>-key_sel  = xsdbool( <field>-fieldname = 'EXTENSION_TEXT' ).

    IF <field>-fieldname <> 'EXTENSION_TEXT' AND <field>-fieldname <> 'ACTIVE'.
      <field>-no_out = abap_true.
      <field>-edit   = abap_false.
    ENDIF.
  ENDLOOP.
ENDFORM.
