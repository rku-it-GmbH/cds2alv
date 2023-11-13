*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_EXTENSIONSF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_GRID
*&---------------------------------------------------------------------*
FORM build_grid RAISING zcx_cds_alv_message.
  IF container IS NOT BOUND.
    CREATE OBJECT container
      EXPORTING
        container_name = 'EXTENSIONS_CONTAINER'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  IF alv_grid IS NOT BOUND.
    CREATE OBJECT alv_grid
      EXPORTING
        i_parent = container
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM prepare_grid.

    alv_grid->set_table_for_first_display(
      EXPORTING
        i_structure_name     = 'ZCDS_ALV_PROGRAM_EXTENSION'
        i_save               = 'A'
        is_layout            = layout
        it_toolbar_excluding = excluding
      CHANGING
        it_outtab            = extensions
        it_fieldcatalog      = fieldcatalog
      EXCEPTIONS
        OTHERS               = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLOSE_DISPLAY
*&---------------------------------------------------------------------*
FORM close_display RAISING zcx_cds_alv_message.
  container->free( EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    RAISE EXCEPTION TYPE zcx_cds_alv_message
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  FREE: container, alv_grid, extensions.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_CHANGES
*&---------------------------------------------------------------------*
FORM save_changes RAISING zcx_cds_alv_message.
  alv_grid->check_changed_data( ).
  persistence->set_report_extensions(
      i_cds_view   = cds_view
      i_extensions = extensions ).
  COMMIT WORK AND WAIT.
ENDFORM.
