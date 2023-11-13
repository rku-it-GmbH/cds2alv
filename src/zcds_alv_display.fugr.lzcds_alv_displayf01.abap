*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_DISPLAYF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BUILD_GRID
*&---------------------------------------------------------------------*
FORM build_grid USING container_name RAISING zcx_cds_alv_message.
  IF container IS NOT BOUND.
    CREATE OBJECT container
      EXPORTING
        container_name = container_name
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  IF alv_grid IS NOT BOUND.
    builder->create_alv_grid(
      EXPORTING
        i_container       = container
        i_table_container = table_container
      IMPORTING
        e_alv_grid        = alv_grid ).
  ENDIF.
ENDFORM.
