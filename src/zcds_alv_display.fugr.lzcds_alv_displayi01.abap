*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_DISPLAYI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE exit INPUT.
  IF container IS BOUND.
    container->free( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  FREE: container, alv_grid.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE user_command INPUT.
  TRY.
      IF controller IS BOUND.
        controller->handle_user_command( ok_code ).
        alv_grid->refresh_table_display( EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      CLEAR ok_code.
    CLEANUP.
      CLEAR ok_code.
  ENDTRY.
ENDMODULE.
