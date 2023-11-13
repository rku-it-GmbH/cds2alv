*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_LAUNCHERI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VIEW  INPUT
*&---------------------------------------------------------------------*
MODULE check_view INPUT.
  IF NOT ddic_access->exists_view( cds_view ).
    MESSAGE e007(zcds_alv) WITH cds_view.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  TRY.
      CASE ok_code.
        WHEN space OR 'ENTER'.
          CALL 'GET_PARAM_TCOD' ID 'PTCOD' FIELD tcode. "#EC CI_CCALL

          launcher->start_report_for_view(
              i_cds_view        = cds_view
              i_in_split_screen = mode-split_screen ).

          IF tcode <> 'ZCDS_ALV_START'.
            SET SCREEN 0.
            LEAVE SCREEN.
          ENDIF.

        WHEN 'EXTENSIONS'.
          CALL FUNCTION 'Z_CDS_ALV_EXTENSIONS_DIALOG'
            EXPORTING
              i_cds_view = cds_view.
      ENDCASE.

    CATCH zcx_cds_alv_message INTO message.
      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMODULE.
