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
    MESSAGE e007(/c2a/cds_alv) WITH cds_view.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  TRY.
      CASE ok_code.
        WHEN space OR 'ENTER'.
          CALL FUNCTION 'GET_PARAMETER_TCOD'
            IMPORTING
              ptcod = tcode.

          launcher->start_report_for_view( i_cds_view        = cds_view
                                           i_in_split_screen = mode-split_screen ).

          IF tcode <> '/C2A/CDS_ALV_START'.
            SET SCREEN 0.
            LEAVE SCREEN.
          ENDIF.

        WHEN 'EXTENSIONS'.
          CALL FUNCTION '/C2A/EXTENSIONS_DIALOG'
            EXPORTING
              i_cds_view = cds_view.
      ENDCASE.

    CATCH /c2a/cx_error_message INTO message.
      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
ENDMODULE.
