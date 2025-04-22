*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_EXTENSIONSI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'ENTER'.
      PERFORM save_changes.
      PERFORM close_display.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      PERFORM close_display.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
