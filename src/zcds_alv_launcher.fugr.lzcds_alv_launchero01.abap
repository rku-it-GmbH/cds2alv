*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_LAUNCHERO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  AUTH_CHECK_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE auth_check_0100 OUTPUT.
  TRY.
      auth_check->check_authority_for_tcode( ).
    CATCH zcx_cds_alv_message INTO message.
      MESSAGE message TYPE 'A'.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.
