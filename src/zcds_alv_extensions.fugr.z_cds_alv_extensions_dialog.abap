FUNCTION z_cds_alv_extensions_dialog.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(I_CDS_VIEW) TYPE  DDSTRUCOBJNAME
*"  RAISING
*"      ZCX_CDS_ALV_MESSAGE
*"----------------------------------------------------------------------

  cds_view = i_cds_view.
  extensions = persistence->get_report_extensions(
      i_cds_view    = i_cds_view
      i_only_active = abap_false ).

  IF extensions IS INITIAL.
    MESSAGE i020(zcds_alv).
  ELSE.
    CALL SCREEN '0100' STARTING AT 10 10.
  ENDIF.

ENDFUNCTION.
