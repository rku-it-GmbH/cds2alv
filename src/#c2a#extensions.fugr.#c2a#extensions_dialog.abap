FUNCTION /C2A/EXTENSIONS_DIALOG.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_CDS_VIEW) TYPE  /C2A/CDS_VIEW_NAME
*"  RAISING
*"      /C2A/CX_ERROR_MESSAGE
*"----------------------------------------------------------------------

  cds_view = i_cds_view.
  extensions = persistence->get_report_extensions(
      i_cds_view    = i_cds_view
      i_only_active = abap_false ).

  IF extensions IS INITIAL.
    MESSAGE i020(/c2a/cds_alv).
  ELSE.
    CALL SCREEN '0100' STARTING AT 10 10.
  ENDIF.

ENDFUNCTION.
