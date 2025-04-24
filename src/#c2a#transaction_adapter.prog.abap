*&---------------------------------------------------------------------*
*& Report /c2a/transaction_adapter
*&---------------------------------------------------------------------*
REPORT /c2a/transaction_adapter.

PARAMETERS p_view TYPE /c2a/cds_view_name.

START-OF-SELECTION.
  SET PARAMETER ID '/C2A/CDS_VIEW' FIELD p_view.
  CALL TRANSACTION '/C2A/CDS_ALV_START' AND SKIP FIRST SCREEN.
  LEAVE PROGRAM.
