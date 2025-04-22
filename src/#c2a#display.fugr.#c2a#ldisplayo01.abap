*----------------------------------------------------------------------*
***INCLUDE LZCDS_ALV_DISPLAYO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STAT0100'.
  SET TITLEBAR 'TITLE' WITH title.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUILD_GRID_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE build_grid_0100 OUTPUT.
  PERFORM build_grid USING 'FULL_SCREEN_CONTAINER'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STAT0200'.
  SET TITLEBAR 'TITLE' WITH title.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUILD_GRID_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE build_grid_0200 OUTPUT.
  PERFORM build_grid USING 'SPLIT_SCREEN_CONTAINER'.
ENDMODULE.
