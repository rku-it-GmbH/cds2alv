FUNCTION z_cds_alv_split_screen.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(I_BUILDER) TYPE REF TO  ZIF_CDS_ALV_GRID_BUILDER
*"     REFERENCE(I_TABLE_CONTAINER) TYPE REF TO
*"        ZIF_CDS_ALV_TABLE_CONTAINER
*"     REFERENCE(I_CONTROLLER) TYPE REF TO
*"        ZIF_CDS_ALV_SPLIT_SCREEN_CNTR
*"     REFERENCE(I_SUB_REPID) TYPE  SYREPID
*"     REFERENCE(I_SUB_DYNNR) TYPE  SYDYNNR
*"----------------------------------------------------------------------

  title = i_builder->get_gui_title( ).
  builder = i_builder.
  controller = i_controller.
  sub_repid = i_sub_repid.
  sub_dynnr = i_sub_dynnr.
  table_container = i_table_container.
  ref_to_table = table_container->get_ref_to_table( ).

  CALL SCREEN split_screen.

ENDFUNCTION.
