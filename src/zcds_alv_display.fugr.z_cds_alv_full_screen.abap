FUNCTION z_cds_alv_full_screen.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(I_BUILDER) TYPE REF TO  ZIF_CDS_ALV_GRID_BUILDER
*"     REFERENCE(I_TABLE_CONTAINER) TYPE REF TO
*"        ZIF_CDS_ALV_TABLE_CONTAINER
*"  RAISING
*"      ZCX_CDS_ALV_MESSAGE
*"----------------------------------------------------------------------

  title = i_builder->get_gui_title( ).
  builder = i_builder.
  table_container = i_table_container.
  ref_to_table = table_container->get_ref_to_table( ).

  CALL SCREEN full_screen.

ENDFUNCTION.
