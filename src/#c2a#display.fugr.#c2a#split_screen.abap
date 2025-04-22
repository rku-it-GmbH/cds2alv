FUNCTION /C2A/SPLIT_SCREEN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_BUILDER) TYPE REF TO  /C2A/IF_GRID_BUILDER
*"     REFERENCE(I_TABLE_CONTAINER) TYPE REF TO
*"        /C2A/IF_TABLE_CONTAINER
*"     REFERENCE(I_CONTROLLER) TYPE REF TO  /C2A/IF_SPLIT_SCREEN_CONTR
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
