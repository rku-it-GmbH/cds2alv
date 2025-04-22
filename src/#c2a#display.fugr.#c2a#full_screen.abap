FUNCTION /C2A/FULL_SCREEN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_BUILDER) TYPE REF TO  /C2A/IF_GRID_BUILDER
*"     REFERENCE(I_TABLE_CONTAINER) TYPE REF TO
*"        /C2A/IF_TABLE_CONTAINER
*"  RAISING
*"      /C2A/CX_ERROR_MESSAGE
*"----------------------------------------------------------------------

  title = i_builder->get_gui_title( ).
  builder = i_builder.
  table_container = i_table_container.
  ref_to_table = table_container->get_ref_to_table( ).

  CALL SCREEN full_screen.

ENDFUNCTION.
