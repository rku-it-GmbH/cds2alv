FUNCTION-POOL zcds_alv_extensions.

DATA: cds_view     TYPE ddstrucobjname,
      extensions   TYPE zcds_alv_program_extensions,
      persistence  TYPE REF TO zif_cds_alv_persistence,
      container    TYPE REF TO cl_gui_custom_container,
      alv_grid     TYPE REF TO cl_gui_alv_grid,
      layout       TYPE lvc_s_layo,
      fieldcatalog TYPE lvc_t_fcat,
      excluding    TYPE ui_functions,
      ok_code      TYPE cua_code.

LOAD-OF-PROGRAM.
  persistence = zcl_cds_alv_factory=>get_instance( )->get_persistence( ).
