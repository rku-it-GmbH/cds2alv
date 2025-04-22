FUNCTION-POOL /c2a/extensions.

DATA: cds_view     TYPE /c2a/cds_view_name,
      extensions   TYPE /c2a/report_extension_tab,
      persistence  TYPE REF TO /c2a/if_persistence,
      container    TYPE REF TO cl_gui_custom_container,
      alv_grid     TYPE REF TO cl_gui_alv_grid,
      layout       TYPE lvc_s_layo,
      fieldcatalog TYPE lvc_t_fcat,
      excluding    TYPE ui_functions,
      ok_code      TYPE cua_code.

LOAD-OF-PROGRAM.
  persistence = /c2a/cl_object_factory=>get_instance( )->get_persistence( ).
