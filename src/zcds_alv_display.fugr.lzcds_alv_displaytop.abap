FUNCTION-POOL zcds_alv_display.
CONSTANTS: full_screen  TYPE sydynnr VALUE '0100',
           split_screen TYPE sydynnr VALUE '0200'.

DATA: table_container TYPE REF TO zif_cds_alv_table_container,
      controller      TYPE REF TO zif_cds_alv_split_screen_cntr,
      builder         TYPE REF TO zif_cds_alv_grid_builder,
      alv_grid        TYPE REF TO cl_gui_alv_grid,
      container       TYPE REF TO cl_gui_custom_container,
      ref_to_table    TYPE REF TO data,
      sub_repid       TYPE syrepid,
      sub_dynnr       TYPE sydynnr,
      ok_code         TYPE cua_code,
      title           TYPE sytitle.
