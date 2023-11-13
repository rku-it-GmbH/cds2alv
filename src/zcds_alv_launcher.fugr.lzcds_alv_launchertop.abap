FUNCTION-POOL zcds_alv_launcher.

DATA: tcode       TYPE sytcode,
      ok_code     TYPE cua_code,
      cds_view    TYPE ddstrucobjname,
      auth_check  TYPE REF TO zif_cds_alv_authority_check,
      ddic_access TYPE REF TO zif_cds_alv_ddic_access,
      launcher    TYPE REF TO zif_cds_alv_report_launcher,
      message     TYPE REF TO zcx_cds_alv_message.

DATA: BEGIN OF mode,
        full_screen  TYPE xfeld VALUE 'X',
        split_screen TYPE xfeld,
      END OF mode.

LOAD-OF-PROGRAM.
  TRY.
      launcher = zcl_cds_alv_factory=>get_instance( )->get_launcher( ).
      ddic_access = zcl_cds_alv_factory=>get_instance( )->get_ddic_access( ).
      auth_check = zcl_cds_alv_factory=>get_instance( )->get_authority_checker( ).
    CATCH zcx_cds_alv_message INTO message.
      MESSAGE message TYPE 'A'.
  ENDTRY.
