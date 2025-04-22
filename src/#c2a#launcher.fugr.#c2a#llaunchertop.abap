FUNCTION-POOL /c2a/launcher.

DATA: tcode       TYPE sytcode,
      ok_code     TYPE cua_code,
      cds_view    TYPE /c2a/cds_view_name,
      auth_check  TYPE REF TO /c2a/if_authority_check,
      ddic_access TYPE REF TO /c2a/if_ddic_access,
      launcher    TYPE REF TO /c2a/if_report_launcher,
      message     TYPE REF TO /c2a/cx_error_message.

DATA: BEGIN OF mode,
        full_screen  TYPE xfeld VALUE 'X',
        split_screen TYPE xfeld,
      END OF mode.

LOAD-OF-PROGRAM.
  TRY.
      launcher    = /c2a/cl_object_factory=>get_instance( )->get_launcher( ).
      ddic_access = /c2a/cl_object_factory=>get_instance( )->get_ddic_access( ).
      auth_check  = /c2a/cl_object_factory=>get_instance( )->get_authority_checker( ).
    CATCH /c2a/cx_error_message INTO message.
      MESSAGE message TYPE 'A'.
  ENDTRY.
