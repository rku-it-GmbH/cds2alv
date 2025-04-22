"! This interface provides methods to create instances of report extensions.
INTERFACE /c2a/if_extension_provider PUBLIC.

  TYPES: "! Report extension with key
    BEGIN OF ty_report_extension,
      extension TYPE /c2a/report_extension_name,
      instance  TYPE REF TO /c2a/if_report_extension,
    END OF ty_report_extension.
  "! Table of report extensions with key
  TYPES ty_report_extensions TYPE HASHED TABLE OF ty_report_extension WITH UNIQUE KEY extension.

  "! Returns instances of all active report extensions for a given CDS view
  "! @parameter i_cds_view            | CDS view
  "! @parameter r_report_extensions   | Instances of all active extensions
  "! @raising   /c2a/cx_error_message | Errors during creation are propagated
  METHODS get_report_extensions
    IMPORTING i_cds_view                 TYPE /c2a/cds_view_name
    RETURNING VALUE(r_report_extensions) TYPE ty_report_extensions
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
