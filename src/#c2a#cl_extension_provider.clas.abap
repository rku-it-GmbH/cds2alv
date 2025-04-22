CLASS /c2a/cl_extension_provider DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES /c2a/if_extension_provider.

    METHODS constructor
      IMPORTING i_persistence TYPE REF TO /c2a/if_persistence.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA persistence TYPE REF TO /c2a/if_persistence.
ENDCLASS.


CLASS /c2a/cl_extension_provider IMPLEMENTATION.
  METHOD constructor.
    persistence = i_persistence.
  ENDMETHOD.

  METHOD /c2a/if_extension_provider~get_report_extensions.
    LOOP AT persistence->get_report_extensions( i_cds_view ) INTO DATA(extension).
      DATA(extension_parameters) = persistence->get_extension_parameters( extension-extension_name ).

      INSERT VALUE #( extension = extension-extension_name ) INTO TABLE r_report_extensions
             ASSIGNING FIELD-SYMBOL(<report_extension>).

      TRY.
          CREATE OBJECT <report_extension>-instance TYPE (extension-implementing_class)
            EXPORTING i_extension_name       = extension-extension_name
                      i_extension_parameters = extension_parameters.

        CATCH cx_sy_create_object_error.
          RAISE EXCEPTION TYPE /c2a/cx_error_message
                MESSAGE e019(/c2a/cds_alv) WITH extension-implementing_class extension-extension_name.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
