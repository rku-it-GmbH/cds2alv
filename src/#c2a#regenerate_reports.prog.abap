*&------------------------------------------------------------------------------*
*& Report /c2a/regenerate_reports
*&------------------------------------------------------------------------------*
*& Regenerates all existing reports fpr CDS views.
*& This may be sensible when a new version of the framework was installed
*& or new extensions were added.
*&------------------------------------------------------------------------------*
REPORT /c2a/regenerate_reports.

DATA cds_views TYPE TABLE OF ddstrucobjname.
DATA generator TYPE REF TO /c2a/if_report_generator.

START-OF-SELECTION.
  generator = /c2a/cl_object_factory=>get_instance( )->get_report_generator( ).
  SELECT cds_view FROM /c2a/report INTO TABLE @cds_views. "#EC CI_NOWHERE

  LOOP AT cds_views INTO DATA(cds_view).
    TRY.
        generator->generate_report( cds_view ).
      CATCH /c2a/cx_error_message INTO DATA(message).
        WRITE |{ message->get_text( ) }|.
    ENDTRY.
  ENDLOOP.
