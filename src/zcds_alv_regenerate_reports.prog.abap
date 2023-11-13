*&------------------------------------------------------------------------------*
*& Report zcds_alv_regenerate_reports
*&------------------------------------------------------------------------------*
*& Regenerates all existing reports fpr CDS views.
*& This may be sensible when a new version of the framework was installed
*& or new extensions were added.
*&------------------------------------------------------------------------------*
REPORT zcds_alv_regenerate_reports.

DATA cds_views TYPE TABLE OF ddstrucobjname.
DATA generator TYPE REF TO zif_cds_alv_report_generator.

START-OF-SELECTION.
  generator = zcl_cds_alv_factory=>get_instance( )->get_report_generator( ).
  SELECT cds_view FROM zcds_alv_program INTO TABLE @cds_views. "#EC CI_NOWHERE

  LOOP AT cds_views INTO DATA(cds_view).
    TRY.
        generator->generate_report( cds_view ).
      CATCH zcx_cds_alv_message INTO DATA(message).
        WRITE |{ message->get_text( ) }|.
    ENDTRY.
  ENDLOOP.
