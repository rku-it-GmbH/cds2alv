"! This interface serves to exchange data between two generated reports for CDS views.
"! As of now it is used to inject the FORALL table for navigation via association.
INTERFACE zif_cds_alv_memory PUBLIC.

  "! exports a FORALL table and the corresponding CDS association to ABAP memory
  "! @parameter i_memory_id         | Memory ID
  "! @parameter i_source_view       | Source CDS view
  "! @parameter i_association_name  | CDS assocoation (defines the FORALL condition)
  "! @parameter i_source_parameters | Parameters of the source CDS view
  "! @parameter i_forall_table      | FORALL table to be stored
  "! @raising   zcx_cds_alv_message | Errors during memory access are propagated
  METHODS export_forall_table
    IMPORTING i_memory_id         TYPE memory_id
              i_source_view       TYPE ddstrucobjname
              i_association_name  TYPE ddassociationname
              i_source_parameters TYPE zcds_alv_parameters
              i_forall_table      TYPE ANY TABLE
    RAISING   zcx_cds_alv_message.

  "! imports a FORALL table and the corresponding CDS association from ABAP memory
  "! @parameter i_memory_id           | Memory ID
  "! @parameter e_source_view         | Source CDS view
  "! @parameter e_association_name    | CDS assocoation (defines the FORALL condition)
  "! @parameter e_source_parameters   | Parameters of the source CDS view
  "! @parameter e_ref_to_forall_table | FORALL table to be retrieved
  "! @raising   zcx_cds_alv_message   | Errors during memory access are propagated
  METHODS import_forall_table
    IMPORTING i_memory_id           TYPE memory_id
    EXPORTING e_source_view         TYPE ddstrucobjname
              e_association_name    TYPE ddassociationname
              e_source_parameters   TYPE zcds_alv_parameters
              e_ref_to_forall_table TYPE REF TO data
    RAISING   zcx_cds_alv_message.

ENDINTERFACE.
