"! This interface serves to exchange data between two generated reports for CDS views.
"! As of now it is used to inject the FORALL table for navigation via association.
INTERFACE /c2a/if_memory PUBLIC.
  "! exports a FORALL table and the corresponding CDS association to ABAP memory
  "! @parameter i_memory_id           | Memory ID
  "! @parameter i_source_view         | Source CDS view
  "! @parameter i_association_name    | CDS assocoation (defines the FORALL condition)
  "! @parameter i_source_parameters   | Parameters of the source CDS view
  "! @parameter i_forall_table        | FORALL table to be stored
  "! @raising   /c2a/cx_error_message | Errors during memory access are propagated
  METHODS export_forall_table
    IMPORTING i_memory_id         TYPE memory_id
              i_source_view       TYPE /c2a/cds_view_name
              i_association_name  TYPE ddassociationname
              i_source_parameters TYPE /c2a/cds_parameter_tab
              i_forall_table      TYPE ANY TABLE
    RAISING   /c2a/cx_error_message.

  "! imports a FORALL table and the corresponding CDS association from ABAP memory
  "! @parameter i_memory_id           | Memory ID
  "! @parameter e_source_view         | Source CDS view
  "! @parameter e_association_name    | CDS assocoation (defines the FORALL condition)
  "! @parameter e_source_parameters   | Parameters of the source CDS view
  "! @parameter e_ref_to_forall_table | FORALL table to be retrieved
  "! @raising   /c2a/cx_error_message | Errors during memory access are propagated
  METHODS import_forall_table
    IMPORTING i_memory_id           TYPE memory_id
    EXPORTING e_source_view         TYPE /c2a/cds_view_name
              e_association_name    TYPE ddassociationname
              e_source_parameters   TYPE /c2a/cds_parameter_tab
              e_ref_to_forall_table TYPE REF TO data
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
