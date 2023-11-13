"! This interface is used to provide parameters and selections to the selection class
"! to be used in the query for the SADL framework.
"! It will be implemented by the selection screen class to provide input from the selection screen.
"! Other possible implementations may include a free selections dialog or reading contents from a report variant.
INTERFACE zif_cds_alv_condition_provider PUBLIC.

  "! Provides the selection criteria
  "! @parameter e_where_tab    | OSQL where condition
  "! @parameter e_field_ranges | A collection of all conditions as RANGE tables
  "! @parameter e_maxrec       | Maximum number of records to be selected
  "! @parameter e_no_max       | No maximum
  METHODS get_selections
    EXPORTING e_where_tab    TYPE rsds_where_tab
              e_field_ranges TYPE rsds_frange_t
              e_maxrec       TYPE ddshmaxrec
              e_no_max       TYPE abap_bool.

  "! Provides the CDS View parameters to be used for the selection
  "! @parameter r_parameters | table of CDS View parameter values
  METHODS get_parameters
    RETURNING VALUE(r_parameters) TYPE zcds_alv_parameters.

ENDINTERFACE.
