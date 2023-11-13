"! This class comprises several DDIC calls used by the CDS ALV framework
"! to access information about the CDS Views and related DDIC objects.
"! For simplicity, an instance of this interface is provided to all instances of the other classes.
INTERFACE zif_cds_alv_ddic_access PUBLIC.
  "! Checks whether a given CDS View exists
  "! @parameter i_cds_view | CDS View
  "! @parameter r_exists   | true, if the CDS View exists
  METHODS exists_view
    IMPORTING i_cds_view      TYPE ddstrucobjname
    RETURNING VALUE(r_exists) TYPE abap_bool.

  "! Retrieves the target of an associations between CDS Views
  "! @parameter i_source_view       | the source CDS View
  "! @parameter i_association_name  | the association
  "! @parameter r_target_view       | the target CDS View
  "! @raising   zcx_cds_alv_message | Invalid combination of source view and association
  METHODS get_target_for_association
    IMPORTING i_source_view        TYPE ddstrucobjname
              i_association_name   TYPE ddassociationname
    RETURNING VALUE(r_target_view) TYPE ddstrucobjname
    RAISING   zcx_cds_alv_message.

  "! Retrieves the join conditions used to define an association between CDS Views
  "! @parameter i_source_view       | the source CDS View
  "! @parameter i_association_name  | the association
  "! @parameter i_only_constants    | only constants conditions (literals, parameters and system variables),
  "! conditions on source or target elements are removed
  "! @parameter e_target_view       | the target CDS View
  "! @parameter e_dd08bv_tab        | header data of the association (target, multiplicity, etc.)
  "! @parameter e_dd05bv_tab        | table of join conditions (each line contains a single condition and an AND/OR-indicator)
  "! @raising   zcx_cds_alv_message | Invalid combination of source view and association
  METHODS get_conditions_for_association
    IMPORTING i_source_view      TYPE ddstrucobjname
              i_association_name TYPE ddassociationname
              i_only_constants   TYPE abap_bool DEFAULT abap_false
    EXPORTING e_target_view      TYPE ddstrucobjname
              e_dd08bv_tab       TYPE dd08bvtab
              e_dd05bv_tab       TYPE dd05bvtab
    RAISING   zcx_cds_alv_message.

  "! Retrieves all annotations of a given CDS View
  "! @parameter i_cds_view              | the CDS View
  "! @parameter e_entity_annotations    | all entity resp. view annotations
  "! @parameter e_element_annotations   | all element annotations
  "! @parameter e_parameter_annotations | all parameter annotations
  METHODS get_annotations_for_cds_view
    IMPORTING i_cds_view              TYPE ddstrucobjname
    EXPORTING e_entity_annotations    TYPE cl_dd_ddl_annotation_service=>ty_t_anno_value
              e_element_annotations   TYPE cl_dd_ddl_annotation_service=>ty_t_elmnt_anno_val_src_dtel
              e_parameter_annotations TYPE cl_dd_ddl_annotation_service=>ty_t_para_anno_val_src_dtel.

  "! Retrieves the DDIC field information for a given CDS View
  "! @parameter i_cds_view          | the CDS View
  "! @parameter r_ddfields          | table of DDIC field information
  "! @raising   zcx_cds_alv_message | Occurs when the CDS View does not exist
  METHODS get_ddic_fields_for_cds_view
    IMPORTING i_cds_view        TYPE ddstrucobjname
    RETURNING VALUE(r_ddfields) TYPE ddfields
    RAISING   zcx_cds_alv_message.

  "! Retrieves all parameters of a given CDS View
  "! @parameter i_cds_view          | the CDS View
  "! @parameter r_dd10bv_tab        | table of CDS View parameters
  "! @raising   zcx_cds_alv_message | Occurs when the CDS View does not exist
  METHODS get_parameters_for_cds_view
    IMPORTING i_cds_view          TYPE ddstrucobjname
    RETURNING VALUE(r_dd10bv_tab) TYPE dd10bvtab
    RAISING   zcx_cds_alv_message.

  "! Provides an instance of the SADL runtime to perform queries on the CDS view.
  "! It is used for database selection and value help.
  "! @parameter i_cds_view          | the CDS View
  "! @parameter r_sadl_runtime      | SADL runtime instance
  "! @raising   zcx_cds_alv_message | Occurs when the CDS View does not exist
  METHODS get_sadl_runtime
    IMPORTING i_cds_view            TYPE ddstrucobjname
    RETURNING VALUE(r_sadl_runtime) TYPE REF TO if_sadl_entity_runtime
    RAISING   zcx_cds_alv_message.

  "! Provides the SADL source entity ID, if transactional processing is delegated.
  "! It is used for transactional processing.
  "! @parameter i_entity            | SADL entity
  "! @parameter r_source_id         | SADL source entity ID
  "! @raising   zcx_cds_alv_message | Occurs when the source entity ID cannot be obtained from the entity
  METHODS get_source_id
    IMPORTING i_entity           TYPE REF TO if_sadl_entity
    RETURNING VALUE(r_source_id) TYPE sadl_entity_id
    RAISING   zcx_cds_alv_message.

  "! Provides the Mapping Provider for the SADL framework.
  "! This object is the key to gain access to the SADL framework.
  "! It is used for queries, as well as for transactional processing.
  "! @parameter i_entity            | SADL entity
  "! @parameter r_mp                | SADL mapping provider
  "! @raising   zcx_cds_alv_message | Occurs when the mapping provider cannot be obtained from the entity
  METHODS get_mp_for_entity
    IMPORTING i_entity    TYPE REF TO if_sadl_entity
    RETURNING VALUE(r_mp) TYPE REF TO cl_bsa_sadl_mp
    RAISING   zcx_cds_alv_message.

  "! Retrieves the time stamp of the last change of the CDS View
  "! @parameter i_cds_view          | the source CDS View
  "! @parameter r_modified_at       | time stamp of the last change of the CDS View
  "! @raising   zcx_cds_alv_message | Occurs when the CDS View does not exist
  METHODS get_last_modified_at
    IMPORTING i_cds_view           TYPE ddstrucobjname
    RETURNING VALUE(r_modified_at) TYPE timestamp
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
