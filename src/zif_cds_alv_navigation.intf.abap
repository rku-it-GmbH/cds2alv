"! This interface provides Intent-Based Navigation and navigation via association.
"! The respective funstion in the ALV grid are enabled by using the annoation
"! UI.lineItem.type with values #WITH_INTENT_BASED_NAVIGATION, #FOR_INTENT_BASED_NAVIGATION
"! or #WITH_NAVIGATION_PATH.
"! Annotations for the targets:
"! - Consumption.semanticObject
"! - UI.lineItem.semanticObjectAction
"! - UI.lineItem.targetElement
INTERFACE zif_cds_alv_navigation PUBLIC.
  "! Intent-Based Navigation for a single Object (UI.lineItem.type: #WITH_INTENT_BASED_NAVIGATION).
  "! @parameter i_object            | semantic Object (provided by Consumption.semanticObject)
  "! @parameter i_action            | semantic Action (provided by UI.lineItem.semanticObjectAction)
  "! @parameter i_key_field         | annotated element / field name of the hotspot
  "! @parameter i_selected_row      | selected row
  "! @raising   zcx_cds_alv_message | Errors during navigation are propagated
  METHODS navigate_to_object_single
    IMPORTING i_object        TYPE zcds_alv_semantic_object
              i_action        TYPE zcds_alv_semantic_action
              i_cds_view      TYPE ddstrucobjname OPTIONAL
              i_key_field     TYPE fieldname      OPTIONAL
              i_selected_row  TYPE any            OPTIONAL
    EXPORTING e_refresh_after TYPE abap_bool
    RAISING   zcx_cds_alv_message.

  "! Intent-Based Navigation for multiple Objects (UI.lineItem.type: #FOR_INTENT_BASED_NAVIGATION).
  "! @parameter i_object            | semantic Object (provided by Consumption.semanticObject)
  "! @parameter i_action            | semantic Action (provided by UI.lineItem.semanticObjectAction)
  "! @parameter i_key_field         | annotated element
  "! @parameter i_selected_rows     | selected rows
  "! @raising   zcx_cds_alv_message | Errors during navigation are propagated
  METHODS navigate_to_object_mass
    IMPORTING i_object        TYPE zcds_alv_semantic_object
              i_action        TYPE zcds_alv_semantic_action
              i_cds_view      TYPE ddstrucobjname OPTIONAL
              i_key_field     TYPE fieldname      OPTIONAL
              i_selected_rows TYPE STANDARD TABLE OPTIONAL
    EXPORTING e_refresh_after TYPE abap_bool
    RAISING   zcx_cds_alv_message.

  "! Navigation to another CDS View via association (UI.lineItem.type: #WITH_NAVIGATION_PATH)
  "! @parameter i_source_view       | source CDS view
  "! @parameter i_association_name  | CDS association (provided by UI.lineItem.targetElement)
  "! @parameter i_source_parameters | source CDS parameters
  "! @parameter i_target_parameters | target CDS parameters
  "! @parameter i_selected_rows     | selected rows
  "! @raising   zcx_cds_alv_message | Errors during navigation are propagated
  METHODS navigate_via_association
    IMPORTING i_source_view       TYPE ddstrucobjname
              i_association_name  TYPE ddassociationname
              i_source_parameters TYPE zcds_alv_parameters OPTIONAL
              i_target_parameters TYPE zcds_alv_parameters OPTIONAL
              i_selected_rows     TYPE ANY TABLE
    RAISING   zcx_cds_alv_message.
ENDINTERFACE.
