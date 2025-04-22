"! This interface serves to call the RAP (or BOPF) framework to execute actions or change data.
"! It is called by ALV Event Handler.
"! The availability of the respective functions on the ALV grid is controlled by the following Annotations:
"! - ObjectModel.UpdateEnabled
"! - ObjectModel.DeleteEnabled
"! - UI.lineItem.type: #FOR_ACTION
"!
"! The interaction with the RAP (or BOPF) framework is minimal,
"! that is to say all validations, authority checks, etc. need to be done within the respective framework classes.
INTERFACE /c2a/if_action_handler PUBLIC.
  "! Executes an action
  "! This function is available in the ALV Grid, if a field is annotated by UI.lineItem.type: #FOR_ACTION
  "! @parameter i_action              | the action (provided by annotation UI.lineItem.dataAction)
  "! @parameter i_selected_rows       | the selected rows from the ALV Grid
  "! @parameter e_refresh_after       | If set to 'true' the data will be reselected from the database and the display will be refreshed
  "! @raising   /c2a/cx_error_message | Errors from the SADL or BOPF Framework are propagated
  METHODS execute_action
    IMPORTING i_action               TYPE /c2a/action_name
              i_selected_rows        TYPE STANDARD TABLE
    EXPORTING VALUE(e_refresh_after) TYPE abap_bool
    RAISING   /c2a/cx_error_message.

  "! Updates all selected entries on the database.
  "! This function is available in the ALV Grid, if the CDS View is annotated by ObjectModel.UpdateEnabled
  "! @parameter i_selected_rows       | the selected rows from the ALV Grid
  "! @raising   /c2a/cx_error_message | Errors from the SADL or BOPF Framework are propagated
  METHODS update
    IMPORTING i_selected_rows TYPE STANDARD TABLE
    RAISING   /c2a/cx_error_message.

  "! Deletes all selected entries on the database.
  "! This function is available in the ALV Grid, if the CDS View is annotated by ObjectModel.DeleteEnabled
  "! @parameter i_selected_rows       | the selected rows from the ALV Grid
  "! @raising   /c2a/cx_error_message | Errors from the SADL or BOPF Framework are propagated
  METHODS delete
    IMPORTING i_selected_rows TYPE STANDARD TABLE
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
