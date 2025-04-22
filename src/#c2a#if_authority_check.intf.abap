"! Provides some authority checks for CDS views
INTERFACE /c2a/if_authority_check PUBLIC.
  "! Checks authority for the start transaction
  "! @raising /c2a/cx_error_message | Occurs, when the user is not authorized
  METHODS check_authority_for_tcode
    RAISING /c2a/cx_error_message.

  "! Checks authority for access to a CDS view
  "! @parameter i_cds_view          | CDS View
  "! @raising /c2a/cx_error_message | Occurs, when the user is not authorized
  METHODS check_authority_for_view
    IMPORTING i_cds_view TYPE /c2a/cds_view_name
    RAISING   /c2a/cx_error_message.

  "! Checks authority for the target view during Navigation via Association
  "! @parameter i_cds_view          | Source CDS view
  "! @parameter i_association       | CDS Association
  "! @raising /c2a/cx_error_message | Occurs, when the user is not authorized
  METHODS check_authority_for_assoc
    IMPORTING i_cds_view    TYPE /c2a/cds_view_name
              i_association TYPE ddassociationname
    RAISING   /c2a/cx_error_message.
ENDINTERFACE.
