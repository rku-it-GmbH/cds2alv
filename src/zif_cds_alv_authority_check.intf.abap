"! Provides some authority checks for CDS views
INTERFACE zif_cds_alv_authority_check PUBLIC.

  "! Checks authority for the start transaction
  "! @raising zcx_cds_alv_message | Occurs, when the user is not authorized
  METHODS check_authority_for_tcode
    RAISING zcx_cds_alv_message.

  "! Checks authority for access to a CDS view
  "! @parameter i_cds_view          | CDS View
  "! @raising   zcx_cds_alv_message | Occurs, when the user is not authorized
  METHODS check_authority_for_view
    IMPORTING i_cds_view TYPE ddstrucobjname
    RAISING   zcx_cds_alv_message.

  "! Checks authority for the target view during Navigation via Association
  "! @parameter i_cds_view          | Source CDS view
  "! @parameter i_association       | CDS Association
  "! @raising   zcx_cds_alv_message | Occurs, when the user is not authorized
  METHODS check_authority_for_assoc
    IMPORTING i_cds_view    TYPE ddstrucobjname
              i_association TYPE ddassociationname
    RAISING   zcx_cds_alv_message.

ENDINTERFACE.
