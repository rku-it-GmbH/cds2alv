"! This interface provides a simple Inversion of Control container
"! to resolve the dependencies between the individual classes of the framework.
INTERFACE zif_cds_alv_ioc_container PUBLIC.

  TYPES: "! Filter to allow creation of several different implementations of an interface
    BEGIN OF ty_filter,
      key   TYPE string,
      value TYPE string,
    END OF ty_filter.

  "! Filters to allow creation of several different implementations of an interface
  TYPES ty_filters TYPE SORTED TABLE OF ty_filter WITH UNIQUE KEY key.

  "! Registers a class to implement an interface.
  "! If an instance of the interface is requested, this class will be instantiated
  "! @parameter i_interface         | interface to be implemented
  "! @parameter i_class             | implementing class
  "! @parameter i_parameters        | constructor parameters
  "! @raising   zcx_cds_alv_message | Invalid class for the given interface
  METHODS register_implementing_class
    IMPORTING i_interface  TYPE seoitfname
              i_filters    TYPE ty_filters        OPTIONAL
              i_class      TYPE seoclsname
              i_parameters TYPE abap_parmbind_tab OPTIONAL
    RAISING   zcx_cds_alv_message.

  "! Registers an instance of an interface for reuse
  "! @parameter i_interface         | interface to be implemented
  "! @parameter i_instance          | instance to be reused
  "! @raising   zcx_cds_alv_message | Invalid instance for the interface
  METHODS register_instance
    IMPORTING i_interface TYPE seoitfname
              i_filters   TYPE ty_filters OPTIONAL
              i_instance  TYPE REF TO object
    RAISING   zcx_cds_alv_message.

  "! Returns an instance of the requested interface.
  "! Dependencies are resolved recursively
  "! @parameter i_interface         | requested interface
  "! @parameter i_filters |
  "! @parameter r_instance          | instance implementing the interface
  "! @raising   zcx_cds_alv_message | Object creation errors are propagated
  METHODS resolve
    IMPORTING i_interface       TYPE seoitfname OPTIONAL
              i_filters         TYPE ty_filters OPTIONAL
    RETURNING VALUE(r_instance) TYPE REF TO object
    RAISING   zcx_cds_alv_message.

ENDINTERFACE.
