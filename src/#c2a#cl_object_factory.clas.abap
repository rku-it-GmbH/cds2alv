CLASS /c2a/cl_object_factory DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES /c2a/if_object_factory.

    CLASS-METHODS get_instance
      RETURNING VALUE(r_factory) TYPE REF TO /c2a/if_object_factory.

    CLASS-METHODS set_instance
      IMPORTING i_factory TYPE REF TO /c2a/if_object_factory.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_ioc_container,
        cds_view      TYPE /c2a/cds_view_name,
        ioc_container TYPE REF TO /c2a/if_ioc_container,
      END OF ty_ioc_container.
    TYPES tt_ioc_containers TYPE HASHED TABLE OF ty_ioc_container WITH UNIQUE KEY cds_view.

    DATA ioc_containers TYPE tt_ioc_containers.

    METHODS get_container_for_cds_view
      IMPORTING i_cds_view             TYPE /c2a/cds_view_name
      RETURNING VALUE(r_ioc_container) TYPE REF TO /c2a/if_ioc_container
      RAISING   /c2a/cx_error_message.

    METHODS get_fallback
      IMPORTING i_interface    TYPE seoitfname
      RETURNING VALUE(r_class) TYPE seoclsname.

    METHODS get_from_customizing
      IMPORTING i_interface    TYPE seoitfname
      RETURNING VALUE(r_class) TYPE seoclsname.

    METHODS get_implementation
      IMPORTING i_interface    TYPE seoitfname
      RETURNING VALUE(r_class) TYPE seoclsname.

    METHODS get_parent_container
      RETURNING VALUE(r_ioc_container) TYPE REF TO /c2a/if_ioc_container
      RAISING   /c2a/cx_error_message.

  PRIVATE SECTION.
    CLASS-DATA singleton TYPE REF TO /c2a/if_object_factory.

    CLASS-METHODS get_fallback_static
      IMPORTING i_interface    TYPE seoitfname
      RETURNING VALUE(r_class) TYPE seoclsname.

    CLASS-METHODS get_from_customizing_static
      IMPORTING i_interface    TYPE seoitfname
      RETURNING VALUE(r_class) TYPE seoclsname.

    CLASS-METHODS get_implementation_static
      IMPORTING i_interface    TYPE seoitfname
      RETURNING VALUE(r_class) TYPE seoclsname.
ENDCLASS.


CLASS /c2a/cl_object_factory IMPLEMENTATION.
  METHOD get_container_for_cds_view.
    TRY.
        r_ioc_container = ioc_containers[ cds_view = i_cds_view ]-ioc_container.
      CATCH cx_sy_itab_line_not_found.
        DATA(ioc_container) = NEW /c2a/cl_ioc_container( i_cds_view = i_cds_view i_parent = get_parent_container( ) ).
        DATA(parameters) = VALUE abap_parmbind_tab( ( name = 'I_CDS_VIEW' value = REF #( ioc_container->cds_view ) ) ).
        r_ioc_container = ioc_container.
        r_ioc_container->register_implementing_class( i_parameters = parameters:
          i_interface = '/C2A/IF_GRID_BUILDER'       i_class = get_implementation( '/C2A/IF_GRID_BUILDER' ) ),
          i_interface = '/C2A/IF_SELECTION'          i_class = get_implementation( '/C2A/IF_SELECTION' ) ),
          i_interface = '/C2A/IF_SELECTION_SCREEN'   i_class = get_implementation( '/C2A/IF_SELECTION_SCREEN' ) ),
          i_interface = '/C2A/IF_CONDITION_PROVIDER' i_class = get_implementation( '/C2A/IF_CONDITION_PROVIDER' ) ),
          i_interface = '/C2A/IF_VALUE_HELP'         i_class = get_implementation( '/C2A/IF_VALUE_HELP' ) ),
          i_interface = '/C2A/IF_REPORT_CONTROLLER'  i_class = get_implementation( '/C2A/IF_REPORT_CONTROLLER' ) ),
          i_interface = '/C2A/IF_REPORT_STRATEGY'    i_class = get_implementation( '/C2A/IF_REPORT_STRATEGY' ) ),
          i_interface = '/C2A/IF_ACTION_HANDLER'     i_class = get_implementation( '/C2A/IF_ACTION_HANDLER' ) ),
          i_interface = '/C2A/IF_TABLE_CONTAINER'    i_class = get_implementation( '/C2A/IF_TABLE_CONTAINER' ) ).
        INSERT VALUE #( cds_view = i_cds_view ioc_container = r_ioc_container ) INTO TABLE ioc_containers.
    ENDTRY.
  ENDMETHOD.

  METHOD get_fallback.
    r_class = SWITCH #( i_interface
                        WHEN '/C2A/IF_OBJECT_FACTORY'     THEN '/C2A/CL_OBJECT_FACTORY'
                        WHEN '/C2A/IF_DDIC_ACCESS'        THEN '/C2A/CL_DDIC_ACCESS'
                        WHEN '/C2A/IF_PERSISTENCE'        THEN '/C2A/CL_PERSISTENCE'
                        WHEN '/C2A/IF_MEMORY'             THEN '/C2A/CL_MEMORY'
                        WHEN '/C2A/IF_REPORT_GENERATOR'   THEN '/C2A/CL_REPORT_GENERATOR'
                        WHEN '/C2A/IF_REPORT_LAUNCHER'    THEN '/C2A/CL_REPORT_LAUNCHER'
                        WHEN '/C2A/IF_NAVIGATION'         THEN '/C2A/CL_NAVIGATION'
                        WHEN '/C2A/IF_GRID_BUILDER'       THEN '/C2A/CL_GRID_BUILDER'
                        WHEN '/C2A/IF_SELECTION'          THEN '/C2A/CL_SELECTION'
                        WHEN '/C2A/IF_SELECTION_SCREEN'   THEN '/C2A/CL_SELECTION_SCREEN'
                        WHEN '/C2A/IF_CONDITION_PROVIDER' THEN '/C2A/CL_SELECTION_SCREEN'
                        WHEN '/C2A/IF_VALUE_HELP'         THEN '/C2A/CL_VALUE_HELP'
                        WHEN '/C2A/IF_REPORT_CONTROLLER'  THEN '/C2A/CL_REPORT_CONTROLLER'
                        WHEN '/C2A/IF_REPORT_STRATEGY'    THEN '/C2A/CL_REPORT_STRATEGY'
                        WHEN '/C2A/IF_ACTION_HANDLER'     THEN '/C2A/CL_ACTION_HANDLER'
                        WHEN '/C2A/IF_AUTHORITY_CHECK'    THEN '/C2A/CL_AUTHORITY_CHECK'
                        WHEN '/C2A/IF_TABLE_CONTAINER'    THEN '/C2A/CL_TABLE_CONTAINER'
                        WHEN '/C2A/IF_EXTENSION_PROVIDER' THEN '/C2A/CL_EXTENSION_PROVIDER' ).
  ENDMETHOD.

  METHOD get_fallback_static.
    r_class = SWITCH #( i_interface
                        WHEN '/C2A/IF_OBJECT_FACTORY'     THEN '/C2A/CL_OBJECT_FACTORY'
                        WHEN '/C2A/IF_DDIC_ACCESS'        THEN '/C2A/CL_DDIC_ACCESS'
                        WHEN '/C2A/IF_PERSISTENCE'        THEN '/C2A/CL_PERSISTENCE'
                        WHEN '/C2A/IF_MEMORY'             THEN '/C2A/CL_MEMORY'
                        WHEN '/C2A/IF_REPORT_GENERATOR'   THEN '/C2A/CL_REPORT_GENERATOR'
                        WHEN '/C2A/IF_REPORT_LAUNCHER'    THEN '/C2A/CL_REPORT_LAUNCHER'
                        WHEN '/C2A/IF_NAVIGATION'         THEN '/C2A/CL_NAVIGATION'
                        WHEN '/C2A/IF_GRID_BUILDER'       THEN '/C2A/CL_GRID_BUILDER'
                        WHEN '/C2A/IF_SELECTION'          THEN '/C2A/CL_SELECTION'
                        WHEN '/C2A/IF_SELECTION_SCREEN'   THEN '/C2A/CL_SELECTION_SCREEN'
                        WHEN '/C2A/IF_CONDITION_PROVIDER' THEN '/C2A/CL_SELECTION_SCREEN'
                        WHEN '/C2A/IF_VALUE_HELP'         THEN '/C2A/CL_VALUE_HELP'
                        WHEN '/C2A/IF_REPORT_CONTROLLER'  THEN '/C2A/CL_REPORT_CONTROLLER'
                        WHEN '/C2A/IF_REPORT_STRATEGY'    THEN '/C2A/CL_REPORT_STRATEGY'
                        WHEN '/C2A/IF_ACTION_HANDLER'     THEN '/C2A/CL_ACTION_HANDLER'
                        WHEN '/C2A/IF_AUTHORITY_CHECK'    THEN '/C2A/CL_AUTHORITY_CHECK'
                        WHEN '/C2A/IF_TABLE_CONTAINER'    THEN '/C2A/CL_TABLE_CONTAINER'
                        WHEN '/C2A/IF_EXTENSION_PROVIDER' THEN '/C2A/CL_EXTENSION_PROVIDER' ).
  ENDMETHOD.

  METHOD get_from_customizing.
    SELECT SINGLE class FROM zcds_alv_iocclif WHERE interface = @i_interface INTO @r_class.
  ENDMETHOD.

  METHOD get_from_customizing_static.
    SELECT SINGLE class FROM zcds_alv_iocclif WHERE interface = @i_interface INTO @r_class.
  ENDMETHOD.

  METHOD get_implementation.
    r_class = get_from_customizing( i_interface ).
    IF r_class IS INITIAL.
      r_class = get_fallback( i_interface ).
    ENDIF.
  ENDMETHOD.

  METHOD get_implementation_static.
    r_class = get_from_customizing_static( i_interface ).
    IF r_class IS INITIAL.
      r_class = get_fallback_static( i_interface ).
    ENDIF.
  ENDMETHOD.

  METHOD get_instance.
    IF singleton IS NOT BOUND.
      DATA(class) = get_implementation_static( '/C2A/IF_OBJECT_FACTORY' ).
      CREATE OBJECT singleton TYPE (class).
    ENDIF.

    r_factory = singleton.
  ENDMETHOD.

  METHOD get_parent_container.
    TRY.
        r_ioc_container = ioc_containers[ cds_view = space ]-ioc_container.
      CATCH cx_sy_itab_line_not_found.
        r_ioc_container = NEW /c2a/cl_ioc_container( ).
        r_ioc_container->register_instance( i_interface = '/C2A/IF_OBJECT_FACTORY' i_instance = me ).
        r_ioc_container->register_implementing_class(:
          i_interface = '/C2A/IF_DDIC_ACCESS'        i_class = get_implementation( '/C2A/IF_DDIC_ACCESS' ) ),
          i_interface = '/C2A/IF_PERSISTENCE'        i_class = get_implementation( '/C2A/IF_PERSISTENCE' ) ),
          i_interface = '/C2A/IF_MEMORY'             i_class = get_implementation( '/C2A/IF_MEMORY' ) ),
          i_interface = '/C2A/IF_REPORT_GENERATOR'   i_class = get_implementation( '/C2A/IF_REPORT_GENERATOR'  ) ),
          i_interface = '/C2A/IF_REPORT_LAUNCHER'    i_class = get_implementation( '/C2A/IF_REPORT_LAUNCHER' ) ),
          i_interface = '/C2A/IF_NAVIGATION'         i_class = get_implementation( '/C2A/IF_NAVIGATION' ) ),
          i_interface = '/C2A/IF_AUTHORITY_CHECK'    i_class = get_implementation( '/C2A/IF_AUTHORITY_CHECK' ) ),
          i_interface = '/C2A/IF_EXTENSION_PROVIDER' i_class = get_implementation( '/C2A/IF_EXTENSION_PROVIDER' ) ).
        INSERT VALUE #( cds_view = space ioc_container = r_ioc_container ) INTO TABLE ioc_containers.
    ENDTRY.
  ENDMETHOD.

  METHOD set_instance.
    singleton = i_factory.
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_authority_checker.
    r_authority_checker ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_action_handler.
    r_action_handler ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_builder.
    r_builder ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_ddic_access.
    r_ddic_access ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_extension_provider.
    r_extension_provider ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_generation_strategy.
    r_strategy ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_launcher.
    r_launcher ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_memory.
    r_memory ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_navigation.
    r_navigation ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_persistence.
    r_persistence ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_report_controller.
    r_controller ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_report_generator.
    r_generator ?= get_parent_container( )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_selection.
    r_selection ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_selection_screen.
    r_selection_screen ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_table_container.
    r_table_container ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.

  METHOD /c2a/if_object_factory~get_value_help.
    r_value_help ?= get_container_for_cds_view( i_cds_view )->resolve( ).
  ENDMETHOD.
ENDCLASS.
