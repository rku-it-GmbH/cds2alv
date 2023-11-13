CLASS zcl_cds_alv_ioc_container DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_ioc_container.

    ALIASES resolve    FOR zif_cds_alv_ioc_container~resolve.
    ALIASES ty_filters FOR zif_cds_alv_ioc_container~ty_filters.

    DATA cds_view TYPE ddstrucobjname READ-ONLY.

    METHODS constructor
      IMPORTING i_cds_view TYPE ddstrucobjname                   OPTIONAL
                i_parent   TYPE REF TO zif_cds_alv_ioc_container OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_instance,
        interface TYPE seoitfname,
        filters   TYPE ty_filters,
        object    TYPE REF TO object,
      END OF ty_instance.
    TYPES ty_instances TYPE SORTED TABLE OF ty_instance WITH NON-UNIQUE KEY interface.
    TYPES:
      BEGIN OF ty_implementation,
        interface  TYPE seoitfname,
        filters    TYPE ty_filters,
        class      TYPE seoclsname,
        parameters TYPE abap_parmbind_tab,
      END OF ty_implementation.
    TYPES ty_implementations TYPE SORTED TABLE OF ty_implementation WITH NON-UNIQUE KEY interface.

    DATA parent          TYPE REF TO zif_cds_alv_ioc_container.
    DATA instances       TYPE ty_instances.
    DATA implementations TYPE ty_implementations.

    METHODS create_object
      IMPORTING i_interface     TYPE seoitfname
                i_filters       TYPE zif_cds_alv_ioc_container=>ty_filters OPTIONAL
      RETURNING VALUE(r_object) TYPE REF TO object
      RAISING   zcx_cds_alv_message.

    METHODS store_object
      IMPORTING i_interface TYPE seoitfname
                i_filters   TYPE zif_cds_alv_ioc_container=>ty_filters OPTIONAL
                i_object    TYPE REF TO object
      RAISING   zcx_cds_alv_message.

    METHODS is_interface
      IMPORTING i_parameter_type      TYPE vseoparam-type
      RETURNING VALUE(r_is_interface) TYPE abap_bool.

    METHODS get_return_type
      IMPORTING i_increase_depth_by  TYPE i DEFAULT 0
      RETURNING VALUE(r_return_type) TYPE seoitfname
      RAISING   zcx_cds_alv_message.

    METHODS check_interface
      IMPORTING i_interface TYPE seoitfname
      RAISING   zcx_cds_alv_message.

    METHODS check_class
      IMPORTING i_class TYPE seoclsname
      RAISING   zcx_cds_alv_message.

    METHODS check_implements
      IMPORTING i_class     TYPE seoclsname
                i_interface TYPE seoitfname
      RAISING   zcx_cds_alv_message.

    METHODS get_constructor_parameters
      IMPORTING i_class_name        TYPE seoclsname
      RETURNING VALUE(r_parameters) TYPE seo_parameters.
ENDCLASS.



CLASS zcl_cds_alv_ioc_container IMPLEMENTATION.
  METHOD check_class.
    TRY.
        IF cl_oo_object=>get_instance( i_class ) IS NOT INSTANCE OF cl_oo_class.
          RAISE EXCEPTION TYPE cx_class_not_existent
            EXPORTING clsname = i_class.
        ENDIF.

      CATCH cx_class_not_existent.
        RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e027(zcds_alv) WITH i_class.
    ENDTRY.
  ENDMETHOD.

  METHOD check_implements.
    DATA(if_relations) = NEW cl_oo_if_relations( clsname = i_interface ).
    IF     NOT line_exists( if_relations->implementing_classes[ clsname = i_class ] )
       AND NOT line_exists( if_relations->subclasses[ clsname = i_class ] ).
      RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e029(zcds_alv) WITH i_class i_interface.
    ENDIF.
  ENDMETHOD.


  METHOD check_interface.
    TRY.
        IF cl_oo_object=>get_instance( i_interface ) IS NOT INSTANCE OF cl_oo_interface.
          RAISE EXCEPTION TYPE cx_class_not_existent
            EXPORTING clsname = i_interface.
        ENDIF.

      CATCH cx_class_not_existent.
        RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e028(zcds_alv) WITH i_interface.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    cds_view = i_cds_view.
    parent = i_parent.
  ENDMETHOD.

  METHOD create_object.
    DATA ref_to_object TYPE REF TO data.

    TRY.
        DATA(implementation) = implementations[ interface = i_interface filters = i_filters ].
        DATA(parameters) = VALUE abap_parmbind_tab( ).

        LOOP AT get_constructor_parameters( implementation-class ) INTO DATA(parameter).
          " Check for injected parameter
          IF line_exists( implementation-parameters[ name = parameter-sconame ]  ).
            INSERT implementation-parameters[ name = parameter-sconame ]
                   INTO TABLE parameters.
            CONTINUE.
          ENDIF.

          IF is_interface( parameter-type ).
            DATA(interface) = CONV seoitfname( parameter-type ).
            CREATE DATA ref_to_object TYPE REF TO (interface).
            ASSIGN ref_to_object->* TO FIELD-SYMBOL(<object>).

            " Remind me to allow propagation of filters when needed
            <object> ?= resolve( i_interface = interface ).

            INSERT VALUE #( name = parameter-sconame value = ref_to_object )
                   INTO TABLE parameters.
          ENDIF.
        ENDLOOP.

        CREATE OBJECT r_object TYPE (implementation-class) PARAMETER-TABLE parameters.

      CATCH cx_sy_itab_line_not_found cx_class_not_existent.
        RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e016(zcds_alv) WITH i_interface.
      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e017(zcds_alv) WITH i_interface implementation-class.
    ENDTRY.
  ENDMETHOD.

  METHOD get_constructor_parameters.
    TRY.
        DATA(class) = CAST cl_oo_class( cl_oo_class=>get_instance( i_class_name ) ).
        IF line_exists( class->methods[ cmpname = 'CONSTRUCTOR' ] ).
          r_parameters = VALUE #( FOR parameter IN class->method_parameters
                                  WHERE
                                  ( cmpname = 'CONSTRUCTOR' )
                                  ( parameter ) ).
        ELSEIF class->superclass IS NOT INITIAL.
          r_parameters = get_constructor_parameters( class->superclass ).
        ENDIF.
      CATCH cx_class_not_existent.
    ENDTRY.
  ENDMETHOD.

  METHOD get_return_type.
    DATA(depth) = 2 + i_increase_depth_by.
    DATA(call_stack) = cl_abap_get_call_stack=>get_call_stack( ).
    DATA(formatted_call_stack) = cl_abap_get_call_stack=>format_call_stack_with_struct( call_stack ).
    DATA(last_caller) = formatted_call_stack[ depth ].
    SPLIT last_caller-event_long AT '=>' INTO DATA(class_name) DATA(long_method_name).
    IF long_method_name CS '~'.
      SPLIT long_method_name AT '~' INTO DATA(interface_name) DATA(method_name).
    ELSE.
      method_name = long_method_name.
    ENDIF.

    TRY.
        IF interface_name IS NOT INITIAL AND method_name IS NOT INITIAL.
          DATA(interface) = cl_oo_interface=>get_instance( CONV #( interface_name ) ).
          r_return_type = interface->method_parameters[ cmpname = method_name pardecltyp = seos_pardecltyp_returning ]-type.
        ELSEIF class_name IS NOT INITIAL AND method_name IS NOT INITIAL.
          DATA(class) = cl_oo_class=>get_instance( CONV #( class_name ) ).
          r_return_type = class->method_parameters[ cmpname = method_name pardecltyp = seos_pardecltyp_returning ]-type.
        ENDIF.

        check_interface( r_return_type ).
      CATCH cx_class_not_existent cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e030(zcds_alv) WITH method_name.
    ENDTRY.
  ENDMETHOD.

  METHOD is_interface.
    TRY.
        r_is_interface = xsdbool( cl_oo_object=>get_instance( CONV #( i_parameter_type ) ) IS INSTANCE OF cl_oo_interface ).
      CATCH cx_class_not_existent.
        r_is_interface = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD store_object.
    check_interface( i_interface ).
    DATA(interface_descriptor) = CAST cl_abap_intfdescr( cl_abap_typedescr=>describe_by_name( i_interface ) ).

    IF NOT interface_descriptor->applies_to( i_object ).
      RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e015(zcds_alv) WITH i_interface.
    ENDIF.

    READ TABLE instances ASSIGNING FIELD-SYMBOL(<instance>)
         WITH KEY interface = i_interface
                  filters   = i_filters.
    IF sy-subrc <> 0.
      INSERT VALUE #( interface = i_interface filters = i_filters )
             INTO TABLE instances ASSIGNING <instance>.
    ENDIF.

    <instance>-object = i_object.
  ENDMETHOD.

  METHOD zif_cds_alv_ioc_container~register_implementing_class.
    check_interface( i_interface ).
    check_class( i_class ).
    check_implements( i_class = i_class i_interface = i_interface ).

    READ TABLE implementations ASSIGNING FIELD-SYMBOL(<implementation>)
         WITH KEY interface = i_interface
                  filters   = i_filters.
    IF sy-subrc <> 0.
      INSERT VALUE #( interface = i_interface filters = i_filters )
             INTO TABLE implementations ASSIGNING <implementation>.
    ENDIF.

    <implementation>-class      = i_class.
    <implementation>-parameters = i_parameters.
  ENDMETHOD.

  METHOD zif_cds_alv_ioc_container~register_instance.
    store_object( i_interface = i_interface i_filters = i_filters i_object = i_instance ).
  ENDMETHOD.

  METHOD zif_cds_alv_ioc_container~resolve.
    DATA(interface) = COND #( WHEN i_interface IS NOT INITIAL
                              THEN i_interface
                              ELSE get_return_type(
                                       i_increase_depth_by = 1 ) ).

    IF parent IS BOUND.
      TRY.
          r_instance = parent->resolve( i_interface = i_interface i_filters = i_filters ).
          IF r_instance IS BOUND.
            RETURN.
          ENDIF.

        CATCH zcx_cds_alv_message.
          " not resolved by parent
      ENDTRY.
    ENDIF.

    TRY.
        r_instance = instances[ interface = interface filters = i_filters ]-object.

      CATCH cx_sy_itab_line_not_found.
        r_instance = create_object( i_interface = interface i_filters = i_filters ).
        store_object( i_interface = interface i_filters = i_filters i_object = r_instance ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
