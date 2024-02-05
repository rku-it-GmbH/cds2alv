CLASS zcl_cds_alv_navigation DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_navigation.

    METHODS constructor
      IMPORTING i_persistence TYPE REF TO zif_cds_alv_persistence
                i_ddic_access TYPE REF TO zif_cds_alv_ddic_access
                i_launcher    TYPE REF TO zif_cds_alv_report_launcher.

  PROTECTED SECTION.

private section.

  constants:
    BEGIN OF mass_processing,
                 none  TYPE zcds_alv_nav_mass_processing VALUE space,
                 loop  TYPE zcds_alv_nav_mass_processing VALUE 'L',
                 table TYPE zcds_alv_nav_mass_processing VALUE 'T',
               END OF mass_processing .
  constants EXIT_INTERFACE type SEOITFNAME value 'ZIF_CDS_ALV_NAVIGATION' ##NO_TEXT.
  data PERSISTENCE type ref to ZIF_CDS_ALV_PERSISTENCE .
  data DDIC_ACCESS type ref to ZIF_CDS_ALV_DDIC_ACCESS .
  data LAUNCHER type ref to ZIF_CDS_ALV_REPORT_LAUNCHER .
  data IOC_CONTAINER type ref to ZIF_CDS_ALV_IOC_CONTAINER .
  data NAVIGATION_TABLE type ZCDS_ALV_NAVIGATION_TAB .
  data NAVIGATION_EXITS type ZCDS_ALV_NAVIGATION_EXIT_TAB .

  methods ASK_FOR_MISSING_PARAMETERS
    importing
      !I_TARGET_VIEW type DDSTRUCOBJNAME
    changing
      !C_PARAMETER_VALUES type ZCDS_ALV_PARAMETERS
    raising
      ZCX_CDS_ALV_MESSAGE .
  methods CALL_BOR_METHOD
    importing
      !I_NAVIGATION type ZCDS_ALV_NAV
      !I_KEY_FIELD type FIELDNAME
      !I_SELECTED_ROW type ANY
    raising
      ZCX_CDS_ALV_MESSAGE .
  methods CALL_FUNCTION_MODULE
    importing
      !I_NAVIGATION type ZCDS_ALV_NAV
      !I_KEY_FIELD type FIELDNAME optional
      !I_SELECTED_ROW type ANY optional
      !I_SELECTED_ROWS type STANDARD TABLE optional
    raising
      ZCX_CDS_ALV_MESSAGE .
  methods CALL_TRANSACTION
    importing
      !I_NAVIGATION type ZCDS_ALV_NAV
      !I_KEY_FIELD type FIELDNAME
      !I_SELECTED_ROW type ANY
    raising
      ZCX_CDS_ALV_MESSAGE .
  methods CALL_OO_METHOD
    importing
      !I_NAVIGATION type ZCDS_ALV_NAV
      !I_KEY_FIELD type FIELDNAME optional
      !I_SELECTED_ROW type ANY optional
      !I_SELECTED_ROWS type STANDARD TABLE optional
    raising
      ZCX_CDS_ALV_MESSAGE .
  methods FILL_IOC_CONTAINER .
  methods GET_OBJECT_FROM_IOC_CONTAINER
    importing
      !I_NAVIGATION_EXIT type ZCDS_ALV_NAVEXIT
    returning
      value(R_OBJECT) type ref to ZIF_CDS_ALV_NAVIGATION
    raising
      ZCX_CDS_ALV_MESSAGE .
ENDCLASS.



CLASS ZCL_CDS_ALV_NAVIGATION IMPLEMENTATION.


  METHOD ask_for_missing_parameters.
    DATA(dd10bv_tab) = ddic_access->get_parameters_for_cds_view( i_target_view ).

    LOOP AT dd10bv_tab INTO DATA(dd10bv).
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(ddfield) = VALUE dfies( ).
      CALL FUNCTION 'DDIF_FIELDINFO_GET'
        EXPORTING  tabname   = dd10bv-rollname
                   all_types = abap_true
        IMPORTING  dfies_wa  = ddfield
        EXCEPTIONS OTHERS    = 1.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      READ TABLE c_parameter_values ASSIGNING FIELD-SYMBOL(<parameter_value>)
           WITH KEY cds_view = dd10bv-strucobjn
                    parname  = dd10bv-parametername.
      IF sy-subrc <> 0.
        INSERT VALUE #( cds_view = dd10bv-strucobjn
                        parname  = dd10bv-parametername )
               INTO TABLE c_parameter_values ASSIGNING <parameter_value>.
      ENDIF.

      IF <parameter_value>-value IS INITIAL.
        CALL FUNCTION 'FOBU_POPUP_GET_VALUE'
          EXPORTING  typename        = dd10bv-rollname
                     field_value     = <parameter_value>-value
                     popup_title     = 'Parameter eingeben'
          IMPORTING  field_value_int = <parameter_value>-value
          EXCEPTIONS internal_error  = 1
                     cancelled       = 2
                     OTHERS          = 3.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_cds_alv_message
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD call_bor_method.
    DATA objkey    TYPE swo_typeid.
    DATA object    TYPE swo_objhnd.
    DATA return    TYPE swotreturn.
    DATA container TYPE swconttab.

    ASSIGN COMPONENT i_key_field OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<key>).
    objkey = <key>.

    CALL FUNCTION 'SWO_CREATE'
      EXPORTING objtype = i_navigation-object_type
                objkey  = objkey
      IMPORTING object  = object
                return  = return.
    IF return-errortype <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'SWO_INVOKE'
      EXPORTING object    = object
                verb      = i_navigation-object_method
      IMPORTING return    = return
      TABLES    container = container.
    IF return-errortype <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'SWO_FREE'
      EXPORTING object = object
      IMPORTING return = return.
    IF return-errortype <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD call_function_module.
    DATA exceptions TYPE STANDARD TABLE OF rsexc.
    DATA exporting  TYPE STANDARD TABLE OF rsexp.
    DATA importing  TYPE STANDARD TABLE OF rsimp.
    DATA changing   TYPE STANDARD TABLE OF rscha.
    DATA tables     TYPE STANDARD TABLE OF rstbl.
    DATA input      TYPE REF TO data.

    FIELD-SYMBOLS <table> TYPE ANY TABLE.

    CALL FUNCTION 'FUNCTION_IMPORT_INTERFACE'
      EXPORTING  funcname           = i_navigation-function
      TABLES     exception_list     = exceptions
                 export_parameter   = exporting
                 import_parameter   = importing
                 changing_parameter = changing
                 tables_parameter   = tables
      EXCEPTIONS error_message      = 1
                 function_not_found = 2
                 invalid_name       = 3
                 OTHERS             = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE importing INTO DATA(import)
         WITH KEY parameter = i_navigation-default_parameter.
    IF sy-subrc = 0.
      IF import-dbfield IS NOT INITIAL.
        CREATE DATA input TYPE (import-dbfield).
      ELSEIF import-typ IS NOT INITIAL.
        CREATE DATA input TYPE (import-typ).
      ENDIF.
    ENDIF.

    READ TABLE tables INTO DATA(table)
         WITH KEY parameter = i_navigation-default_parameter.
    IF sy-subrc = 0.
      CREATE DATA input TYPE TABLE OF (table-dbstruct).
    ENDIF.

    IF input IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e005(zcds_alv) WITH i_navigation-default_parameter i_navigation-function.
    ENDIF.

    ASSIGN input->* TO FIELD-SYMBOL(<input>).

    IF i_navigation-conversion_exit IS NOT INITIAL.
      DATA(conversion_function) = |CONVERSION_EXIT_{ i_navigation-conversion_exit }_INPUT|.
    ENDIF.

    IF i_selected_row IS NOT INITIAL.
      ASSIGN COMPONENT i_key_field OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<key>).
      <input> = <key>.

      IF conversion_function IS NOT INITIAL.
        CALL FUNCTION conversion_function
          EXPORTING input  = <input>
          IMPORTING output = <input>.
      ENDIF.

    ELSEIF i_selected_rows IS NOT INITIAL.
      LOOP AT i_selected_rows ASSIGNING FIELD-SYMBOL(<selected_row>).
        ASSIGN COMPONENT i_key_field OF STRUCTURE <selected_row> TO <key>.
        ASSIGN <input> TO <table>.
        INSERT <key> INTO TABLE <table> ASSIGNING FIELD-SYMBOL(<line>).

        IF conversion_function IS NOT INITIAL.
          CALL FUNCTION conversion_function
            EXPORTING input  = <line>
            IMPORTING output = <line>.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DATA(parameter_binding) = COND abap_func_parmbind_tab(
      WHEN import-parameter IS NOT INITIAL THEN VALUE #( ( name  = import-parameter
                                                           kind  = abap_func_exporting
                                                           value = input ) )
      WHEN table-parameter IS NOT INITIAL  THEN VALUE #( ( name  = table-parameter
                                                           kind  = abap_func_tables
                                                           value = input ) ) ).

    DATA(exception_binding) = VALUE abap_func_excpbind_tab( ( name = `OTHERS` value = 1 ) ).

    CALL FUNCTION i_navigation-function
      PARAMETER-TABLE parameter_binding
      EXCEPTION-TABLE exception_binding.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD call_oo_method.
    DATA(class_descriptor) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_name( i_navigation-class ) ).
    DATA(parameter_type_descriptor) = class_descriptor->get_method_parameter_type( p_method_name    = i_navigation-method
                                                                                   p_parameter_name = i_navigation-method_parameter ).

    DATA input TYPE REF TO data.
    CREATE DATA input TYPE HANDLE parameter_type_descriptor.

    IF input IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e033(zcds_alv) WITH i_navigation-method_parameter i_navigation-class i_navigation-method.
    ENDIF.

    ASSIGN input->* TO FIELD-SYMBOL(<input>).

    IF i_navigation-conversion_exit IS NOT INITIAL.
      DATA(conversion_function) = |CONVERSION_EXIT_{ i_navigation-conversion_exit }_INPUT|.
    ENDIF.

    IF i_selected_row IS NOT INITIAL.
      ASSIGN COMPONENT i_key_field OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<key>).
      <input> = <key>.

      IF conversion_function IS NOT INITIAL.
        CALL FUNCTION conversion_function
          EXPORTING
            input  = <input>
          IMPORTING
            output = <input>.
      ENDIF.

    ELSEIF i_selected_rows IS NOT INITIAL.
      FIELD-SYMBOLS <table> TYPE ANY TABLE.

      LOOP AT i_selected_rows ASSIGNING FIELD-SYMBOL(<selected_row>).
        ASSIGN COMPONENT i_key_field OF STRUCTURE <selected_row> TO <key>.
        ASSIGN <input> TO <table>.
        INSERT <key> INTO TABLE <table> ASSIGNING FIELD-SYMBOL(<line>).

        IF conversion_function IS NOT INITIAL.
          CALL FUNCTION conversion_function
            EXPORTING
              input  = <line>
            IMPORTING
              output = <line>.
        ENDIF.
      ENDLOOP.
    ENDIF.

    READ TABLE class_descriptor->methods INTO DATA(method) WITH KEY name = i_navigation-method.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_cds_alv_message
            MESSAGE e035(zcds_alv) WITH i_navigation-class i_navigation-method.
    ENDIF.

    DATA(parameter_binding) = VALUE abap_parmbind_tab(
       ( name  = i_navigation-method_parameter
         kind  = cl_abap_objectdescr=>exporting
         value = input ) ).

    IF method-is_raising_excps IS NOT INITIAL.
      DATA(exception_binding) = VALUE abap_excpbind_tab( ( name = `OTHERS` value = 1 ) ).
    ENDIF.

    TRY.
        CASE method-is_class.
          WHEN abap_true.
            IF method-is_raising_excps IS NOT INITIAL.
              CALL METHOD (i_navigation-class)=>(i_navigation-method)
                   PARAMETER-TABLE parameter_binding
                   EXCEPTION-TABLE exception_binding.
              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE zcx_cds_alv_message
                      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDIF.
            ELSE.
              TRY.
                  CALL METHOD (i_navigation-class)=>(i_navigation-method)
                       PARAMETER-TABLE parameter_binding.

                CATCH cx_root.
                  RAISE EXCEPTION TYPE zcx_cds_alv_message
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDTRY.
            ENDIF.

          WHEN abap_false.
            DATA instance TYPE REF TO object.
            CREATE OBJECT instance TYPE (i_navigation-class).

            IF method-is_raising_excps IS NOT INITIAL.
              CALL METHOD instance->(i_navigation-method)
                   PARAMETER-TABLE parameter_binding
                   EXCEPTION-TABLE exception_binding.
              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE zcx_cds_alv_message
                      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDIF.
            ELSE.
              TRY.
                  CALL METHOD instance->(i_navigation-method)
                       PARAMETER-TABLE parameter_binding.

                CATCH cx_root.
                  RAISE EXCEPTION TYPE zcx_cds_alv_message
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDTRY.
            ENDIF.
        ENDCASE.

      CATCH cx_sy_create_object_error.
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE e036(zcds_alv) WITH i_navigation-class.

      CATCH cx_sy_dyn_call_error.
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE e034(zcds_alv) WITH i_navigation-class i_navigation-method.
    ENDTRY.
  ENDMETHOD.


  METHOD call_transaction.
    ASSIGN COMPONENT i_key_field OF STRUCTURE i_selected_row TO FIELD-SYMBOL(<key>).

    IF i_navigation-parameter_id IS NOT INITIAL.
      SET PARAMETER ID i_navigation-parameter_id FIELD <key>.
    ENDIF.

    CALL TRANSACTION i_navigation-transaction_code
         WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.
  ENDMETHOD.


  METHOD constructor.
    persistence = i_persistence.
    ddic_access = i_ddic_access.
    launcher    = i_launcher.
    navigation_table = persistence->get_intent_based_navigation( ).
    navigation_exits = persistence->get_navigation_exits( ).
    fill_ioc_container( ).
  ENDMETHOD.


  METHOD fill_ioc_container.
    ioc_container = NEW zcl_cds_alv_ioc_container( ).
    LOOP AT navigation_exits INTO DATA(navigation_exit).
      DATA(filters) = VALUE zif_cds_alv_ioc_container=>ty_filters(
                                ( key = 'SEMANTIC_OBJECT' value = navigation_exit-semantic_object )
                                ( key = 'SEMANTIC_ACTION' value = navigation_exit-semantic_action )
                                ( key = 'CDS_VIEW'        value = navigation_exit-cds_view ) ).

      TRY.
          ioc_container->register_implementing_class( i_interface = exit_interface
                                                      i_filters   = filters
                                                      i_class     = navigation_exit-implementing_class ).

        CATCH zcx_cds_alv_message.
          " TODO: log errors at this point
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_object_from_ioc_container.
    DATA(filters) = VALUE zif_cds_alv_ioc_container=>ty_filters(
                              ( key = 'SEMANTIC_OBJECT' value = i_navigation_exit-semantic_object )
                              ( key = 'SEMANTIC_ACTION' value = i_navigation_exit-semantic_action )
                              ( key = 'CDS_VIEW'        value = i_navigation_exit-cds_view ) ).

    r_object ?= ioc_container->resolve( i_interface = exit_interface i_filters = filters ).
  ENDMETHOD.


  METHOD zif_cds_alv_navigation~navigate_to_object_mass.
    e_refresh_after = abap_false.

    TRY.
        DATA(exit_called) = abap_false.
        DATA(navigation_exit) = navigation_exits[ semantic_object = i_object
                                                  semantic_action = i_action
                                                  cds_view        = i_cds_view ].

        DATA(exit_object) = get_object_from_ioc_container( navigation_exit ).

        exit_object->navigate_to_object_mass( EXPORTING i_object        = i_object
                                                        i_action        = i_action
                                                        i_cds_view      = i_cds_view
                                                        i_key_field     = i_key_field
                                                        i_selected_rows = i_selected_rows
                                              IMPORTING e_refresh_after = e_refresh_after ).

        exit_called = abap_true.
      CATCH cx_sy_itab_line_not_found.
        exit_called = abap_false.
    ENDTRY.

    IF exit_called = abap_true.
      RETURN.
    ENDIF.

    TRY.
        DATA(navigation) = navigation_table[ semantic_object = i_object
                                             semantic_action = i_action ].

        IF navigation-mass_processing = mass_processing-none AND lines( i_selected_rows ) > 1.
          RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e003(zcds_alv).
        ENDIF.

        CASE navigation-mass_processing.
          WHEN mass_processing-none
            OR mass_processing-loop.

            LOOP AT i_selected_rows ASSIGNING FIELD-SYMBOL(<selected_row>).
              IF     navigation-function          IS NOT INITIAL
                 AND navigation-default_parameter IS NOT INITIAL.
                call_function_module( i_navigation   = navigation
                                      i_key_field    = i_key_field
                                      i_selected_row = <selected_row> ).

              ELSEIF     navigation-object_type   IS NOT INITIAL
                     AND navigation-object_method IS NOT INITIAL.
                call_bor_method( i_navigation   = navigation
                                 i_key_field    = i_key_field
                                 i_selected_row = <selected_row> ).

              ELSEIF navigation-transaction_code IS NOT INITIAL.
                call_transaction( i_navigation   = navigation
                                  i_key_field    = i_key_field
                                  i_selected_row = <selected_row> ).

              ELSEIF     navigation-class            IS NOT INITIAL
                     AND navigation-method           IS NOT INITIAL
                     AND navigation-method_parameter IS NOT INITIAL.
                call_oo_method( i_navigation   = navigation
                                i_key_field    = i_key_field
                                i_selected_row = <selected_row> ).
              ENDIF.
            ENDLOOP.

          WHEN mass_processing-table.
            IF     navigation-function          IS NOT INITIAL
               AND navigation-default_parameter IS NOT INITIAL.
              call_function_module( i_navigation    = navigation
                                    i_key_field     = i_key_field
                                    i_selected_rows = i_selected_rows ).

            ELSEIF     navigation-class            IS NOT INITIAL
                   AND navigation-method           IS NOT INITIAL
                   AND navigation-method_parameter IS NOT INITIAL.
              call_oo_method( i_navigation    = navigation
                              i_key_field     = i_key_field
                              i_selected_rows = i_selected_rows ).
            ELSE.
              RAISE EXCEPTION TYPE zcx_cds_alv_message MESSAGE e004(zcds_alv).
            ENDIF.
        ENDCASE.

        e_refresh_after = navigation-refresh_after.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE e002(zcds_alv) WITH i_object i_action.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cds_alv_navigation~navigate_to_object_single.
    e_refresh_after = abap_false.

    TRY.
        DATA(exit_called) = abap_false.
        DATA(navigation_exit) = navigation_exits[ semantic_object = i_object
                                                  semantic_action = i_action
                                                  cds_view        = i_cds_view ].

        DATA(exit_object) = get_object_from_ioc_container( navigation_exit ).

        exit_object->navigate_to_object_single( EXPORTING i_object        = i_object
                                                          i_action        = i_action
                                                          i_cds_view      = i_cds_view
                                                          i_key_field     = i_key_field
                                                          i_selected_row  = i_selected_row
                                                IMPORTING e_refresh_after = e_refresh_after ).

        exit_called = abap_true.
      CATCH cx_sy_itab_line_not_found.
        exit_called = abap_false.
    ENDTRY.

    IF exit_called = abap_true.
      RETURN.
    ENDIF.

    TRY.
        DATA(navigation) = navigation_table[ semantic_object = i_object
                                             semantic_action = i_action ].

        IF     navigation-function          IS NOT INITIAL
           AND navigation-default_parameter IS NOT INITIAL.
          call_function_module( i_navigation   = navigation
                                i_key_field    = i_key_field
                                i_selected_row = i_selected_row ).

        ELSEIF     navigation-object_type   IS NOT INITIAL
               AND navigation-object_method IS NOT INITIAL.
          call_bor_method( i_navigation   = navigation
                           i_key_field    = i_key_field
                           i_selected_row = i_selected_row ).

        ELSEIF navigation-transaction_code IS NOT INITIAL.
          call_transaction( i_navigation   = navigation
                            i_key_field    = i_key_field
                            i_selected_row = i_selected_row ).

        ELSEIF     navigation-class            IS NOT INITIAL
               AND navigation-method           IS NOT INITIAL
               AND navigation-method_parameter IS NOT INITIAL.
          call_oo_method( i_navigation   = navigation
                          i_key_field    = i_key_field
                          i_selected_row = i_selected_row ).
        ENDIF.

        e_refresh_after = navigation-refresh_after.

      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_cds_alv_message
              MESSAGE e002(zcds_alv) WITH i_object i_action.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_cds_alv_navigation~navigate_via_association.
    DATA(target_view) = ddic_access->get_target_for_association( i_source_view      = i_source_view
                                                                 i_association_name = i_association_name ).

    DATA(target_parameters) = i_target_parameters.

    ask_for_missing_parameters( EXPORTING i_target_view      = target_view
                                CHANGING  c_parameter_values = target_parameters ).

    launcher->start_report_for_association( i_source_view       = i_source_view
                                            i_target_view       = target_view
                                            i_association_name  = i_association_name
                                            i_source_parameters = i_source_parameters
                                            i_target_parameters = target_parameters
                                            i_forall_table      = i_selected_rows ).
  ENDMETHOD.
ENDCLASS.
