CLASS zcl_cds_alv_report_launcher DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_report_launcher.

    DATA ddic_access  TYPE REF TO zif_cds_alv_ddic_access.
    DATA persistence  TYPE REF TO zif_cds_alv_persistence.
    DATA memory       TYPE REF TO zif_cds_alv_memory.
    DATA generator    TYPE REF TO zif_cds_alv_report_generator.
    DATA auth_checker TYPE REF TO zif_cds_alv_authority_check.

    METHODS constructor
      IMPORTING i_ddic_access  TYPE REF TO zif_cds_alv_ddic_access
                i_persistence  TYPE REF TO zif_cds_alv_persistence
                i_memory       TYPE REF TO zif_cds_alv_memory
                i_generator    TYPE REF TO zif_cds_alv_report_generator
                i_auth_checker TYPE REF TO zif_cds_alv_authority_check.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS ensure_generation
      IMPORTING i_cds_view TYPE ddstrucobjname
      RAISING   zcx_cds_alv_message.
ENDCLASS.



CLASS zcl_cds_alv_report_launcher IMPLEMENTATION.
  METHOD constructor.
    ddic_access  = i_ddic_access.
    persistence  = i_persistence.
    memory       = i_memory.
    generator    = i_generator.
    auth_checker = i_auth_checker.
  ENDMETHOD.

  METHOD ensure_generation.
    DATA(view_modified_at) = ddic_access->get_last_modified_at( i_cds_view ).

    TRY.
        DATA(program) = persistence->get_report_for_cds_view( i_cds_view ).
        DATA(extensions_active_since) = VALUE timestamp( ).
        LOOP AT persistence->get_report_extensions( i_cds_view = i_cds_view i_only_active = abap_false ) INTO DATA(extension).
          extensions_active_since = nmax( val1 = extensions_active_since val2 = extension-activated_on ).
        ENDLOOP.

      CATCH zcx_cds_alv_message INTO DATA(message).
        DATA(no_program) = abap_true.
        IF program-no_generation = abap_true.
          RAISE EXCEPTION message.
        ENDIF.
    ENDTRY.

    IF     (      no_program              = abap_true
             OR ( view_modified_at        > program-generated_at )
             OR ( extensions_active_since > program-generated_at ) )
       AND program-no_generation = abap_false.

      generator->generate_report( i_cds_view ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_cds_alv_report_launcher~run_in_dark_mode.
    ensure_generation( i_cds_view ).
    DATA(program) = persistence->get_report_for_cds_view( i_cds_view ).

    DATA(read_metadata) = xsdbool(    e_layout IS SUPPLIED OR e_field_catalog IS SUPPLIED
                                   OR e_filter IS SUPPLIED OR e_sort          IS SUPPLIED ).

    DATA(read_data) = xsdbool( e_ref_to_table IS SUPPLIED OR e_table_descriptor IS SUPPLIED ).

    TRY.
        cl_salv_bs_runtime_info=>set( display  = abap_false
                                      metadata = read_metadata
                                      data     = read_data ).

        IF i_variant IS NOT INITIAL.
          SUBMIT (program-progname) USING SELECTION-SET i_variant AND RETURN.
        ELSE.
          SUBMIT (program-progname) WITH SELECTION-TABLE i_selection_table AND RETURN.
        ENDIF.

        IF read_metadata = abap_true.
          DATA(metadata) = cl_salv_bs_runtime_info=>get_metadata( ).
          e_layout = metadata-s_layout.
          e_field_catalog = metadata-t_fcat.
          e_filter = metadata-t_filter.
          e_sort = metadata-t_sort.
        ENDIF.

        IF read_data = abap_true.
          cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data       = DATA(ref_to_data)
                                                           r_data_descr = DATA(data_descriptor) ).

          e_ref_to_table ?= ref_to_data.
          e_table_descriptor ?= data_descriptor.
        ENDIF.

        cl_salv_bs_runtime_info=>clear_all( ).
      CATCH cx_salv_bs_sc_runtime_info INTO DATA(previous).
        cl_salv_bs_runtime_info=>clear_all( ).
        RAISE EXCEPTION TYPE zcx_cds_alv_message
          EXPORTING previous = previous.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_cds_alv_report_launcher~start_report_for_association.
    DATA selection_table     TYPE STANDARD TABLE OF rsparamsl_255.
    DATA ref_to_forall_table TYPE REF TO data.
    FIELD-SYMBOLS <forall_table> TYPE STANDARD TABLE.

    ensure_generation( i_target_view ).

    DATA(program) = persistence->get_report_for_cds_view( i_source_view ).

    LOOP AT program-parameters INTO DATA(parameter).
      READ TABLE i_target_parameters INTO DATA(parameter_value)
           WITH KEY parname = parameter-parname.
      IF sy-subrc = 0.
        INSERT VALUE #( kind = 'P' selname = parameter-sel_name low = parameter_value-value )
               INTO TABLE selection_table.
      ENDIF.
    ENDLOOP.

    DATA(memory_id) = CONV zcds_alv_memory_id( |{ i_source_view }\\{ i_association_name }| ).

    CREATE DATA ref_to_forall_table TYPE TABLE OF (i_source_view).
    ASSIGN ref_to_forall_table->* TO <forall_table>.
    MOVE-CORRESPONDING i_forall_table TO <forall_table>.

    memory->export_forall_table( i_memory_id         = memory_id
                                 i_source_view       = i_source_view
                                 i_association_name  = i_association_name
                                 i_source_parameters = i_source_parameters
                                 i_forall_table      = <forall_table> ).

    selection_table = VALUE #( BASE selection_table
                               kind = 'P'
                               ( selname = 'P_FORALL' low = abap_true )
                               ( selname = 'P_MEM_ID' low = memory_id ) ).

    SUBMIT (program-progname) WITH SELECTION-TABLE selection_table AND RETURN. "#EC CI_SUBMIT

    FREE MEMORY ID memory_id.
  ENDMETHOD.

  METHOD zif_cds_alv_report_launcher~start_report_for_view.
    ensure_generation( i_cds_view ).
    DATA(program) = persistence->get_report_for_cds_view( i_cds_view ).
    SUBMIT (program-progname) WITH p_split = i_in_split_screen VIA SELECTION-SCREEN AND RETURN. "#EC CI_SUBMIT
  ENDMETHOD.
ENDCLASS.
