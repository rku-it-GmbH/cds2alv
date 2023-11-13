CLASS zcl_cds_alv_memory DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_cds_alv_memory.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cds_alv_memory IMPLEMENTATION.
  METHOD zif_cds_alv_memory~export_forall_table.
    DATA ref_to_forall_table TYPE REF TO data.

    CREATE DATA ref_to_forall_table TYPE STANDARD TABLE OF (i_source_view).
    ASSIGN ref_to_forall_table->* TO FIELD-SYMBOL(<forall_table>).

    MOVE-CORRESPONDING i_forall_table TO <forall_table>.

    EXPORT source_view       = i_source_view
           association_name  = i_association_name
           source_parameters = i_source_parameters
           forall_table      = <forall_table>
           TO MEMORY ID i_memory_id.
  ENDMETHOD.

  METHOD zif_cds_alv_memory~import_forall_table.
    IMPORT source_view       = e_source_view
           association_name  = e_association_name
           source_parameters = e_source_parameters
           FROM MEMORY ID i_memory_id.

    CREATE DATA e_ref_to_forall_table TYPE STANDARD TABLE OF (e_source_view).
    ASSIGN e_ref_to_forall_table->* TO FIELD-SYMBOL(<forall_table>).

    IMPORT forall_table = <forall_table> FROM MEMORY ID i_memory_id.
  ENDMETHOD.
ENDCLASS.
