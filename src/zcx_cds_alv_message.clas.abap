CLASS zcx_cds_alv_message DEFINITION PUBLIC INHERITING FROM cx_static_check FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_alv_message.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    ALIASES get_message FOR if_alv_message~get_message.
    ALIASES t100key     FOR if_t100_message~t100key.
    ALIASES msgty       FOR if_t100_dyn_msg~msgty.
    ALIASES msgv1       FOR if_t100_dyn_msg~msgv1.
    ALIASES msgv2       FOR if_t100_dyn_msg~msgv2.
    ALIASES msgv3       FOR if_t100_dyn_msg~msgv3.
    ALIASES msgv4       FOR if_t100_dyn_msg~msgv4.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF message_type,
                 info    TYPE symsgty VALUE 'I',
                 success TYPE symsgty VALUE 'S',
                 warning TYPE symsgty VALUE 'W',
                 error   TYPE symsgty VALUE 'E',
                 abort   TYPE symsgty VALUE 'A',
                 exit    TYPE symsgty VALUE 'X',
               END OF message_type.

    CONSTANTS: BEGIN OF problem_class,
                 very_high TYPE balprobcl VALUE '1',
                 high      TYPE balprobcl VALUE '2',
                 medium    TYPE balprobcl VALUE '3',
                 low       TYPE balprobcl VALUE '4',
                 others    TYPE balprobcl VALUE '5',
               END OF problem_class.

    METHODS problem_class_for_message_type
      IMPORTING i_message_type         TYPE symsgty
      RETURNING VALUE(r_problem_class) TYPE balprobcl.
ENDCLASS.


CLASS zcx_cds_alv_message IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
    IF msgty IS INITIAL.
      msgty = message_type-error.
    ENDIF.
  ENDMETHOD.

  METHOD if_alv_message~get_message.
    r_s_msg = VALUE #( msgid = t100key-msgid
                       msgno = t100key-msgno
                       msgty = msgty
                       msgv1 = msgv1
                       msgv2 = msgv2
                       msgv3 = msgv3
                       msgv4 = msgv4 ).

    r_s_msg-probclass = problem_class_for_message_type( r_s_msg-msgty ).
    GET TIME STAMP FIELD r_s_msg-time_stmp.
  ENDMETHOD.

  METHOD problem_class_for_message_type.
    r_problem_class = SWITCH #( i_message_type
                                WHEN message_type-exit    OR message_type-abort THEN problem_class-very_high
                                WHEN message_type-error                         THEN problem_class-high
                                WHEN message_type-warning                       THEN problem_class-medium
                                WHEN message_type-success OR message_type-info  THEN problem_class-low
                                ELSE                                                 problem_class-others ).
  ENDMETHOD.
ENDCLASS.
