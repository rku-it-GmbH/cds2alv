class /C2A/CX_ERROR_MESSAGE definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_ALV_MESSAGE .
  interfaces IF_T100_MESSAGE .
  interfaces IF_T100_DYN_MSG .

  aliases MSGTY
    for IF_T100_DYN_MSG~MSGTY .
  aliases MSGV1
    for IF_T100_DYN_MSG~MSGV1 .
  aliases MSGV2
    for IF_T100_DYN_MSG~MSGV2 .
  aliases MSGV3
    for IF_T100_DYN_MSG~MSGV3 .
  aliases MSGV4
    for IF_T100_DYN_MSG~MSGV4 .
  aliases T100KEY
    for IF_T100_MESSAGE~T100KEY .
  aliases GET_MESSAGE
    for IF_ALV_MESSAGE~GET_MESSAGE .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYMSGTY optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
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



CLASS /C2A/CX_ERROR_MESSAGE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD IF_ALV_MESSAGE~GET_MESSAGE.
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


  METHOD PROBLEM_CLASS_FOR_MESSAGE_TYPE.
    r_problem_class = SWITCH #( i_message_type
                                WHEN message_type-exit    OR message_type-abort THEN problem_class-very_high
                                WHEN message_type-error                         THEN problem_class-high
                                WHEN message_type-warning                       THEN problem_class-medium
                                WHEN message_type-success OR message_type-info  THEN problem_class-low
                                ELSE                                                 problem_class-others ).
  ENDMETHOD.
ENDCLASS.
