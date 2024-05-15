*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCDS_ALV_EXTHDR.................................*
DATA:  BEGIN OF STATUS_ZCDS_ALV_EXTHDR               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCDS_ALV_EXTHDR               .
CONTROLS: TCTRL_ZCDS_ALV_EXTHDR
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZCDS_ALV_EXTPAR.................................*
DATA:  BEGIN OF STATUS_ZCDS_ALV_EXTPAR               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCDS_ALV_EXTPAR               .
CONTROLS: TCTRL_ZCDS_ALV_EXTPAR
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZCDS_ALV_IOCCLIF................................*
DATA:  BEGIN OF STATUS_ZCDS_ALV_IOCCLIF              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCDS_ALV_IOCCLIF              .
CONTROLS: TCTRL_ZCDS_ALV_IOCCLIF
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZCDS_ALV_NAV....................................*
DATA:  BEGIN OF STATUS_ZCDS_ALV_NAV                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCDS_ALV_NAV                  .
CONTROLS: TCTRL_ZCDS_ALV_NAV
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZCDS_ALV_NAVEXIT................................*
DATA:  BEGIN OF STATUS_ZCDS_ALV_NAVEXIT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCDS_ALV_NAVEXIT              .
CONTROLS: TCTRL_ZCDS_ALV_NAVEXIT
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZMCDSALVPROGRAM.................................*
TABLES: ZMCDSALVPROGRAM, *ZMCDSALVPROGRAM. "view work areas
CONTROLS: TCTRL_ZMCDSALVPROGRAM
TYPE TABLEVIEW USING SCREEN '0006'.
DATA: BEGIN OF STATUS_ZMCDSALVPROGRAM. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMCDSALVPROGRAM.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMCDSALVPROGRAM_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMCDSALVPROGRAM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMCDSALVPROGRAM_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMCDSALVPROGRAM_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMCDSALVPROGRAM.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMCDSALVPROGRAM_TOTAL.

*.........table declarations:.................................*
TABLES: *ZCDS_ALV_EXTHDR               .
TABLES: *ZCDS_ALV_EXTHDRT              .
TABLES: *ZCDS_ALV_EXTPAR               .
TABLES: *ZCDS_ALV_EXTPART              .
TABLES: *ZCDS_ALV_IOCCLIF              .
TABLES: *ZCDS_ALV_NAV                  .
TABLES: *ZCDS_ALV_NAVEXIT              .
TABLES: ZCDS_ALV_EXTHDR                .
TABLES: ZCDS_ALV_EXTHDRT               .
TABLES: ZCDS_ALV_EXTPAR                .
TABLES: ZCDS_ALV_EXTPART               .
TABLES: ZCDS_ALV_IOCCLIF               .
TABLES: ZCDS_ALV_NAV                   .
TABLES: ZCDS_ALV_NAVEXIT               .
TABLES: ZCDS_ALV_PROGRAM               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
