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

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
