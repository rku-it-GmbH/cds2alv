# Program extensions

Program extensions are plugins for the reports genereated by the framework. 
They can be switched on and off for each individual CDS view in the first screen of the transaction *ZCDS_ALV_START*.

## Defining a Program extension

Program extensions are defined in the Viewcluster *ZVC_CDS_ALV_FW*.
The definition of a program extension consists of the following information:
* database key
* display name
* implementing class
* descriptive texts for alternative selection or display, if any of these are impemented
* addtional parameters that are added to the selection screen and bound to attributes of the implementing class

## Implementing a Program extension 

The implementing class of a program extension must implement the interface *ZIF_CDS_ALV_REPORT_EXTENSION*, 
furthermore it is recommended to inherit from the abstract class *ZCL_CDS_ALV_REPORT_EXTENSION* or a suitable subclass.

Program extensions can enhance the following events:

* INITIALIZATION (initialization)
* AT SELECTION-SCREEN OUTPUT (modify_screen)
* AT SELECTION-SCREEN (handle_user_command)
* AT SELECTION-SCREEN ON HELP-REQUEST (show_help)
* AT SELECTION-SCREEN ON VALUE-REQUEST (value_help)
* START-OF-SELECTION:
	* alternative_selection to replace the selection via the SADL framework
	* alternative_display to replace the ALV grid display

Moreover, the method *alternative_reselection* needs to be implemented for handling the ALV user command *REFRESH*, if an alternative selection is defined

## Collection of Program extensions

A collection of program extensions will be made available as a separate repository in the near future.