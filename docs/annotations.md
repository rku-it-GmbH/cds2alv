# Annotations
The following CDS annotations are evaluated by the components of the CDS ALV framework.

## Base
* EndUserText.label - Descriptive name of the CDS entity

## Grid Builder
* Consumption.hidden - field is treated as technical by ALV
* Consumption.valueHelp - custom F4 help for the field in the ALV grid
* Consumption.valueHelpDefinition - custom F4 help for the field in the ALV grid
* Consumption.semanticObject - semantic object for Intent-Based Navigation (Hotspot or Button)
* DefaultAggregation - aggregation is applied to the respective column
* EndUserText.label - column heading
* EndUserText.quickInfo - column tooltip
* Semantics.email.address - column becomes hotspot, link click to send mail
* UI.hidden - field is hidden in the ALV grid by default
* UI.lineItem.position - column position in the ALV grid
* UI.lineItem.label - column heading
* UI.lineItem.type - behaviour according to type:
	* #STANDARD or none: plain column
	* #FOR_ACTION: button for a BOPF action
	* #FOR_INTENT_BASED_NAVIGATION: button for Intent-Based Navigation
	* #WITH_INTENT_BASED_NAVIGATION: hotspot for Intent-Based Navigation
	* #WITH_URL: hotspot for an HTTP call
	* #WITH_NAVIGATION_PATH: hotspot for calling the report of an associated CDS view with the current display as forall-table
* UI.lineItem.semanticObject - semantic object for Intent-Based Navigation (Hotspot or Button)
* UI.lineItem.semanticObjectAction - semantic action for Intent-Based Navigation (Hotspot or Button)
* UI.lineItem.targetElement - target path of the navigation via association
* UI.lineItem.dataAction - name of the BOPF action
* UI.lineItem.url - target URL for the HTTP call 		
* ObjectModel.semanticKey - key columns in the ALV grid
* ObjectModel.deleteEnabled - if true, adds user command DELETE to the alv grid
* ObjectModel.updateEnabled - if true, the ALV becomes editable, adds user command SAVE to the alv grid
* ObjectModel.readOnly - column remains read-only

## Report Generation Strategy and Selection Screen
* AbapCatalog.sqlViewName - used to compose the name of the generated report
* Consumption.filter.selectionType - displays the field on the selection screen. Restrictions apply:
	* #SINGLE: NO-EXTENSIONS and NO INTERVALS, only EQ allowed
	* #INTERVAL: NO-EXTENSIONS, only EQ and BT allowed
	* #RANGE: no restrictions
* Consumption.filter.multipleSelections - restricts the selection
* Consumption.filter.mandatory - marks the field as mandatory on the selection screen
* Consumption.filter.defaultValue - default value for a select option on the selection screen
* Consumption.defaultValue - default value for a parameter on the selection screen
* Consumption.valueHelp - custom F4 help for the field in the selection screen
* Consumption.valueHelpDefinition - custom F4 help for the field in the selection screen
* Consumption.hidden - field is not displayed on the selection screen
* EndUserText.label - label for the line on the selection screen
* UI.selectionField.position - position of the field on the selection screen
* UI.selectionField.qualifier - used to group the selection fields into separate blocks on the selection screen

## Table Container
* ObjectModel.updateEnabled - if true, add style table field to the ALV grid
* UI.lineItem.criticality - add color table field to the ALV grid

## Value Help
* Consumption.valueHelp - association that is used for the value help
* Consumption.valueHelpDefinition.association - association that is used for the value help
* Consumption.valueHelpDefinition.entity.name - name of the target CDS entity
* Consumption.valueHelpDefinition.entity.element - name of the target CDS element
* Consumption.valueHelpDefinition.distinctValues - selection is executed as SELECT DISTINCT
* Consumption.valueHelpDefinition.label - not yet used
* Consumption.valueHelpDefinition.additionalBinding.localParameter - name of the source parameter for an ON-condition
* Consumption.valueHelpDefinition.additionalBinding.localElement - name of the source element for an ON-condition
* Consumption.valueHelpDefinition.additionalBinding.parameter - name of the target parameter for an ON-condition
* Consumption.valueHelpDefinition.additionalBinding.element - name of the target element for an ON-condition
* Consumption.valueHelpDefinition.additionalBinding.usage - controls whether the binding is used as filter or result