PROCESS BEFORE OUTPUT.
  MODULE status_0200.
  MODULE build_grid_0200.

  CALL SUBSCREEN selection_subscreen INCLUDING sub_repid sub_dynnr.

PROCESS AFTER INPUT.
  CALL SUBSCREEN selection_subscreen.

  MODULE exit AT EXIT-COMMAND.
  MODULE user_command.
