bugfixes
--------

* collectSameName is pure-garbage and buggy. There's an edge case see,
  testcast #157.

refactor
--------

* try to remove all call to "++" and ":::" and use prepending instead of
  appending to list, this is more about rewriting a lot of scalatest cases.

helper
------

* port language_agnostic.py in scala for better debugging capability.
