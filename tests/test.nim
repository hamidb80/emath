import std/unittest
import emath


when isMainModule:
  # let r = parse "1 + 2 * 3 ^ 4 - 5 * 6 * 7"

  for t in ["-1", "-1 + 3"]:
    let r = parse t
    echo r
    echo treerepr r
    echo eval r
    echo "------------------"
