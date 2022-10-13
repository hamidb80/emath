# Package

version       = "0.0.1"
author        = "hamidb80"
description   = "math parser/evaluator"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.6"

task docs, "generate docs":
    exec "cd src/; nim --project doc emath.nim"