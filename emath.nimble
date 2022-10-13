# Package

version       = "0.0.5"
author        = "hamidb80"
description   = "math parser/evaluator library"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.6"

task docs, "generate docs":
    exec "cd src/; nim -f --project doc emath.nim"
