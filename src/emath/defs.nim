import std/[math, tables]
import model

# TODO add macro for converting a `typed fn` to equivalent `emath fn`

func esin*(args: seq[float]): float =
  assert args.len == 1
  sin args[0]

func ecos*(args: seq[float]): float =
  assert args.len == 1
  cos args[0]

func elog*(args: seq[float]): float =
  assert args.len == 2
  log args[0], args[1]


const
  defaultFns*: MathFnLookup = toTable {
    "sin": esin,
    "cos": ecos,
    "log": elog,
  }

  defaultVars*: MathVarLookup = toTable {
    "PI": PI,
    "TAU": TAU,
    "e": E
  }
