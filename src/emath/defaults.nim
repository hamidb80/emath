import std/[math, tables, macros]
import model


macro toEmathFn(fnIdent: untyped, argsLen: static[int]): untyped =
  let
    fnStr = fnIdent.strval
    call = newCall(fnIdent)
    argsId = ident"args"
    returnType = quote: seq[float]
    argsLenCheck = quote:
      doAssert `argsId`.len == `argsLen`:
        "invalid number of arguments for function " & `fnStr` &
        ". expected " & $`argsLen` & " but given " & $`argsId`.len

  for n in 1..argsLen:
    call.add newTree(nnkBracketExpr, argsId, newlit n-1)

  newProc(
    params = @[ident"float", newIdentDefs(argsId, returnType)],
    procType = nnkFuncDef,
    body = newStmtList(argsLenCheck, call))


const
  defaultFns*: MathFnLookup = block:
    var res: MathFnLookup
    res["sin"] = toEmathFn(sin, 1)
    res["cos"] = toEmathFn(cos, 1)
    res["tan"] = toEmathFn(tan, 1)
    res["cot"] = toEmathFn(cot, 1)
    res["sec"] = toEmathFn(sec, 1)
    res["csc"] = toEmathFn(csc, 1)
    res["sinh"] = toEmathFn(sinh, 1)
    res["cosh"] = toEmathFn(cosh, 1)
    res["tanh"] = toEmathFn(tanh, 1)
    res["coth"] = toEmathFn(coth, 1)
    res["sech"] = toEmathFn(sech, 1)
    res["csch"] = toEmathFn(csch, 1)
    res["arcsin"] = toEmathFn(arcsin, 1)
    res["arccos"] = toEmathFn(arccos, 1)
    res["arctan"] = toEmathFn(arctan, 1)
    res["arccot"] = toEmathFn(arccot, 1)
    res["arcsec"] = toEmathFn(arcsec, 1)
    res["arccsc"] = toEmathFn(arccsc, 1)
    res["arcsinh"] = toEmathFn(arcsinh, 1)
    res["arccosh"] = toEmathFn(arccosh, 1)
    res["arctanh"] = toEmathFn(arctanh, 1)
    res["arccoth"] = toEmathFn(arccoth, 1)
    res["arcsech"] = toEmathFn(arcsech, 1)
    res["arccsch"] = toEmathFn(arccsch, 1)
    res["log"] = toEmathFn(log, 2)
    res["log2"] = toEmathFn(log2, 1)
    res["lg"] = toEmathFn(log2, 1)
    res["ln"] = toEmathFn(ln, 1)
    res["log10"] = toEmathFn(log10, 1)
    res["sqrt"] = toEmathFn(sqrt, 1)
    res

  defaultVars*: MathVarLookup = toTable {
    "PI": PI,
    "TAU": TAU,
    "e": E
  }
