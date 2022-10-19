import emath, nimkalc, mathexpr, benchy

## The result on my laptop:
##   min time    avg time  std dv   runs name
##   0.077 ms    0.188 ms  ±0.136  x1000 emath
##   0.164 ms    0.292 ms  ±0.095  x1000 nimkalc
##   0.011 ms    0.019 ms  ±0.009  x1000 mathexpr
##
## as you can see, `emath` is 10x slower than `mathexpr`
## and it's almost 2x more perfromante than `nimkalc`
##
## `meath` like `nimkalc` creates intermidiate AST whereas `mathexpr`
## evaluates expressions immidiately.


let expr =
    # "-10^2" # <---- bug in `mathexpr` and
    "1 / 5 * ((5^2 + 45 +1 / 5^2 - 1)* log10(5 + sqrt(5^2 - 1)) - (5 +3) / sqrt(5^2 -1))"


timeit "emath":
    discard emath.eval(emath.parse(expr))

timeit "nimkalc":
    discard nimkalc.eval(expr)

let e = newEvaluator()
timeit "mathexpr":
    discard e.eval(expr)

block test:
    echo emath.eval(emath.parse(expr))
    echo nimkalc.eval(expr)
    echo e.eval(expr)
