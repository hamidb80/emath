>  Prophet Muhammad asked his companions, “Do you know who is the strongest person?” His companions replied, “The one who is able to wrestle others down.” The Prophet responded, “No, it is the one who is able to control their anger".

# Introduction
`emath` is library that provides functionality to parse/evaluate math experssions.

## Flow
```nim
         parsing          evaluating          
string  ─────────►  AST ────────────►  number
```

## Usage
the API functions should be used in this way:
```nim
echo "1 + sin(PI)".parse.eval
```

there is another form of `eval` function which also takes variable-lookup and function-lookup tables.

## Documentation
for more information see documentations [here](https://hamidb80.github.io/emath/).

## Advantages
parsing the expression to the actual AST, gives you the power and flexibility to do whatever processings you want with that. for example you may want to remove unnecessary parentheses around the values.

## Inspiration
I discovered `tree-sitter` some times ago, and I watched a video on youtube about it in which the author explained some form of `LR` parsers as the base of his library.

Honestly despite searching on the internet about it, I couldn't understand much, but I could guess what it's doing by the very simple example that he provided about `LR` parsers and the generated codes from `tree-sitter` library.

## Implementation
First, the lexer groups the raw exression (which is given as an `string`) to tokens. Tokens are one of these 6 types, either a `number` or an ident (like `var`) or `operator` or `(` or `)` or `,`.

Then the lexer passes tokens one-by-one to the parser. The parser is the *heart* of the library, it first creates an AST with an empty parenthesis as its child, then it increamentally updates that. the generated AST could be consisted of some simple Nodes like **literals** (like `1.2`) and **variable names** (like `PI`) or some compound nodes like **parentheses** `()` (which must contain only 1 sub node) or a **function call** which is consists of the caller name and its arguments, and **prefix** which has an operator and a node, and **infix** which has a operator and 2 sub nodes.

### Time complexity
as parser increamentally iterates over tokens and updates the AST, it should have `O(n)` time complexity.

### Example
**NOTEs:** 
- the nesting means it's inside of the upper node.
- I keep track of focused nodes with `#n` in the codes. it's actually a stack that contains nodes with index of `n-1`.

Imagine you have `1 + 2 * 3 ^ 4 < 5` as the expression, your initial node is a parentheses.
```nim
Par # 1
```

The first token is number `1`. we simply add the number to the upper parenthesis:
```nim
Par #1
    Lit 1 #2
```

Obviosly if you hit a number or a open/closed parentheses instead of an operator after a number, it would be an invalid token.

The next token is operator `+`. we are going to transfrom the tree:

```nim
Par #1
    Infix + #2
        Lit 1
```

Notice that a complete infix has 2 nodes, but here it has given one, we expect that it would be completed in next iteratations.

The next token is number `2`. we add `2` to the infix `+` to have a complete infix.

```nim
Par #1
    Infix + #2
        Lit 1
        Lit 2 #3
```

The next token is operator `*`. which has higher priority over `+` operator and we should apply it to our tree:


```nim
Par #1
    Infix + #2
        Lit 1 
        Infix * #3
            Lit 2
```

The next token is number `3`.

```nim
Par #1
    Infix + #2
        Lit 1 
        Infix * #3
            Lit 2 
            Lit 3 #4
```

The next token is operator `^`. which has higher priority over `*`:

```nim
Par #1
    Infix + #2
        Lit 1 
        Infix * #3
            Lit 2 
            Infix ^ #4
                Lit 3
```

The next token is number `4`.

```nim
Par #1
    Infix + #2
        Lit 1 
        Infix * #3
            Lit 2 
            Infix ^ #4
                Lit 3
                Lit 4 #5
```

The next token is operator `<`. which has lower priority than `^`. we actually go up from the last node (literal `4`) until we hit a lower priority operator or we hit a wrapper (parenthesis or a function call). so here we add the infix on top.


```nim
Par #1
    Infix < #2
        Infix +
            Lit 1 
            Infix *
                Lit 2 
                Infix ^
                    Lit 3
                    Lit 4
```

the next token is number `5`.


```nim
Par #1
    Infix < #2
        Infix +
            Lit 1 
            Infix *
                Lit 2 
                Infix ^
                    Lit 3
                    Lit 4
        Lit 5 #3
```

We made our correct AST! I didn't convered a lot of things like prefixes and function calls, but they are not that much hard to think of.

## Similar Projects
- [nimkalc](https://github.com/nocturn9x/nimkalc)
- [mathexpr](https://github.com/Yardanico/nim-mathexpr/)
- [kexpr](https://github.com/brentp/kexpr-nim)
