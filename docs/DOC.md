# ScopeLand

A functional programming language, where the abstract syntax is exposed to the programmer.

# Language Constructs
## Statements

Statements are simply expressions, which may optionally be given a name for easier referencing.

In the following example is two statements, both a constant integer expression, but one is given the name 'my_name'.

```
...
my_name: 1337,
9001,
...
```

## Expressions

### Constant

An integer which might be negative.
```
1337
-9001
```

### Route

This is the way to access other defined statements in the program. There are a handful of elements that can be part of a route, called steps, which are chained together with '.' (a dot).
| Syntax | Explaination |
|---|---|
| _name_ | Lookup the value of a named statement, in the current scope. |
| [_expr_] | Index into a scope, first element is at 0, and lookup with negative values, starts for the last defined element, i.e. [-1] referes to the previous statement. |
| ^ | Goes to the containing scope, or crashes if there is no containing scope. | 

Here are a few syntax examples, we go in further details in [Routing](#routing).
```
my_name
^.other_name
^.^.[-1].[0]
```

### Binary Operation

Used to compute over two elements, currently support: <br>
+, -, *, =, !=, <, >, <=, >=

### Scope

A collection of statements, by way of a comma-separated array. A statement can refer directly to other statements in the scope, which are defined prior to itself. When evaluated, then result is the value of the last statement in the scope, or '0' if the scope is empty. 

Examples:
```
...
consts: [
    one: 1,
    two: 2,
    ten: 10
],
[
    incr: [\n -> n + 1],
    double: [\n -> 2 * n],
]
```

### Func

A scope with a positive number of undefined, but named, statements. These statements are defined at function application. Applications can be partial, and functions can return functions.

Examples:
```
double: [\n -> 2 * n],
sum: [\a -> if a then (^.sum (a - 1)) + a else 0],
fib: [\a -> match a with 
    | 0 -> 1
    | 1 -> 1 
    | _ -> (^.fib (a - 1)) + (^.fib (a - 2))
],
add: [\a b -> a + b],
sub: [\a -> [\b -> ^.a - b]],
```

### If

Computes one of two expressions, depending of wether or not another expression is non-zero (zero is false).

Examples:
```
if a > 2 then a else 3
```

### Call

This is applying an argument to a function.

Examples:
```
fib 10,
^.funcs.double 2,
[\x -> x - 1] 1,
```

### Match

Useful for reasoning about the value of an expression, and evaluating some other expression depending on the result. Currently patterns can be any integer, or the wildcard '_'.

Examples:
```
match x with
| 0 -> 1
| 1 -> 1
| _ -> x - 1
```

# Routing

Explaination coming soon :)