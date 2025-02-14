# ScopeLand

A functional programming language, where the programs data structure is exposed to the programmer.

# Language Constructs
## Statements

| statement | explaination |
| --- | --- | 
| Anon | An unnamed expression. |
| Named | A named expression. |
| Output | Prints an expression to console. |
| Import | Imports another source file, using the scopeland route syntax, excluding indexing. |


In the following example is four statements. First is an anonymous statement, a named statement, an output statement, and finally an import statement.

```
...
9001,
my_name: 1337,
!999,
import ^.std,
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
| _expr_ | Index into a scope. The first element is at 0, and lookup with negative values, starts for the last defined element, i.e. index -1 referes to the last statement. When starting a route with an expression, it must be postfix with ':'. |
| ^ | Goes to the containing scope, or crashes if there is no containing scope. | 
| @ | Goes to the outermost scope, in the defining file |

Here are a few syntax examples, we go in further details in [Routing](#routing).
```
my_name
^.other_name
^.^.-1.0
1:.var_name
```

### Binary Operation

Used to compute over two elements, currently support: <br>
+, -, *, =, !=, <, >, <=, >=

### Scope

A collection of statements, by way of a comma-separated array. A statement can refer directly to other statements in the scope, which are defined prior to itself. The brakets of the outermost scope of a file, are optional.

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

A scope with a positive number of undefined, but named, statements. These statements are defined at function application. Applications can be partial, and functions return the value of their last statement.

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
Writing ```f x y```, first applies _x_ to _f_, and the _y_ onto the result.
Another option is using the ```|>``` operator, which applies the left operand to the right operand. 

Examples:
```
fib 10,
10 |> fib,
^.funcs.double 2,
[\x -> x - 1] 1,
```



### Match

Useful for reasoning about the value, or shape, of an expression, and evaluating some other expression depending on the result. 

Patterns:
| Pattern | Example | Explaination |
| --- | --- | -- |
| _ | ```_``` | Matches anything |
| _name_ | ```x``` | Matches anything, and binds it to the name |
| _int_ | ```1``` | Matches a specific integer |
| int | ```int``` | Matches any integer |
| string | ```string``` | Matches any string |
| [] | ```[]``` | Matches the empty scope |
| _p1_ & _p2_ | ```t&h``` | Matches the scope where the last element matches _p2_ and the rest matches _p1_ |
| [_p1_, ..., _pn_] | ```[x, 2]``` | Matches a scope of _n_ elemements, where the _i_'th element matches the _i_'th pattern.

Examples:
```
[\x -> match x with
    | 0 -> 1
    | 1 -> 1
    | _ -> x - 1
],
rev: [\l acc -> match l with
    | [] -> acc
    | t&h -> ^.rev t (acc&h)
],
is_int: [\v -> match v with
    | int -> 1
    | _ -> 0
]
```