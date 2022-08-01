# WORK IN PROGRESS
# PINK: A Toy Programming Language

Pink is a functional stack based programming language. The programming language uses a stack to optionally store the result of each statement of the program. Each statement is made up of either a definition of a variable or an expression. Variables can either be defined globally, which can be accessed anywhere but only modified in the global scope, or locally in a function as a parameter to that function. Expressions are made up of values and operations on those values. A value can be anything from a function, to a variable, or to a plain number.

Pink is currently a work in progress and contains many bugs. Alot of work is needed.

## Examples

### Factorial
```
fact: (x:0) -> [x=0?( 1; x * (x-1) -> fact!)];
debug|(10)->fact|;
```

### Fibonacci
#### Recursive
```
fib: (x:0)->[
    x=0?(0!;0);
    x=1?(1!;0);
    (x-1)->fib,
    (x-2)->fib,
    @+@
],
debug|(10)->fib|;
```

#### Iterative
```
fib: (n:0;i:0;a:0;b:1) -> [
    n=i?(
        a!;
        (n;i+1;b;a+b)->fib!
    );
];
debug|(10)->fib|;
```

## Todo
- better error reporting
- more tests
  - unit tests
  - create integration tests
- dynamic defaults for scope definition

## Syntax

### Statements & Terminators
A statement is a [definition](#definitions--variables) or an [expression](#expressions), and a terminator. A terminator either throws the result of the statement away or pushes it onto the stack.
- `1,` pushes `1` onto the stack
- `1*2;` calculates `1*2` then throws the result away 

### Data & References
#### Numbers
A number is any floating point number. The dot point can be placed before, after or in the middle of the number. A `-` sign can be placed before to make the number negative.
- `0`
- `.0`
- `0.`
- `0.0`

#### Booleans
Booleans are just numbers. `0.0` is false and anything is true.
- `true` is `1.0`
- `false`  is `0.0`

#### Identifiers
Identifiers are used to refer to [variables](#definitions--variables) or [external calls](#external-calls). They start with a letter or underscore and can contain alphanumeric characters and underscores.

#### Indexed Value
An indexed value is an expression that indexes an element in the output [matrix](#matrix). Indexing starts at 0.
- `#(1;1)` gets the element on row one and column one

#### Stack References
- `@` pops the top of the stack
- `@@` peeks of the stack

#### Matrix
A matrix is used represent the output image of the program. It stores only numbers. All values in the matrix are converted into a scale of `0.0` to `1.0`, `1.0` being pink and `0.0` being black, when the program ends. Then the matrix is outputted as an image.
- `#` refers to the output matrix
- `width` refers to its width
- `height` refers to its height

#### Strings
Strings are an list of characters surrounded by `"`s, An escape can be used to put special characters in the string. The escapes are:
- `\n` for a new line
- `\r` for a carriage return
- `\t` for a tab
- `\\` for a `\`
- `\"` for a `"`

#### Range
A range defines a range of numbers from a starting number to an ending number. By default the step is one but can be optionally define. The starting number is inclusive, the ending number is not. They are used as parameters to [extended functions](#extended-functions).
- `{0;10}` creates a range from 0 to 9 inclusive, stepping by 1
  - giving you `0 1 2 3 4 5 6 7 8 9`
- `{0;10;2}` creates a range from 0 to 8, stepping by 2
  - giving you `0 2 4 8`
- `{10;0;-1}` creates a range from 10 to 2, stepping by -1
  - giving you `10 8 6 4 2`


### Definitions & Variables
Definitions define variables. Variables can contain expressions and functions. Variables are defined by first writing the identifier, then the set operator `:`, and then an expression or function. Definitions can also be used to change the value of a variable. No type is needed to be given but once a variable is set the type of cannot changed.
- `var: 1+1;` creates a variable called `var` that contains `2`

The set operator, `:`, can be combined with other operators to perform an operation on the variable with the given expression and then set it to the result. This can only be done with numbers. The variable must already be defined before this can be done.
- `var+: 1;` adds `1` to `var` then sets `var` to the result.

Definitions evaluate to a reference to the variable that was created or modified.

#### Scope
The scope of a variable is defined by the function it is defined in. It may only be accessed from within that function except for global variables, variables defined out side of functions, which can be accessed anywhere. Global variables can only be modified in the global scope.

### Expressions
An expression is made up of values and operators. Values and operators are chained together to create an expression. The most simple expression is just a value `1`. A more complex expression is two values being operator on `1+1`.

Values can be:
- a [number](#numbers)
- a [boolean](#booleans)
- an [indexed value](#indexed-value)
- a [stack reference](#stack-references)
- an [variable](#definitions--variables)  pointing a [function](#functions) or a number
- a [function](#functions)
- an [external call](#external-calls)

#### Operators
###### 0if Precedence
- `not` logical not operator, is a unary operator
  - `not 1` becomes `0.0`
- `!` return the result of the expression from the current function, or exit program if used in the global scope
  - `2+2!` return 4 from current function
  - when used on an exec in a function, or a reference to an exec in a function, it will call the exec using tail recursion before returning

###### 1st Precedence
- `*` multiply
- `/` divide

###### 2nd Precedence
- `+` add
- `-` subtract

###### 3rd Precedence
- `<` less than, resolves to true or false
- `>` greater than, resolves to true or false
- `<=` less than or equal, resolves to true or false
- `>=` greater than or equal, resolves to true or false
- `=` equals, resolves to true or false

###### 4th Precedence
- `and` logical and operator, resolves to true or false
- `or` logical or operator, resolves to true or false
- `xor` logical xor operator, resolves to true or false

###### 5th Precedence
- `if` conditional operator, takes an expression and a list containing two expressions surrounded by `(` and `)` and separated by `;`
  - the first element is evaluated if the condition is true and the second if false

### Functions
In pink functions are first class. They can be set to variables and evaluated inline in expressions.

#### Execs
An exec is a simple function. It has a [scope definition](#scope-definition) (a list containing expressions and definitions) and a [body definition](#body-definition), containing a list of statements that are terminated, separated by `->`. The function can access the parameters defined in the scope by name and the expressions passed by the stack in they order they were defined, last on top and first on the bottom. The parameters can be modified inside the function. All named exec's must have a defined scope. Anonymous exec's can omit the scope and the `->`, and just define a body. Its scope will then be the containing scope.
- `(x:0)->[x+1];` a exec that takes one param, `x`, and returns `x+1`
- `x:10; [x+1];` an anonymous exec that omits it's scope and returns `x+1`, `x` now being a variable in the surrounding scope.

Variables containing a function can be called by simply using as a value, `func + 1` resolves to the return of `func` plus `1`, or by using a scope definition to pass parameters. Parameters are passed in the order they are returned and you can pass any number of them.
- `add:(x:0,y:0) -> [x+y]`
- `add` returns `0`
- `()->add` returns `0`
- `(1)->add` returns `1`
- `(1,1)->add` returns `2`

Placing a [`!`](#0if-precedence) after an exec will cause it to be called using tail recursion causing the calling function to be replaced. 

##### Scope Definition
A scope definition is a list containing expressions and definitions surrounded by `(` and `)` and seperated by `;`. The definitions define the parameters. The expressions define the starting stack values.
- `(1; x: 0; 10; y:1)` the starting stack will be `1,10`, with `10` at the top, and the function will have two parameters `x`, with a default value of `0`, and `y`, with a default value of `1`

##### Body Definition
The body is a list of statements surrounded by `[` and `]`. Each statement must be terminated except the last statement which by default will have it's resulting value pushed to the stack.
- `[1; 10, 11, @ + @]` resolves to 21

#### Extended Functions
##### Into
Into is a function that calls a exec to populate the matrix. It can populate:
- a [matrix](#matrix)
It is defined by writing matrix, then `<-`, and then an [exec](#execs) or a variable containing an exec.
- `#<-[10];` will fill a the matrix with `10`s

The into passes the value already in the matrix then the index/x and y coordinate to the called exec by places them on top of its stack. For example,the first call of an into would have a stack that looks like `0 0`, `0 0` being the index and `10` being the current elements value, followed by it's default starting stack values.

Into resolves to nothing.

##### For Each
For each is a function that calls a exec for each value of a collection of elements. It can iterate over:
- a [range](#range)
- a [matrix](#matrix)
A for each is defined by the collection, then `<*`, and then the exec.
- `{0;10}<*[@+1];` will call the exec for each number from `0` to `9`

The for each passes the value in the collection then the index/x and y coordinate to the called exec by places them on top of its stack. For example,the first call of the into on the range `{0;10}` would have a stack that looks like `0 10`, `0` being the index and `10` being the current elements value on the top of the stack, followed by it's default starting stack values.

For each resolves nothing, or the last return if it was iterating a range.

##### Reduce
Reduce is a function that calls a exec for each value of a collection of elements and adds the value of the return of each call together iteratively. It can iterate over:
- a [range](#range)
- a [matrix](#matrix)
A reduce is defined by the collection, then `<*`, and then the exec.
- `{0;10}*>[@+@];` will add the values from `0` to `9` together

The reduce passes the value in the collection, then the accumulation, and then the index/x and y coordinate to the called exec by places them on top of its stack. For example,the first call of reduce on the range `{0;10}` would have a stack that looks like `0 0 10`, `0` being the index, `0` being the starting accumulation, and `10` being the current elements value on the top of the stack, followed by it's default starting stack values.

Reduce resolves to the final value of the accumulator.

### External Calls
External calls run functions that exist outside of the program. They are called by writing their identifier then an argument list surrounded by `|` and `|`, and separated by `;`. 
- `sin|10|;` will call the external function sin with the argument 10

Some external calls can take an optional [string](#strings) as a parameter. The string is passed as the first argument with no separator followed by the rest of the arguments.
- `debug|"should be 10" @|` passes the string `"should be 10"` and the value `@` to the call `debug`

All external calls will resolve to some value.

#### Calls

##### Math
- `sin`
  - one param
- `cos`
  - one param
- `tan`
  - one param
- `floor`
  - one param
- `ceil`
  - one param
- `sqrt`
  - one param

##### Signed Distance Field Calls (SDF)
These external calls are used to calculate SDFs. 
- `circle` calculates SDF of a circle
  - 3 params
    - the current pixel x coord
    - the current pixel y coord
    - the radius
- `rect` calculates SDF of a rectangle
  - 4 params
    - the current pixel x coord
    - the current pixel y coord
    - the width
    - the height
- `translate` translates a coordinate
  - 2 params
    - the current pixel x/y coord
    - the offset
- `scale` scales a coordinate
  - 2 params
    - the current pixel x/y coord
    - the scale
- `rotateX` rotate the x component of a coordinate
  - 3 params
    - the current pixel x coord
    - the current pixel y coord
    - the rotation (0.0 no rotation, 1.0 full rotation)
- `rotateY` rotate the Y component of a coordinate
  - 3 params
    - the current pixel x coord
    - the current pixel y coord
    - the rotation (0.0 no rotation, 1.0 full rotation)
- `sdf` takes the output of a SDF and returns the color output
  - one param

##### Console
- `debug`
  - one param and optional string
  - prints value to the console
## License

This software is provided under the MIT license. Click [here](./LICENSE) to view.