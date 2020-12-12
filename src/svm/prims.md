
| Type Name    | Description                      |
| ------------ | -------------------------------- |
| *alist*      | Association list (list of pairs) |
| *boolean*    | `#t` or `#f`                     |
| *byte*       | Integer 0 ≤ *byte* ≤ 255         |
| *bytevector* | Bytevector                       |
| *char*       | Character                        |
| *end*        | Exact non-negative integer       |
| *k*          | Exact non-negative integer       |
| *letter*     | Alphanumeric character           |
| *list*       | List                             |
| *n*          | Integer                          |
| *obj*        | Any object                       |
| *pair*       | Pair                             |
| *port*       | Port                             |
| *proc*       | Procedure                        |
| *q*          | Rational number                  |
| *start*      | Exact non-negative integer       |
| *string*     | String                           |
| *symbol*     | Symbol                           |
| *thunk*      | Zero-argument procedure          |
| *vector*     | Vector                           |
| *x*          | Real number                      |
| *y*          | Real number                      |
| *z*          | Complex number                   |

# Equivalence Procedures

- `(eqv? obj₁ obj₂)`
    - Returns `#t` if `obj₁` and `obj₂` are regarded as the same object.
- `(eq? obj₁ obj₂)`
    - Returns `#t` if `obj₁` and `obj₂` are the exact same object, ie. pointer equality.
      This returns `#t` in strictly less cases than `eqv?`.
- `(equal? obj₁ obj₂)`
    - Returns `#t` if `obj₁` and `obj₂` have the same structure.
      In general `(equal? obj₁ obj₂)` implies `obj₁` and `obj₂` print the same.

# Numbers

- `(number? obj)`
    - Returns `#t` if `obj` is a number, `#f` otherwise.
- `(complex? obj)`
    - Returns `#t` if `obj` is a complex number, `#f` otherwise.
      The SVM implements this as `number?` since it doesn't support complex numbers.
- `(real? obj)`
    - Returns `#t` if `obj` is a real number, `#f` otherwise.
      The SVM implements this as `number?`.
- `(rational? obj)`
    - Returns `#t` if `obj` is a rational number, `#f` otherwise.
      The SVM implementation returns `#t` for finite numbers since it doesn't support ratios.
- `(integer? obj)`
    - Returns `#t` if `obj` is an integer, `#f` otherwise.
      The SVM implements this by checking if the number is equal to its truncation.

- `(exact? z)`
    - Returns `#t` if `z` is an exact number, `#f` otherwise.
      The SVM implements this by checking if the number is an integer that fits
      in the range for doubles to perfectly represent the integer.
- `(inexact? z)`
    - Returns `#t` if `z` is an inexact number, `#f` otherwise.

- `(exact-integer? z)`
    - Returns `#t` if `z` is an integer and exact, `#f` otherwise.
      The SVM implements this as `integer?`.
- `(finite? z)`
    - Returns `#t` if `z` is not `+inf.0`, `-inf.0`, or `+nan.0`, `#f` otherwise.
- `(infinite? z)`
    - Returns `#t` if `z` is `+inf.0` or `-inf.0`, `#f` otherwise.
- `(nan? z)`
    - Returns `#t` if `z` is `+nan.0`, `#f` otherwise.

- `(= z₁ z₂ z₃ ...)`
    - Returns `#t` if all arguments are equal, `#f` otherwise.
- `(< z₁ z₂ z₃ ...)`
    - Returns `#t` if arguments are monotonically increasing, `#f` otherwise.
- `(> z₁ z₂ z₃ ...)`
    - Returns `#t` if arguments are monotonically decreasing, `#f` otherwise.
- `(<= z₁ z₂ z₃ ...)`
    - Returns `#t` if arguments are monotonically non-decreasing, `#f` otherwise.
- `(>= z₁ z₂ z₃ ...)`
    - Returns `#t` if arguments are monotonically non-increasing, `#f` otherwise.

- `(zero? z)`
    - Returns `#t` if the number is 0, `#f` otherwise.
- `(positive? z)`
    - Returns `#t` if the number is positive, `#f` otherwise.
- `(negative? z)`
    - Returns `#t` if the number is negative, `#f` otherwise.
- `(odd? z)`
    - Returns `#t` if the number is odd, `#f` otherwise.
- `(even? z)`
    - Returns `#t` if the number is even, `#f` otherwise.

- `(max x₁ x₂ ...)`
    - Returns the maximum argument.
- `(min x₁ x₂ ...)`
    - Returns the minimum argument.

- `(+ z₁ ...)`
    - Returns the sum of its arguments.
- `(* z₁ ...)`
    - Returns the product of its arguments.
- `(- z)`
    - Returns the negation of `z`.
- `(- z₁ z₂ z₃ ...)`
    - Returns the difference of its arguments, associating left.
- `(/ z)`
    - Returns the reciprocal of `z`.
- `(/ z₁ z₂ z₃ ...)`
    - Returns the quotient of its arguments. associating left.

- `(abs x)`
    - Returns the absolute value of `x`.

- `(floor/ n₁ n₂)`
    - Returns `nq` and `nr`, where `nq = ⌊n₁/n₂⌋` and `n1 = n2 * nq + nr`
- `(floor-quotient n₁ n₂)`
    - Returns `nq` from `floor/`.
- `(floor-remainder n₁ n₂)`
    - Returns `nr` from `floor/`.
- `(truncate/ n₁ n₂)`
    - Returns `nq` and `nr`, where `nq = truncate(n₁/n₂)` and `n1 = n2 * nq + nr`
- `(truncate-quotient n₁ n₂)`
    - Returns `nq` from `truncate/`.
- `(truncate-remainder n₁ n₂)`
    - Returns `nr` from `truncate/`.

- `(quotient n₁ n₂)`
    - Equivalent to `truncate-quotient`.
- `(remainder n₁ n₂)`
    - Equivalent to `truncate-remainder`.
- `(modulo n₁ n₂)`
    - Equivalent to `floor-remainder`.

- `(gcd n₁ ...)`
    - Returns the greatest common divisor of its arguments.
- `(lcm n₁ ...)`
    - Returns the least common multiple of its arguments.

- `(numerator q)`
    - Returns numerator of the ratio.
- `(denominator q)`
    - Returns denominator of the ratio. `(denominator 0)` is `1`.

- `(floor x)`
    - Returns the largest integer not larger than `x`.
- `(ceiling x)`
    - Returns the smallest integer not smaller than `x`.
- `(truncate x)`
    - Returns the integer closest to `x` whose absolute value is not larger than the absolute value of `x`.
- `(round x)`
    - Returns the integer closest to `x`, rounding to even when `x` is halfway between two integers.

- `(rationalize x y)`
    - Returns the simplest rational number differing from `x` by no more than `y`.

- `(exp z)`
    - Returns `e^z`.
- `(log z)`
    - Returns the natural log of `z`.
- `(log z₁ z₂)`
    - Returns the base-`z₂` log of `z₁`.
- `(sin z)`
    - Returns the sine of `z`.
- `(cos z)`
    - Returns the cosine of `z`.
- `(tan z)`
    - Returns the tangent of `z`.
- `(asin z)`
    - Returns the arcsine of `z`.
- `(acos z)`
    - Returns the arccosine of `z`.
- `(atan z)`
    - Returns the arctangent of `z`.
- `(atan y x)`
    - Computes the polar angle of the complex number `x`+`y`i.
      Ranges from -π to +π.

- `(square z)`
    - Returns `(* z z)`.
- `(sqrt z)`
    - Returns the principal square root of `z`.
- `(exact-integer-sqrt k)`
    - Returns non-negative exact integers `s` and `r`, where `k = s^2 + r` and `k < (s + 1)^2`.
- `(expt z₁ z₂)`
    - Returns `z₁` raised to the power `z₂`.
      `(expt 0 0)` is `1`.

- `(inexact z)`
    - Returns an inexact representation of `z`.
- `(exact z)`
    - Returns an exact representation of `z`.

# Numerical I/O

- `(number->string z)`
    - Equivalent to `(number->string z 10)`
- `(number->string z radix)`
    - Returns a string representation of `z` in the given radix.
      `radix` must be 2, 8, 10, or 16.

- `(string->number string)`
    - Equivalent to `(string->number string 10)`.
- `(string->number string radix)`
    - Attempts to parse `string` as a number in base `radix`.
      `radix` is overridden by a base prefix such as `#x` or `#o`.
      Returns `#f` on failure.

# Booleans

- `(not obj)`
    - Returns `#t` if `obj` is `#f`, `#f` otherwise.
- `(boolean? obj)`
    - Returns `#t` if `obj` is `#t` or `#f`, `#f` otherwise.
- `(boolean=? boolean₁ boolean₂ boolean₃ ...)`
    - Returns `#t` if all the arguments are `#t` or all the arguments are `#f`, or `#f` otherwise.

# Pairs and Lists

- `(pair? obj)`
    - Returns `#t` if `obj` is a pair, `#f` otherwise.
- `(cons obj₁ obj₂)`
    - Returns a newly allocated pair whose car is `obj₁` and whose cdr is `obj₂`.
- `(car pair)`
    - Returns the car of the pair.
- `(cdr pair)`
    - Returns the cdr of the pair.
- `(set-car! pair obj)`
    - Stores `obj` in the car field of `pair`.
- `(set-cdr! pair obj)`
    - Stores `obj` in the cdr field of `pair`.

- `(caaar pair)`, `(caadr pair)`, ..., `(cdddar pair)`, `(cdddr pair)`
    - Compositions of `car` and `cdr`.

- `(null? obj)`
    - Returns `#t` if `obj` is the empty list, `#f` otherwise.
- `(list? obj)`
    - Returns `#t` if `obj` is a list.
- `(make-list k)`
    - Returns a newly allocated list of `k` elements.
- `(make-list k fill)`
    - Returns a newly allocated list of `k` elements with elements initialized to `fill`.
- `(list obj ...)`
    - Returns a newly allocated list of its arguments.
- `(length list)`
    - Returns the length of the list.
- `(append list ...)`
    - Returns a list containing the elements of the first list followed by the
      elements of the second list, and so on.
      The last argument can be of any type, and if it is not a proper list, the
      result of `append` is not a proper list.
- `(reverse list)`
    - Returns a newly allocated list consisting of the elements of `list` in reverse order.
- `(list-tail list k)`
    - Returns the sublist of `list` obtained by omitting the first `k` elements.
      It is an error if `list` has fewer than `k` elements.
- `(list-ref list k)`
    - Returns the `k`th element of `list`.
      It is an error if `list` has fewer than `k` elements.
- `(list-set! list k obj)`
    - Stores `obj` in element `k` of `list`.

- `(memq obj list)`
    - Equivalent to `(member obj list eq?)`
- `(memv obj list)`
    - Equivalent to `(member obj list eqv?)`
- `(member obj list)`
    - Equivalent to `(member obj list equal?)`
- `(member obj list compare)`
    - Returns the first sublist of `list` whose car is `obj`, or `#f` if none is found.
      Uses `compare` to determine equality.

- `(assq obj alist)`
    - Equivalent to `(assoc obj alist eq?)`
- `(assv obj alist)`
    - Equivalent to `(assoc obj alist eqv?)`
- `(assoc obj alist)`
    - Equivalent to `(assoc obj alist equal?)`
- `(assoc obj alist compare)`
    - Find the first pair in `alist` whose car field is `obj`, returning that pair.
      Returns `#f` if none is found.
      Uses `compare` to determine equality.

- `(list-copy obj)`
    - Returns a newly allocated copy of `obj` if it is a list.
      Only the pairs are copied, the cars are the same.
      If `obj` is an improper list, only the proper part is copied.

# Symbols

- `(symbol? obj)`
    - Returns `#t` if `obj` is a symbol, `#f` otherwise.
- `(symbol=? symbol₁ symbol₂ symbol₃ ...)`
    - Returns `#t` if all the arguments have the same name.
- `(symbol->string symbol)`
    - Returns the name of `symbol` as an immutable string.
- `(string->symbol string)`
    - Returns the symbol whose name is `string`.

# Characters

- `(char? obj)`
    - Returns `#t` if `obj` is a character, `#f` otherwise.
- `(char=? char₁ char₂ char₃ ...)`
    - Returns `#t` if the arguments' integer representations are equal.
- `(char<? char₁ char₂ char₃ ...)`
    - Returns `#t` if the arguments' integer representations are monotonically increasing.
- `(char>? char₁ char₂ char₃ ...)`
    - Returns `#t` if the arguments' integer representations are monotonically decreasing.
- `(char<=? char₁ char₂ char₃ ...)`
    - Returns `#t` if the arguments' integer representations are monotonically non-decreasing.
- `(char>=? char₁ char₂ char₃ ...)`
    - Returns `#t` if the arguments' integer representations are monotonically non-increasing.
- `(char->integer char)`
    - Returns an exact integer between 0 and #xD7FF or between #xE000 and #x10FFFF
      which is equal to the Unicode scalar value of `char`.
- `(integer->char n)`
    - Inverse of `char->integer`.

# Strings


