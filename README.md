GOG
===
A language with polynotational semantics
-----

# Installation

1. Install Rlwrap and Racket
2. Install optional MySQL support
3. Clone GOG
4. Add GOG launcher to your path

Assuming you have Ubuntu, this can be done with the following lines

    sudo apt-get install racket rlwrap
    sudo apt-get install mysql-client libssl-dev # Optionally for MySQL support
    sudo git clone https://github.com/nosovicki/gog.git /opt/gog
    sudo ln -s /opt/gog/gog /usr/bin/

Now you can run GOG interactively and / or pass programs to it:

    gog
    gog examples/life.gog


## Introduction

Any language is defined by its words and by how we combine those words.
Similarly to a rich vocabulary that has many words for one meaning, rich syntax
conveys semantics by many ways. Perfectly robust language has enough syntactic
cases to make an impression that it has little or no rules; convenience to
express what we want the way we find sutable is what makes natural languages
both powerful and legible, both maintainable and convenient.

While programming languages often excel at mind bending ways to combine their
primitives, examples of elegant simplicity are rare. That happens because most
languages supply one complex strict syntax that whould fits most cases, while a
syntax that suits all cases does not exist.

GOG is a step in another direction. It allows virtually any syntax. Though GOG
is rather complex internally, its complexity is of a kind that makes it simple
to use.

The only two GOG rules are:
1. Words are separated by any number of whitespace characters.
2. Expressions are either separated by line breaks or included in parentheses.

That's right -- no strict order of tokens, and no fancy brackets and arrows.
That's because GOG uses polynotational semantics. The idea of polynotational
semantics is based on an observation that the number of simple convenient
notations is limited:

* Prefix notation: `func(arg1, arg2, arg3)`
* Infix notation: `x = 2 + 2 * 3`
* Postfix notation: `ls | rev | tac | split`

Each notation has its strengths, and GOG allows all of them. Importantly, GOG
understands what you mean without additional syntactic clues. For example:

    λ + 1 2 3
    6
    λ 1 + 2 + 3
    6
    λ 9 sqrt prn prn
    3
    3
    3

This reduces syntax to just alphanumeric keys plus `()`, and you can write your
programs with just these. However, on top of this GOG builds features that do
utilize your spared keyboard range: GOG has rich lexical rules. Similarly to
natural languages, GOG modifies word meaning on the fly in accordance to a
lexical rule. For example, you transform `+` into `(lambda (x) (map + x))`
by writing `@+`.

### Fun with lexemes

Lexemes modify meaning of a function. Similar semantic units are called adverbs
in tacit programming, Here I show how lexemes influence function behavior.
Note that in the last example `+` function works as an identity function.

    λ x = '((1 2 3) (4 5 6) (7 8 9))
    ((1 2 3) (4 5 6) (7 8 9))
    
    λ $+ x
    (1 2 3 4 5 6 7 8 9)  ;; Flatten
    
    λ @$+ x
    (6 15 24) ;; Sum rows
    
    λ $@+ x
    (12 15 18) ;; Sum columns
    
    λ $@&+ x
    ((1 4 7) (2 5 8) (3 6 9)) ;; Transpose

@ = map; $ = apply; & = collect

Note that lexemes are higher-order functions that became part of the language.
Because of that you can use lexemes with any expression that evaluates to a
function, including lambdas and subexpressions that return functions, for
example: `&[X + 2]`, `$(select-function function-list)`.

### Simplified polyadic lambda syntax

Instead of

    (lambda (a b) (a + b))

you can write

     λ [A + B]

To distinguish lambda arguments from other names you start them with capital
letters. Of course you can use any notation inside lambdas too, as well as
nest lambda functions infinitely:

#### Nested example

    λ [map [*Elem / Num] List]

GOG understands as:

    (lambda (List Num) (map (lambda (Elem) (Elem / Num)) List))

Asterisk at the start of Elem informs that it is a parameter of the inner
lambda. The deeper in the nesting resides your argument, the more asterisks you
add. Note that List goes before Num in lambda argument list. It is because
auto-guessed lambda parameters are always assumed to follow alphabetic order.

### Example: sum square difference

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of their sum, i. e.
(1 + 2 + ... 100)^2 - (1^2 + 2^2 + ... 100^2)

*Prefix notation:*

    λ (= x (range 1 100))
    λ (- (** ($+ x) 2) ($+ (@**_2 x)))

*Infix notation:*

    λ x = 1 range 100
    λ ($+ x) ** 2 - ($+ (@**_2 x))

*Postfix notation:*

    λ 1 range_100 !x @**_2 $+ [x $+ **_2 -_X]

**Explanation of the last line**: Postfix notation passes single value from one
function to another, so every token must evaluate to a function of one
argument. Let's trace this expression: `1` is  the initial value. `range_100`
is a modified function `range`, to which by syntax `_` was attached argument
100. `range` takes 2 arguments, `from` and `to`, and returns a list of
increasing values from `from` to `to` . `!` is a lexeme that takes any name
and makes a function that saves its argument to a variable with that name. So
`!x` assigns obtained list to variable `x`, which we will use later. `@**_2`
looks complex, but it is just a modified exponentiation function `**`. `**_2`
means "square", while `@` is the "mapping" lexeme. It makes `**_2` map on our
range list, squaring all its values. `$` is a "reduce" lexeme. `$+` reduces
list by summation. Finally, the last subexpression is a lambda. `[` and `]`
denote a lambda function. GOG automatically guesses which variables inside a
lambda are parameters by a very simple rule: parameters start with a capital
letter.

Let's take a closer look at the last function. It starts with `[` and ends with
`]`. Inside, there are 3 functions: `$+` (reduce), `**_2` (square) and `-_X` (a
function that subtracts variable X from its parameter. X is an argument of the
lambda function, because it starts with a capital letter. `x` is the variable
which was saved earlier. This lambda reduces `x` with summation, squares the
result, and subtracts its argument from it.

# Why GOG?

As you can see, GOG expressions can be packed with meaning. Using both tacit
features and compact lambda notation allows to write concise and powerful
programs that are easy to understand. What is more important, GOG allows
experimenting with different ways to write a program. With GOG, you can make
your program shorter or longer, crystal-clear or opaque. In short, I'm sure
that, though imperfect as it is, GOG may be very useful for further
people-centric experiments with programming languages.

# Relation to ARC and LISP:

In theory, Lisp's prefix notation is just an arbitrary, artificial constraint
of otherwise absolutely minimalistic language, and GOG proves this constraint
to be unnecessary.

GOG is based on [ARC] (https://en.wikipedia.org/wiki/Arc_%28programming_language%29),
which is a very advanced functional language by itself. As a result, GOG has
all data types and features of Arc: macros, symbols, etc.

Basically, GOG extends Arc with polynotational semantics, adds multivariable
nestable lamdas, and introduces an algebra for list operations. The initial idea
was to add polynotational semantics to arc as a patch. It did work, but broke
some minor arc semantics. Other features are incompatible.

# Backward compatibility

Gog is mostly backward compatible with the arc, in sense that traditional arc
expressions are recognized as prefix notation, except the following cases:

* You can not use a procedure or macro object as a data-call argument. It would classify as some unknown notation.
* Same refers to specifying more than one argument during data call (arc tables support second optional argument).

# Execution speed

I did no optimization whatsoever, as I wanted to keep this highly experimental
code as straightforward as possible. That said, the overhead is generally
insignificant. First attempt to determine notation is made at compile time.
That effectively minimizes run-time overhead, especially for prefix notation.
Thus, during arc loading, 10839 of 10318 examined expressions (~95%) recognise
as prefix notation at compile time. The rest requires run-time notation
discovery, which should result in slight overhead for traditional expressions,
and yet somewhat more substantial one for infix and postfix notations.
Nevertheless, I was not able to notice any overhead using simple benchmarks.
This topic might require further investigation, but, as far as I can see, the
execution overhead is low.

# Infix precedence rules

It would be a shame to introduce infix notation to a Lisp without precedence
rules. GOG uses same precedence as C language does. New functions get
precedence by association. Order rules may be added or changed dynamically for
any primitive at any time. See ac.scm comments for details, including how to set
precedence order for your function.
