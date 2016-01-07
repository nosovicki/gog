GOG
===
Experimental language with polynotational semantics
-----
!!! This document is unfinished
# Introduction

What is a language, if not words plus rules of combination?
While programming languages excel at mind-bending ways to combine their primitives,
examples of elegant simplicity are rare. What if we wanted to create an absolutely minimalistic
language with no syntax rules at all? Wouldn't it be great if a programmer could write
expressions any way he wants? Well, GOG is one step in that direction.

The idea of polynotational semantics is that languages have three main types of notation:

* Prefix notation: `func(arg1, arg2, arg3)`
* Infix notation: `x = 2 + 2 * 3`
* Postfix notation: `ls | grep x | tee y`

GOG understands what you mean without any syntax clues. For example:
    > + 1 2 3
    6
    > 1 + 2 + 3
    6
    > 9 sqrt prn prn
    3
    3

### What you can do

GOG features are:
* Polynotational semantics
* Simplified lambda function syntax
* Prefix functional modifiers

### Example: sum square difference

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum, i. e. (1^2 + 2^2 + ... 100^2) - (1 + 2 + ... 100)^2

    1 range_100 !x @**_2 $+ [x $+ **_2 -_X]

# Installation

Assuming you have ubuntu:
    # sudo apt-get install racket rlwrap

After that, you can run gog:
    ./gog

# Backward compatibility

Gog is mostly backward compatible with the arc, in the sense that traditional arc expressions are recognized as prefix notation, except the following cases:

* You can not use a procedure or macro object as a data-call argument. It would classify as some arbitrary notation.
* Same refers to specifying more than one argument during data call (arc tables support second optional argument).

# Switching polynotation features off

Polynotational semantics is on by default. To switch it off completely, you can use "uniform-notation" declaration:

For example:

    (a = (table))
    (declare 'uniform-notation t)
    (= (a map) 'foo)
    (prn (a map))
    (declare 'uniform-notation nil)
    ((2 + 2) prn)
    =>
    foo
    4

# Executioin speed

I did no optimization whatsoever, as I wanted to keep this highly experimental code as straightforward as possible. That said, the overhead is generally insignificant. First attempt to determine notation is made at compile time. That effectively minimizes run-time overhead, especially for prefix notation. Thus, during arc loading, 10839 of 10318 examined expressions (~95%) recognise as prefix notation at compile time. The rest requires run-time notation discovery, which should result in slight overhead for traditional expressions, and yet somewhat more substantial for infix and postfix ones. Nevertheless, I was not able to notice any of it, using simple benchmarks. This topic might require further investigation, but as far as I can see, the execution overhead seems to be low.

# Infix precedence rules

It would be a shame to introduce infix notation to a Lisp without precedence rules. GOG uses same precedence as C language does. New functions get precedence by association: you
