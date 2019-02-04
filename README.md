# Transcal

## About
Hi,

If you are reading this, you are in luck (that this file exists).

We are going to write a general description of how the system works. 
First we will describe the transcal language and than how the rewrite system works.
The system was rewritten to include the minimum possible assumptions but there are
still some so it is recommended to read to the end.

## The Language
The language is fully functional but not functional at all (pun intended).

We support lambda's clojure's and passing functions in general. 

### Builtins
There are many builtin operators like /\\ (and) and \\/ (or) which all 
translate into an operator in the parsed tree as root with two children.  

There are other builtins that behave differently:
1. ↦ (lambda) - has a two children. First child is the arguments and second
is the body. Arguments can be one pattern or a number of patterns in a tuple.
Arguments should be translated to holes 
where the same leafs should be the same holes on the body.
2. -> (command) - part of the rewriting system, used for locate and command style.
Has two subterms, can only be at top level.
3. expression =/>> expression - Let means defining a new rewrite rule while def also adds 
the body (right hand side expression) to the hyper graph. Always a statement with
two expressions.

### Syntax Sugars
There are some possible notations to use which are translated a 
little differently than how they appear:
1. f = ?x ?y ↦ x + y ==> f ?x ?y = x + y
2 (clojure). f ?x = ?y ↦ x + y ==> f ?x = (?x1 ?y ↦ x1 + y)(x) 
3. let f = nil ↦ 0 / x::xs ↦ x + f xs ==> 
    - let f nil = nil ↦ 0
    - let f x::xs = x::xs ↦ x + f xs
    
### Parser

### Let definitions

## The Rewrite system

### Backend graph

### Rewrite search vs Action search
