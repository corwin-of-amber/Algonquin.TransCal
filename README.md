# Transcal

## About
Hi,

If you are reading this, you are in luck (that this file exists).

We are going to write a general description of how the system works. 
First we will describe the transcal language and than how the rewrite system works.
The system was rewritten to include the minimum possible assumptions but there are
still some so it is recommended to read to the end.

## General Overview
Import the sbt.build
The system is built as a user guided rewrite system.
This works by using [rewrite rules](src/main/scala/synthesis/search/rewrites/operators/RewriteRule.scala) stored in [dataset structures](src/main/scala/synthesis/RewriteRulesDB.scala) 
and a [data structure](src/main/scala/structures/mutable/CompactHyperGraph.scala) for efficient congruence closure.
The user guided search is enabled by [Actions](src/main/scala/synthesis/actions), which in turn are usually implemented
by using other actions or by running a [rewrite search](src/main/scala/synthesis/search/action/operators/OperatorRunAction.scala)
which is an implementation of congruence closure over our data structure.   
In addition, some special actions are added which are the core of the research done by our team.
Currently focusing on TheSy, which is a theory exploration system in an action named SPBE.

## The Language
We are working on passing AST's and types from COQ. 
Need to update this if the feature works.
    
### Parser

### Let definitions

## The Rewrite system

### Backend graph

### Rewrite search vs Action search

## Performance

### Run a Profiler 
