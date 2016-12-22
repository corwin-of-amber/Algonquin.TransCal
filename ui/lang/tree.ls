
{zip} = require 'prelude-ls'


eq = (x,y) -> if x.equals? then x.equals(y) else x==y

class Tree
  (@root, @subtrees=[]) ->

  toString: ->
    if @subtrees.length == 0
      "#{@root}"
    else
      "#{@root}(#{@subtrees.join(', ')})"

  of: -> 
    new Tree(@root, @subtrees ++ &[to])
  
  is-leaf: -> @subtrees.length == 0
  
  equals: (other) ->
    eq(@root, other.root) &&
      @subtrees.length == other.subtrees.length &&
      zip(@subtrees, other.subtrees).every -> it.0.equals(it.1)

  nodes:~
    -> [@].concat ...@subtrees.map (.nodes)

  left-branch:~
    -> []
      c = @ ; while c? then ..push c ; c = c.subtrees[0]
    
  leftmost:~
    -> 
      c = @
      while !c.is-leaf! then c = c.subtrees[0]
      c
    
  ## applies op to the root of each subtree
  map: (op) ->
    new Tree(op(@root), [s.map op for s in @subtrees])

  clone: -> @map -> it
    
  filter: (pred) ->
    if pred @root
      new Tree @root, ([s.filter pred for s in @subtrees].filter (x) -> x?)
    else null

  find: (value) ->
    @findT (n) -> n.root == value

  findT: (pred) ->
    if pred @ then @
    else
      for s in @subtrees
        if (n = s.findT(pred))? then return n

  find-path: (value) ->
    @findT-path (n) -> n.root == value

  findT-path: (pred) ->
    if pred @ then [@]
    else
      for s in @subtrees
        if (n = s.findT-path(pred))? then return [@] ++ n
        
  to-digraph: ->
    new Graph
      for n in @nodes
        ..nodes.push n.root
        ..edges.push ...[[n.root, s.root] for s in n.subtrees]
    
        
T = -> new Tree ...


@ <<< {Tree, T}
