# AstTraverse

## Introduction

A implementation of treat erlang abstract format with traversable like api

    traverse :: (a -> f b) -> t a -> f (t b)
   
    map_m :: (a -> m b) -> t a -> m (t b)
    
here is my mutation map_m 
    
    map_m :: (pre | post -> a -> m b) -> t a -> m (t b)
  
where 

    a :: node
    b :: node
    t a :: ast
    t b :: ast 

## Without monad

    mapfold :: (pre | post, -> node -> state -> (node, state)) -> init_state -> ast -> (ast, state)

    map_with_state :: (pre | post, -> node -> state -> (node, state)) -> init_state -> ast -> ast

    map :: (pre | post -> node -> node) -> ast -> ast

    reduce :: (pre | post -> node -> state -> state) -> ast -> init_state -> state
    
## explanation

  **pre** means this node is traversed before it's children 
  
  **post** means this node is traversed after it's children
  
  every node is travered both in pre and post type
  
  so it's a mutation travserse

## traverse order

[form1, form2, [([pattern1, pattern2], [[guard1]], [expression1, expression2]) = function_clause1] = form3] is traversed in such order

    (leaf, form1)
    
    (leaf, form2)
    
    (pre,  form3)
        
    (pre,  function_clause1)
        
    (leaf, pattern1)
        
    (leaf, pattern2)
        
    (leaf, guard_test1)
        
    (leaf, expression1)
    
    (leaf, expression2)
     
    (post, fun_cluase,  funnction_clause1)
        
    (post, form, form3)
