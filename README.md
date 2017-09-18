# AstTraverse

## Introduction

A implementation of treat erlang abstract format with traversable like api

    traverse :: (a -> f b) -> t a -> f (t b)
   
    map_m :: (a -> m b) -> t a -> m (t b)
    
here is my mutation map_m 
    
    map_m :: (pre | post -> node_type -> a -> m b) -> t a -> m (t b)
  
where 

    a :: node
    b :: node
    t a :: ast
    t b :: ast 

## for one do not famillar with monad or do not want use monad here

    ast_traverse:map_reduce :: (pre | post, -> node_type -> node -> state -> (node, state)) -> init_state -> ast -> (ast, state)

    ast_traverse:map_with_state :: (pre | post, -> node_type -> node -> state -> (node, state)) -> init_state -> ast -> ast

    ast_traverse:map :: (pre | post -> node_type, node -> node) -> ast -> ast

    ast_traverse:reduce :: (pre | post -> node_type -> node -> state -> state) -> ast -> init_state -> state
    
## explanation

  **pre** means this node is traversed before it's children 
  
  **post** means this node is traversed after it's children
  
  every node is travered both in pre and post type
  
  so it's a mutation travserse

  **node_type** presents which type the node is 
  
  such as *forms*, *form*, *pattern*, *expression*
  
  for detials, see

[ast traverse lens](https://github.com/slepher/ast_traverse/blob/master/src/ast_traverse_lens.erl)

## list traverse

[form1, form2, form3] is traversed in such order

    (pre,  forms, [form1, form2, form3])
    
    (pre,  form,  form1)
    
    (post, form,  form1)
    
    (pre,  forms, [form2, form3])
    
    (pre,  form,  form2)
    
    (post, form,  form2)
    
    (pre,  forms, [form3])
    
    (pre,  form,  form3)
    
    (post, form,  form3)
    
    (post, forms, [form3])
    
    (post, forms, [form2, form3])
    
    (post, forms, [form1, form2, form3])
