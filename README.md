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

## for one do not famillar with monad or do not want use monad

    map_reduce :: (pre | post, -> node_type -> node -> state -> (node, state)) -> init_state -> ast -> (ast, state)

    map_with_state :: (pre | post, -> node_type -> node -> state -> (node, state)) -> init_state -> ast -> ast

    map :: (pre | post -> node_type, node -> node) -> ast -> ast

    reduce :: (pre | post -> node_type -> node -> state -> state) -> ast -> init_state -> state
    
## explanation

  **pre** means this node is traversed before it's children 
  
  **post** means this node is traversed after it's children
  
  every node is travered both in pre and post type
  
  so it's a mutation travserse

  **node_type** presents which type the node is 
  
  such as *forms*, *form*, *pattern*, *expression*
  
  for detials, see

[ast lens](https://github.com/slepher/ast_traverse/blob/master/src/ast_lens.erl)

## traverse order

[form1, form2, [([pattern1, pattern2], [[guard1]], [expression1, expression2]) = function_clause1] = form3] is traversed in such order

    (pre,  forms, [form1, form2, form3])
    
    (pre,  form,  form1)
    
    (post, form,  form1)
    
    (pre,  form,  form2)
    
    (post, form,  form2)
    
    (pre,  form,  form3)
    
    (pre,  fun_clauses, [function_clause1])
    
    (pre,  fun_clause, function_clause1)
    
    (pre,  patterns, [pattern1, pattern2])
    
    (pre,  pattern,  pattern1)
    
    (post, pattern,  pattern1)
    
    (pre,  pattern,  pattern2)
    
    (post, pattern,  pattern2)
    
    (post, patterns, [pattern1, pattern2])
    
    (pre,  guards, [[guard_test1]])
    
    (pre,  guard,  [guard_test1])
    
    (pre,  guard_test, guard_test1)
    
    (post, guard_test, guard_test1)
    
    (post, guard,  [guard_test1])
    
    (post, guards, [[guard_test1]])
    
    (pre,  expressions, [expression1, expression2])
    
    (pre,  expression,  expression1)
    
    (post, expression,  expression1)
    
    (pre,  expression,  expression2)
    
    (post, expression,  expression2)
    
    (post, expressions, [expression1, expression2])
    
    (post, fun_cluase,  funnction_clause1)
    
    (post, fun_clauses, [function_clause1])
    
    (post, form, form3)
    
    (post, forms, [form1, form2, form3])
