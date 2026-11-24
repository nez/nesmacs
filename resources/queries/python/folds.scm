; Vendored from nvim-treesitter 52773f0d13a0c11d0c3ef33c14ab6ecc1c17148b (Apache-2.0).
; Sources: python/folds.scm
; Normalized for xmas: inherits resolved, #lua-match? -> #match?,
; #set! dropped. Regenerate with script/vendor_queries.clj.

[
  (function_definition)
  (class_definition)
  (while_statement)
  (for_statement)
  (if_statement)
  (with_statement)
  (try_statement)
  (match_statement)
  (import_from_statement)
  (parameters)
  (argument_list)
  (parenthesized_expression)
  (generator_expression)
  (list_comprehension)
  (set_comprehension)
  (dictionary_comprehension)
  (tuple)
  (list)
  (set)
  (dictionary)
  (string)
] @fold

[
  (import_statement)
  (import_from_statement)
]+ @fold

