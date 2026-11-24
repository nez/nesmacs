; Vendored from nvim-treesitter 52773f0d13a0c11d0c3ef33c14ab6ecc1c17148b (Apache-2.0).
; Sources: ecma, typescript/folds.scm
; Normalized for nesmacs: inherits resolved, #lua-match? -> #match?,
; #set! dropped. Regenerate with script/vendor_queries.clj.

[
  (arguments)
  (for_in_statement)
  (for_statement)
  (while_statement)
  (arrow_function)
  (function_expression)
  (function_declaration)
  (class_declaration)
  (method_definition)
  (do_statement)
  (with_statement)
  (switch_statement)
  (switch_case)
  (switch_default)
  (import_statement)+
  (if_statement)
  (try_statement)
  (catch_clause)
  (array)
  (object)
  (generator_function)
  (generator_function_declaration)
] @fold


[
  (interface_declaration)
  (internal_module)
  (type_alias_declaration)
  (enum_declaration)
] @fold

