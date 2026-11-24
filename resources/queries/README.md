# Vendored queries

`highlights.scm` and `folds.scm` come from nvim-treesitter at
commit `52773f0d13a0c11d0c3ef33c14ab6ecc1c17148b` (Apache-2.0, see LICENSE.nvim-treesitter).
They are generated, not edited: change `script/vendor_queries.clj`
and regenerate. `#set!` directives are dropped because the JNI
binding does not accept every arity nvim uses; xmas does not read
query metadata.
