# Nesmacs extensions

Forge (`M-x forge-prs`, `C-x v p`) and HTTP Client (`M-x http`, `C-c C-c` to
send) are bundled and enabled on a fresh install. Use **Disable** or
**Uninstall** in their hub entries to opt out on the next launch. The bundled
packages remain available to reinstall offline.

Run `make extension-catalog` to build all three JARs and the catalog under
`target/extension-catalog/`. In `M-x hub`, press `s` and enter the generated
`catalog.edn` path. Select Project Notes, press `RET` on **Install**, and restart Nesmacs.
`M-x project-notes` opens the notes file for the current project. **Configure
options** in the package's hub entry changes its file name.

Use `TAB` to switch between Explore and Installed, `/` to search, `g` to refresh,
and `q` to go back. `NESMACS_EXTENSION_CATALOG` supplies the initial source when
the user has yet to select one.

Press `U` in the hub or run `M-x package-upgrade-all` to refresh the catalog and
stage all installed package updates. Restart to load the updated packages. A
failed download leaves that package at its previous version. Its details in
the hub show the error.

## Publish a catalog

Host `catalog.edn` over HTTPS in the catalog repository. Place each JAR in a
versioned release of its own repository and use the release asset URL in
`:url`. A relative URL resolves against the catalog URL. The generated example
shows the complete catalog format, including the SHA-256 digest. List one
recommended version per package ID. Refreshing a catalog retains the last
successful copy when a request fails.

## Package a library

Put `plugin.edn` at the root of a JAR alongside the Clojure source and resources.
Its `:id`, `:version`, and `:api` match the catalog entry. API `1` supports command,
mode, key binding, and hook registration. API `2` adds `api/after-command`.
Declare the oldest API your package needs. Use a unique package namespace
and a fully qualified `:entry` symbol.
The entry function receives a map of option values. The supported option types
are `:boolean`, `:string`, and `:choice`. A choice has a vector of strings under
`:choices`. Each option declares its `:key`, `:label`, and `:default`.
In this checkout's build script, add extra resource paths to the manifest's
`:files` vector. The entry namespace's source and manifest are included automatically.

Use `nesmacs.api` to register commands, modes, key bindings, and hooks. The
extension uses the editor's Clojure runtime and may include its own libraries
in its JAR. Install packages from catalogs you trust. Extensions run with the
same privileges as your `init.clj`.
A command can use `api/after-command` to submit an asynchronous operation
once its state has committed. The effect receives a function to apply its
result to the current editor state.

Nesmacs loads enabled packages before `init.clj`, so your own configuration
can override their bindings. If an update fails to load, the hub displays the
error and selects the previous working version for the following restart.
Uninstalling removes the package from the startup list. Downloaded JARs stay
in the cache under `~/.nesmacs/plugins/`.
