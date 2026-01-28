# Abstract Base Class for OpenML objects.

All OML Objects inherit from this class. Don't use his class directly.

## Active bindings

- `desc`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Description of OpenML object.

- `cache_dir`:

  (`logical(1)` \| `character(1)`)  
  Stores the location of the cache for objects retrieved from OpenML. If
  set to `FALSE`, caching is disabled. Objects from the test server are
  stored in the subdirectory 'test', those from the public server are
  stored in the subdirectory 'public'.

  The package [qs2](https://CRAN.R-project.org/package=qs2) is required
  for caching.

- `id`:

  (`integer(1)`)  
  OpenML data id.

- `server`:

  (`character(1)`)  
  The server for this object.

- `man`:

  (`character(1)`)  
  The manual entry.

- `name`:

  (`character(1)`)  
  The name of the object.

- `type`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The type of OpenML object (e.g. task, run, ...).

- `test_server`:

  (`logical(1)`)  
  Whether the object is using the test server.

## Methods

### Public methods

- [`OMLObject$new()`](#method-OMLObject-new)

- [`OMLObject$help()`](#method-OMLObject-help)

- [`OMLObject$clone()`](#method-OMLObject-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OMLObject$new(id, test_server = test_server_default(), type)

#### Arguments

- `id`:

  (`integer(1)`)  
  OpenML id for the object.

- `test_server`:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

- `type`:

  (`charcater()`)  
  The type of OpenML object (e.g. run, task, ...).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    OMLObject$help()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OMLObject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
