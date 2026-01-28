# Interface to OpenML Flows

This is the class for flows served on
[OpenML](https://www.openml.org/search?type=flow&sort=runs). Flows
represent machine learning algorithms. This object can also be
constructed using the sugar function
[`oflw()`](https://mlr3oml.mlr-org.com/reference/oflw.md).

## mlr3 Integration

- Obtain a
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) using
  [`mlr3::as_learner()`](https://mlr3.mlr-org.com/reference/as_learner.html).

## References

Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014). “OpenML.” *ACM
SIGKDD Explorations Newsletter*, **15**(2), 49–60.
[doi:10.1145/2641190.2641198](https://doi.org/10.1145/2641190.2641198) .

## Super class

[`mlr3oml::OMLObject`](https://mlr3oml.mlr-org.com/reference/oml_object.md)
-\> `OMLFlow`

## Active bindings

- `parameter`:

  (`data.table`)  
  The parameters of the flow.

- `dependencies`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The dependencies of the flow.

- `tags`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns all tags of the object.

## Methods

### Public methods

- [`OMLFlow$new()`](#method-OMLFlow-new)

- [`OMLFlow$print()`](#method-OMLFlow-print)

- [`OMLFlow$download()`](#method-OMLFlow-download)

- [`OMLFlow$clone()`](#method-OMLFlow-clone)

Inherited methods

- [`mlr3oml::OMLObject$help()`](https://mlr3oml.mlr-org.com/reference/OMLObject.html#method-help)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    OMLFlow$new(id, test_server = test_server_default())

#### Arguments

- `id`:

  (`integer(1)`)  
  OpenML id for the object.

- `test_server`:

  (`character(1)`)  
  Whether to use the OpenML test server or public server. Defaults to
  value of option `"mlr3oml.test_server"`, or `FALSE` if not set.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the object.

#### Usage

    OMLFlow$print()

------------------------------------------------------------------------

### Method `download()`

Downloads the whole object for offline usage.

#### Usage

    OMLFlow$download()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OMLFlow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# For technical reasons, examples cannot be included in this R package.
# Instead, these are some relevant resources:
#
# Large-Scale Benchmarking chapter in the mlr3book:
# https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html
#
# Package Article:
# https://mlr3oml.mlr-org.com/articles/tutorial.html
```
