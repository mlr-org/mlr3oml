# Convert an OpenML Flow to a mlr3 Learner

By default this function creates a Pseudo-Learner (that cannot be used
for training or prediction) for the given task type. This enables the
conversion of OpenML Runs to
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)s.
This is well defined because each subcomponent (i.e. id) can only appear
once in a Flow according to the OpenML docs.

## Usage

``` r
# S3 method for class 'OMLFlow'
as_learner(x, task_type = NULL, ...)
```

## Arguments

- x:

  (OMLFlow) The OMLFlow that is converted to a mlr3::Learner.

- task_type:

  (`character(1)`) The task type to constrct a pseudo-learner. For more
  information see
  [OMLFlow](https://mlr3oml.mlr-org.com/dev/reference/oml_flow.md).

- ...:

  Additional arguments.
