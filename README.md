
# mlr3oml

Package website: [release](https://mlr3oml.mlr-org.com/) |
[dev](https://mlr3oml.mlr-org.com/dev/)

OpenML integration to the [mlr3 ecosystem](https://mlr-org.com/).

[![r-cmd-check](https://github.com/mlr-org/mlr3oml/actions/workflows/r-cmd-check.yml/badge.svg)](https://github.com/mlr-org/mlr3oml/actions/workflows/r-cmd-check.yml)
[![CRAN Status
Badge](https://www.r-pkg.org/badges/version-ago/mlr3oml)](https://cran.r-project.org/package=mlr3oml)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)

## What is `mlr3oml`?

[OpenML](https://www.openml.org) is an open-source platform that
facilitates the sharing and dissemination of machine learning research
data. All entities on the platform have unique identifiers and
standardized (meta)data that can be accessed via an open-access REST API
or the web interface. `mlr3oml` allows to work with the REST API through
R and integrates [OpenML](https://www.openml.org) with the `mlr3`
ecosystem. Note that uploading to OpenML is currently not supported, use
the [OpenML package](https://cran.r-project.org/package=OpenML) package
for this.

As a brief demo, we show how to access an OpenML task, convert it to an
`mlr3::Task` and associated `mlr3::Resampling`, and conduct a simple
resample experiment.

``` r
library(mlr3oml)
library(mlr3)

# Download and print the OpenML task with ID 145953
oml_task = otsk(145953)
oml_task
```

    ## <OMLTask:145953>
    ##  * Type: Supervised Classification
    ##  * Data: kr-vs-kp (id: 3; dim: 3196x37)
    ##  * Target: class
    ##  * Estimation: crossvalidation (id: 1; repeats: 1, folds: 10)

``` r
# Access the OpenML data object on which the task is built
oml_task$data
```

    ## <OMLData:3:kr-vs-kp> (3196x37)
    ##  * Default target: class

``` r
# Convert the OpenML task to an mlr3 task and resampling
task = as_task(oml_task)
resampling = as_resampling(oml_task)

# Conduct a simple resample experiment
rr = resample(task, lrn("classif.rpart"), resampling)
rr$aggregate()
```

    ## classif.ce 
    ##  0.0319181

Besides working with objects with known IDs, data of interest can also
be queried using listing functions. Below, we search for datasets with
10 - 20 features, 45000 to 50000 observations and 2 classes.

``` r
odatasets = list_oml_data(
  number_features = c(10, 20),
  number_instances = c(45000, 50000),
  number_classes = 2
)

odatasets[, c("data_id", "name")]
```

    ##    data_id                        name
    ## 1:     179                       adult
    ## 2:    1461              bank-marketing
    ## 3:    1590                       adult
    ## 4:   43898                       adult
    ## 5:   44234 Bank_marketing_data_set_UCI
    ## 6:   45051                  adult-test
    ## 7:   45068                       adult

## Feature Overview

  - Datasets, tasks, flows, runs, and collections can be downloaded from
    [OpenML](https://www.openml.org) and are represented as `R6`
    classes.
  - OpenML objects can be easily converted to the corresponding `mlr3`
    counterpart.
  - Filtering of OpenML objects can be achieved using listing functions.
  - Downloaded objects can be cached by setting the `mlr3oml.cache`
    option.
  - Both the `arff` and `parquet` filetype for datasets are supported.

## Documentation

  - Start by reading the [Large-Scale Benchmarking
    chapter](https://mlr3book.mlr-org.com/chapters/chapter11/large-scale_benchmarking.html)
    from the `mlr3` book.
  - The [package website](https://mlr3oml.mlr-org.com/) contains a
    getting started guide.
  - The OpenML [API documentation](https://www.openml.org/apis) is also
    a good resource.

## Bugs, Questions, Feedback

*mlr3oml* is a free and open source software project that encourages
participation and feedback. If you have any issues, questions,
suggestions or feedback, please do not hesitate to open an “issue” about
it on the GitHub page\!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” that showcases the behaviour (but don’t
worry about this if the bug is obvious).
