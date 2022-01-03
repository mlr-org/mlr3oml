## Bechmarking

In this chapter you will learn how utilize `mlr3oml` for benchmarking learners.
You will learn how to:

1. Download an existing [benchmark suite](https://new.openml.org/search?type=benchmark&sort=tasks_included&study_type=task)
1. Conduct a benchmark experiment
1. Compare your learner's performance with the results of others
1. Publish your benchmark result

**1. Downloading a benchmark suite**

To find the appropriate benchmark collection for your task visit the [OpenML website](https://new.openml.org/search?type=benchmark&sort=tasks_included&study_type=task).
In this tutorial we use the OpenML-CC18 benchmark suite, that has id 268.
You can connect to this benchmark suite, using the class `OMLCollection` as follows

```r
library("mlr3oml")
oml_suite = OMLCollection$new(99)

```

An `OMLCollection` is an object containing a collection of *tasks*, *flows*, *datasets*
and *runs*, as well as some metainformation including the creator of the benchmark suite
or its description. Printing the object shows the following information

```r
print(oml_suite)
#  <OMLCollection:8>
# * data:  8
# * flows: 0
# * runs:  0
# * tasks: 8
```

In order use the OpenML *tasks* within the `mlr3` ecosystem, we can access the `$tasks` field,
which returns an instance of the class `OMLContainer`, that provides the method `$convert()`,
which converts the `OMLContainer` instance into a list of `mlr3::Task`s.

```r
oml_tasks = oml_suite$tasks
print(oml_tasks)
# <OMLContainer: OMLTask (72)>
tasks = oml_tasks$convert()
```

Note as opposed to `mlr3`, where a resampling is seperate from a `Task`, the OpenML Tasks come with a resampling split, if you want to use it you can also extract it as follows.

```r
oml_resamplings = oml_suite$resamplings
mlr_resamplings = oml_resamplings$convert()
```

To create the benchmark design we can use the function benchmark_design:
In this tutorial we use the learner [mlr3::LearnerClassifRpart].

```r
learner = lrn("classif.rpart")
design = benchmark_design(tasks, learner, resamplings)
```

The benchmark experiment can now be run as usual

```r
bmr = benchmark(design)
```

If you want to publish your result to OpenML, this can be done with a single call

```r
publish(bmr)
```
