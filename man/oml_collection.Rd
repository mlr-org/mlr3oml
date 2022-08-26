% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/OMLCollection.R
\name{oml_collection}
\alias{oml_collection}
\alias{OMLCollection}
\title{OpenML Collection}
\description{
This is the class for collections (previously known as studies) served on
\url{https://openml.org/search?type=study&study_type=task&sort=tasks_included}.
It is used both for Run Collections and Task Collections.
(Note that all Benchmark Suites on OpenML are also Collections).
A Run Collection (\code{main_entity_type = "run"}) contains runs, flows, datasets and tasks.
A Task Collection (\code{main_entity_type = "task"}) contains tasks and datasets.
}
\section{mlr3 Intergration}{

\itemize{
\item Obtain a list of \link[mlr3:Task]{mlr3::Task}s using \code{as_tasks()}.
\item Obtain a list of \link[mlr3:Resampling]{mlr3::Resampling}s using \code{as_resamplings()}.
\item Obtain a list of \link[mlr3:Learner]{mlr3::Learner}s using \code{as_learners()} (if main_entity_type is "run").
\item Obtain a \link[mlr3:BenchmarkResult]{mlr3::BenchmarkResult} using \code{as_benchmark_result()} (if main_entity_type is "run").
}
}

\examples{
\donttest{
library("mlr3")
# OpenML Run Collection:
collection = OMLCollection$new(232L)
collection$tasks
collection$data
collection$flows
collection$runs

# mlr3 conversion:
tasks = as_tasks(collection)
resamplings = as_resamplings(collection)
# construct pseudo-learners as these are sklearn flows
learners = as_learners(collection, "classif")

# Although pseudo-learners are non-executable the runs can still be analyzed.
bmr = as_benchmark_result(collection)
bmr$score(msr("classif.ce"))
}
}
\references{
Vanschoren J, van Rijn JN, Bischl B, Torgo L (2014).
\dQuote{OpenML.}
\emph{ACM SIGKDD Explorations Newsletter}, \bold{15}(2), 49--60.
\doi{10.1145/2641190.2641198}.
}
\section{Super class}{
\code{\link[mlr3oml:OMLObject]{mlr3oml::OMLObject}} -> \code{OMLCollection}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{desc}}{(\code{list()})\cr
Colllection description (meta information), downloaded and converted from the JSON API response.}

\item{\code{main_entity_type}}{(\code{character(n)})\cr
The main entity type, either \code{"run"} or \code{"task"}.}

\item{\code{flow_ids}}{(\code{integer(n)})\cr
An vector containing the flow ids of the collection.}

\item{\code{data_ids}}{(\code{integer(n)})\cr
An vector containing the data ids of the collection.}

\item{\code{run_ids}}{(\code{integer(n)})\cr
An vector containing the run ids of the collection.}

\item{\code{task_ids}}{(\code{integer(n)})\cr
An vector containing the task ids of the collection.}

\item{\code{runs}}{(\code{data.table()})
A data.table summarizing the runs included in the collection. Returns NULL for
Task Collections.}

\item{\code{flows}}{(\code{data.table()})
A data.table summarizing the flows included in the collection. Returns NULL for
Task Collections.}

\item{\code{data}}{(\code{data.table()})
A data.table summarizing the datasets included in the collection.}

\item{\code{tasks}}{(\code{data.table()})
A data.table summarizing the tasks included in the collection.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-OMLCollection-new}{\code{OMLCollection$new()}}
\item \href{#method-OMLCollection-print}{\code{OMLCollection$print()}}
\item \href{#method-OMLCollection-clone}{\code{OMLCollection$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3oml" data-topic="OMLObject" data-id="help"><a href='../../mlr3oml/html/OMLObject.html#method-OMLObject-help'><code>mlr3oml::OMLObject$help()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLCollection-new"></a>}}
\if{latex}{\out{\hypertarget{method-OMLCollection-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLCollection$new(
  id,
  cache = getOption("mlr3oml.cache", FALSE),
  parquet = getOption("mlr3oml.parquet", FALSE),
  test_server = getOption("mlr3oml.test_server", FALSE)
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{integer(1)})\cr
OpenML id for the object.}

\item{\code{cache}}{(\code{logical(1)} | \code{character(1)})\cr
See field \code{cache} for an explanation of possible values.
Defaults to value of option \code{"mlr3oml.cache"}, or \code{FALSE} if not set.}

\item{\code{parquet}}{(\code{logical(1)})\cr
Whether to use parquet instead of arff.}

\item{\code{test_server}}{(\code{character(1)})\cr
Whether to use the OpenML test server (https://test.openml.org/) or public server
(https://www.openml.org/).
Defaults to value of option \code{"mlr3oml.test_server"}, or \code{FALSE} if not set.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLCollection-print"></a>}}
\if{latex}{\out{\hypertarget{method-OMLCollection-print}{}}}
\subsection{Method \code{print()}}{
Prints the object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLCollection$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-OMLCollection-clone"></a>}}
\if{latex}{\out{\hypertarget{method-OMLCollection-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{OMLCollection$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}