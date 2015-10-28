# callr

[![Build Status](https://travis-ci.org/traitecoevo/callr.png?branch=master)](https://travis-ci.org/traitecoevo/callr)

The `callr` package has two main uses; calling R from other programs (including the shell) and for calling other programs from R.

## Calling R from the shell



There are myriad interfaces for calling R from other languages (e.g., the python package `rpy2`) but debugging these interfaces when something goes wrong is challenging.  Similarly, ad-hoc decisions about serialisation must be made that can fail subtly.  We use the [`jsonlite`](https://github.com/jeroenooms/jsonlite) package to serialise calls to R from any language to give a simple and consistent interface.

`callr` lets you specify, in a json file,

* packages to load
* R files to source
* A function to call
* Arguments to the function

The result will then be added back into the original file.  For example, suppose you had some R function definitions (in a file `example.R`)

```r
## Random multivariate normals with random variance covariance
## matrices, using the `mvtnorm` package:
random_mvnorm <- function(n, dim) {
  rmvnorm(n, rnorm(dim), random_vcv(dim))
}

## Random positive definite matrices:
random_vcv <- function(n) {
  decomp <- qr(matrix(ncol=n, rnorm(n^2)))
  O <- qr.Q(decomp) %*% diag(sign(diag(qr.R(decomp))))
  t(O) %*% diag(runif(n, 0, 10)) %*% O
}
```

We might write a json file:

```json
{
    "packages": "mvtnorm",
    "sources": "example.R",
    "function": "random_mvnorm",
    "args": [
        {
            "value": 10
        },
        {
            "name": "dim",
            "value": 3
        }
    ]
}
```

This will be translated into a function:

```r
random_mvnorm(10L, dim = 3L)
```

Note that because json hashes do not guarantee order, the arguments are in an array (the `[` bits), and the argument name goes in the `name` field.  Names are optional.

This can be run from within R like:


Though it will certainly be easier to run from the a shell session with:

```
Rscript -e 'callr::callr("example.json")
```

or by installing the shell script helper:

```r
callr::install_callr("~/bin")
```

And running

```
callr example.json
```

Regardless of how `callr` is run, the file `example.json` now includes output:

```json
{
    "packages": "mvtnorm",
    "sources": "example.R",
    "function": "random_mvnorm",
    "args": [
        {
            "value": 10
        },
        {
            "name": "dim",
            "value": 3
        }
    ],
    "value": [
        [
            -2.1261,
            -1.2206,
            5.4421
        ],
        [
            -3.8416,
            -0.8677,
            1.984
        ],
        [
            -0.1427,
            0.2005,
            2.5644
        ],
        [
            -2.9937,
            -4.3684,
            3.9871
        ],
        [
            5.5665,
            -2.0638,
            3.3493
        ],
        [
            -2.3557,
            -2.2339,
            0.847
        ],
        [
            0.0287,
            -4.6195,
            2.7421
        ],
        [
            -4.335,
            -2.9102,
            2.9362
        ],
        [
            -0.1674,
            1.3204,
            2.5689
        ],
        [
            -0.5068,
            1.4915,
            2.9484
        ]
    ]
}

```

Further options to the script:

```plain
Usage:
  callr [options] <filename> [<outfile>]

Options:
  -h --help      Show this screen.
  -b --backup    Make a backup of filename if outfile is same as filename?
  -u --ugly      Don't prettify json output
  -s --strict    Warn about extra fields
  -v --vectors   Don't auto unbox vectors to scalars
```



## Calling system programs from R

Calling system commands from R error prone as error handling is difficult and varies on things like whether standard output is to be collected.  `callr` includes a function `call_system` for the case where a system call is blocking (i.e., we want to wait until it completes).  It is a very thin wrapper around R's `system2` function and hides output unless there is an error, when it will print a (possibly truncated) error message.  This makes it useful for things like output of a LaTeX compilation where the amount of generated output is far too much to be useful but when an error occurs, some output is useful.

All output (non-truncated) will be returned, invisibly, by a successful run of `call_system`.

As an example, due to the travis badge in this README, LaTeX cannot compile the LaTeX that pandoc generates:


```r
callr::call_system("pandoc", c("README.md", "-o", "README.tex", "--standalone"))
callr::call_system("pdflatex",
                   c("-interaction=nonstopmode", "-halt-on-error",
                     "README.tex"))
```

```
## Error in callr::call_system("pdflatex", c("-interaction=nonstopmode", : Running command:
##   '/Library/TeX/texbin/pdflatex' -interaction=nonstopmode -halt-on-error README.tex
## had status 1
## Program output:
## ---------------------------------------------------------------------------
##  restricted \write18 enabled.
## entering extended mode
## (./README.tex
## [[... 79 lines dropped ...]]
## ))
##
## LaTeX Warning: File `https://travis-ci.org/traitecoevo/callr.png?branch=master'
##  not found on input line 114.
##
##
## ! LaTeX Error: Unknown graphics extension: .png?branch=master.
##
## See the LaTeX manual or LaTeX Companion for explanation.
## Type  H <return>  for immediate help.
##  ...
##
## l.114 ...org/traitecoevo/callr.png?branch=master}}
##
## !  ==> Fatal error occurred, no output PDF file produced!
## Transcript written on README.log.
## ---------------------------------------------------------------------------
```

only the last bit of output is included in the error, with many lines of output dropped.



## Installation

Install from github with

```
devtools::install_github("traitecoevo/callr")
callr::install_callr("~/bin")
```

replacing the path in the second line with something in your `$PATH`.
