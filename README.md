# callr



[![Build Status](https://travis-ci.org/traitecoevo/callr.png?branch=master)](https://travis-ci.org/traitecoevo/callr)

Simple package for calling R from elsewhere via json

```json
{
    "function": "myfunction",
    "args": [
        {
            "name": "foo",
            "value": "bar"
        }
    ]
}

```

Then:



```
callr myfile.json
```

runs

```r
myfunction(foo="bar")
```

and saves the result in the element `value` in the original json.

```json
{
    "function": "myfunction",
    "args": [
        {
            "name": "foo",
            "value": "bar"
        }
    ],
    "value": [
        "bar",
        "bar",
        "bar"
    ]
}
```

Further options:

```
Usage:
  callr [options] <filename> [<outfile>]

Options:
  -h --help      Show this screen.
  -b --backup    Make a backup of filename if outfile is same as filename?
  -u --ugly      Don't prettify json output
  -v --vectors   Don't auto unbox vectors to scalars
```

# Installation

```r
devtools::install_github("traitecoevo/callr")
callr::install("~/bin")
```

The second line copies the `callr` script somewhere -- preferably in your `$PATH`.
