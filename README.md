# callr

## Calling R from the shell



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
  -s --strict    Warn about extra fields
  -v --vectors   Don't auto unbox vectors to scalars
```

## Script installation

Using `callr` is easiest if you install the `callr` script:

```r
devtools::install_github("traitecoevo/callr")
callr::install("~/bin")
```
The second line copies the `callr` script somewhere -- preferably in your `$PATH`.

This allows using `callr` from the shell with:

```
callr myfile.json
```

or from your favourite other scripting language through their system interface.

## Calling system programs from R

Calling system commands from R error prone as error handling is
inconsistent.  `callr` includes a function `call_system` to help
here.  This is a very thin wrapper around `system2`, and hides output
unless there is an error (exit code other than 0) when it throws an
error and prints a (possibly truncated) error message.

All output (non-truncated) will be returned by a successful run of `call_system`.
