# Check PID version

This function takes an identifier and checks to see if it has been
obsoleted.

## Usage

``` r
check_version(pid, formatType = NULL)
```

## Arguments

- pid:

  (character) The persistent identifier of a data, metadata, or resource
  map object on a DataONE member node.

- formatType:

  (character) Optional. The format type to return (one of 'data',
  'metadata', or 'resource').

## Value

(data.frame) A data frame of object version PIDs and related
information.

## Examples

``` r
if (FALSE) { # \dontrun{
# Most data URLs and identifiers work
check_version("https://cn.dataone.org/cn/v2/resolve/urn:uuid:a2834e3e-f453-4c2b-8343-99477662b570")
check_version("doi:10.18739/A2ZF6M")

# Specify a formatType (data, metadata, or resource)
check_version("doi:10.18739/A2ZF6M", formatType = "metadata")

# Returns a warning if the identifier has been obsoleted
check_version("doi:10.18739/A2HF7Z", formatType = "metadata")

# Returns an error if no matching identifiers are found
check_version("a_test_pid")

# Returns a warning if several identifiers are returned
check_version("10.18739/A2057CR99")
} # }
```
