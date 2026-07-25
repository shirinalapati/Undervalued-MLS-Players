#!/usr/bin/env Rscript
status <- system2("Rscript", "tests/test_value_index.R")
if (!identical(status, 0L)) quit(status = status)
message("test runner complete")
