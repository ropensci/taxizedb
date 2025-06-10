# taxizedb 0.3.2

* This patch release is submitted following the removal of the package from CRAN due to a policy violation regarding correspondence format: "All correspondence with CRAN must be sent ... in plain text ASCII and not HTML". I accidentally replied to a CRAN Team member in HTML format and as a result the package was removed from CRAN without warning. I apologize for this oversight and kindly request that the package be reinstated on CRAN.
* `urlchecker::url_check()` failed for https://www.worldfloraonline.org/. I checked the URL manually and the website is up and running.

# taxizedb 0.3.1

* This patch release was submitted to CRAN because the maintainer of the project
has changed.
* rhub::check_for_cran() returns the following NOTE: Found the following files/directories: 'lastMiKTeXException'. This issue seems to be unrelated to taxizedb: https://github.com/r-hub/rhub/issues/503

# taxizedb 0.3.0

## Test environments

* local OS X install, R 4.0.3 patched
* ubuntu 16.04 (on GitHub Actions), R 4.0.3
* win-builder (devel and release)

## R CMD check results

No notes

## Reverse dependencies

There are no reverse dependencies.

---

This version fixes a bug, adds a new function, and one function gains a new parameter.

Thanks!
Scott Chamberlain
