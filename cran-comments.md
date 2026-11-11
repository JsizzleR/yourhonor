## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.

* The incoming-feasibility check found one URL returning 404:
  <https://jsizzler.github.io/yourhonor/articles/ellmer-judge.html>, linked
  from the "Get started" vignette and the README. It is an article on the
  package website that the next deployment of the site publishes; the
  check is re-run against the deployed site before submission.

* "Files 'README.md' or 'NEWS.md' cannot be checked without 'pandoc' being
  installed": pandoc was not on the check's PATH on the test machine. Both
  files convert with pandoc run directly.

## Test environments

* local macOS 27.0.1 (aarch64-apple-darwin), R 4.6.0:
  `R CMD check --as-cran --no-manual`
