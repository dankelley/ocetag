# ocetag 0.0.4

* Rename `createDatabase()` as [useDatabase()], and remove the
  [updateDatabase()] call from other package functions.

# ocetag 0.0.3

* [ctdTagApp()] shows spiciness contours on TS diagrams.
* [ctdTagApp()] can plot sigma-spiciness diagrams.
* Database internal version changed from 1 to 2, and the `level`
  element of the `tags` table was renamed as `index`.
* Add [readDatabase()], which may be handy for users who do not know
  SQL.
* Add [updateDatabase()], which [useDatabase()] uses to update
  existing databases to the present-day format.
* Rename `ctdtag()` as [ctdTagApp()] to avoid confusion about help pages
  for the package, as opposed to help pages for the function.
* Improve the documentation of several functions, and include a full
  example in the vignette.
* Extend the test suite to cover more functions.

# ocetag 0.0.2

* Change `createDatabase()` and [saveTag()] to handle arbitrary column
  names.
* Add a vignette showing how to work with CTD data.

# ocetag 0.0.1

* First version, with `ctdtag()` (renamed `ctdTagApp()` in version 0.0.3) being
  the main user-facing function.

