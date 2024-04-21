# ocetag 0.0.3

* [ctdTagApp()] can now plot sigma-spiciness diagrams.
* Database internal version changed from 1 to 2, and the `level`
  element of the `tags` table was renamed as `index`.
* Add [readDatabase()], which may be handy for users who do not know
  SQL.
* Add [updateDatabase()], which updates existing databases to the
  present-day format.  All functions in the package that deal with the
  database call [updateDatabase()] at the start, as a way to ensure
  backwards compatibility of database files.
* Rename `ctdtag()` as `ctdTagApp()` to avoid confusion about help pages
  for the package, as opposed to help pages for the function.
* Improve the documentation of several functions, and include a full
  example in the vignette.
* Extend the test suite to cover more functions.

# ocetag 0.0.2

* Change `createDatabase()` and `saveTag()` to handle arbitrary column
  names.
* Add a vignette showing how to work with CTD data.

# ocetag 0.0.1

* First version, with `ctdtag()` (renamed `ctdTagApp()` in version 0.0.3) being
  the main user-facing function.

