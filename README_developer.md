# Changes

* version 1: experimental version (similar to that of argo-summer
  project)
* version 2: as version 1, but use 'index' instead of 'level'


# Development ideas

1. [ ] Make ctdtag() handle parameters such as
   a. [ ] directory holding files
   b. [ ] file suffix (.odf or .cnv by default)
   c. [ ] something about default scheme (for flags)
   d. [ ] name of database file
   e. [ ] etc
2. [ ] Have an interface to turn on suggested QC actions, e.g.
   a. [ ] highlight density inversions
   b. [ ] trim-to-descent
3. [ ] Have an interface to accept/reject suggested actions (and maybe record
   this in the database).

# Related development ideas

1. Develop a tool for exporting oce objects to relevant output formats.  This
   will need to be undertaken by someone with R expertise, SQL expertise, and of
   course expertise in writing the output format in question.

