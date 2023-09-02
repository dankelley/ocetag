# Files

## 01.R

A basic app, essentially a test of whether this GUI is acceptable.
Brushing is not permitted here, either for selection or zooming.
Keystrokes are used for both actions, and also for navigation up and
down in the water column.

## 02.R

As 01.R but

1.  use SA, not S
2.  use CT, not theta
3.  add sigma0 profile
4.  add spiciness0 profile
5.  show tags at focus point (if any) in status box
6.  indicate database name in status box
7.  show database entries
8.  use full pathnames in CTD and database files

# Development ideas

1.  ☐ Make a function to call the app, with parameters such as
    1.  ☐ directory holding files
    2.  ☐ file suffix (.odf or .cnv by default)
    3.  ☐ something about default scheme (for flags)
    4.  ☐ name of database file
    5.  ☐ etc
2.  ☐ Have an interface to turn on suggested QC actions, e.g.
    1.  ☐ highlight density inversions
    2.  ☐ trim-to-descent
3.  ☐ Have an interface to accept/reject suggested actions (and maybe
    record this in the database).

# Related development ideas

1.  Develop a tool for exporting oce objects to relevant output formats.
    This will need to be undertaken by someone with R expertise, SQL
    expertise, and of course expertise in writing the output format in
    question.
