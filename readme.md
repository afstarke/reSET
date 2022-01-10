reSET
================

## reSET

`reSET` , while currently still under active development, intends to
help collectors of marsh surface elevation tables and marker horizon
(SET-MH) data with bringing their data into R for any number of QA, data
visualization, analysis and report creation tasks that the user wishes.
This package currently consists of some rather specific functions for
interacting with the SET-MH database developed by the National Park
Service (Marsh Elevation Monitoring Database v2.75) as referenced in
Lynch et al (2015). It is my hope that as new database systems become
developed to manage SET-MH data, these functions can be enhanced to
allow for more flexible inputs while yielding consistent outputs.

It is my hope that this somewhat narrowly focused package can be
leveraged by other practitioners into something more robust that will
promote the sharing of methods, analyses, and results so that we all can
benefit from the work of so many using this technique of monitoring to
answer some of our big questions relating to marsh resilience. With that
as a goal, this code has been licensed under the GPL 3.0 which allows
use of the code, provided any resulting products be licensed similarly
so that we can all benefit from any development that builds from this or
other similar efforts.

<<<<<<< HEAD
## Installation

Currently this package is only available here on GitHub and can be
installed using the `remotes` package.
`# install.packages("remotes") remotes::install_github("afstarke/reSET")`

=======
>>>>>>> 287fedf4a98a93fc7f5f4e61cc5d8bb8ec5d30c7
### What reSET is NOT?

This package does not intend to be a *push-button; get-answer* type of
tool. It can not substitute the need for careful and thoughtful
approaches to data management, research questions being asked,
statistical analysis etc. It is merely a tool to help in that process.
View this as a disclaimer that there can be, and likely are, bugs in
these functions that may result in error messages or erroneous results
that users of this package should be on the look out for. Please file
issues that you may encounter.

## What’s with the name?

`reSET` is actually a reboot of an effort begun several years ago under
the SETr name. Between that time and resuming this current effort a very
well crafted set of tools for the interacting and reporting of SET data
<<<<<<< HEAD
was developed by Kim Cressman as part of a [larger national synthesis
across NERR
sites](https://nerrssciencecollaborative.org/project/Cressman18), which
resulted in the creation of the [SETr
package](https://github.com/swmpkim/SETr). To avoid confusion (and
competing branding issues when negotiating with corporate sponosors)
moving forward this package was renamed to `reSET` but will always seek
to interact well with the work that has gone into the SETr package. (see
set_to_setr for example)
=======
was developed by [Kim Cressman as part of a larger national synthesis
across NERR sites, which resulted in the creation of the SETr
package](https://nerrssciencecollaborative.org/project/Cressman18). To
avoid confusion (and competing branding issues when negotiating with
corporate sponosors) moving forward this package was renamed to `reSET`
but will always seek to interact well with the work that has gone into
the SETr package. (see set_to_setr for example)
>>>>>>> 287fedf4a98a93fc7f5f4e61cc5d8bb8ec5d30c7

## Future Development

As mentioned this package is very far from what I envision as complete.
You may find notes scattered within the code noting improvements needed
or enhancements that would be nice to implement. These are all things
that were added during development, some may be more important than
others. If there are any glaring omissions or errors please feel free to
reach out by filing an Issue through GitHub.

To date, the focus of this package was on simplifying the process of
reading SET data into R for use in more typical R workflows. As I
complete more of the documentation of what’s currently in this package I
hope to focus on the data visualization and statistical analysis side of
the work flow.

<<<<<<< HEAD
### References

=======
>>>>>>> 287fedf4a98a93fc7f5f4e61cc5d8bb8ec5d30c7
James C. Lynch, Phillippe Hensel, and Donald R. Cahoon. “The Surface
Elevation Table and Marker Horizon Technique: A Protocol for Monitoring
Wetland Elevation Dynamics.” Report. Natural Resource Report, 2015. USGS
Publications Warehouse. <http://pubs.er.usgs.gov/publication/70160049>.
