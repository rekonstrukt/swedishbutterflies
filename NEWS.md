NEWS
====

For more fine-grained list of changes or to report a bug, consult 

* [The issues log](https://github.com/rekonstrukt/swedishbutterflies/issues)
* [The commit log](https://github.com/rekonstrukt/swedishbutterflies/commits/master)



# v 0.1.1

* Fixed bug in the code that tries to recover a lost db connection

* Fixed bug that under certain conditions could put the config.yml in the wrong location

* Added more documentation to README regarding how to get data from the db for a specific species

* Fixed default connection in config.yml to use "test4" database

* Added stubs for an emryonic web API exposing some of the functions (details in inst/bin/api.R) and also  providing Swagger docs, the server can be started with the exec/serve.R script


# v 0.1.0

* Added a `NEWS.md` file to track changes to the package.

* Added spatial data (sunhours, distribution data) and corresponding plots

* Added more content to the Vignette to illustrate using the spatial data and improved the README file with more getting started instructions

* Added a .travis.yml file to the project with the intention to support that Travis CI can build and deploy to GitHub Releases when a tag is pushed using git

* Fixed some bugs from initial testing on Win 7 / R 3.5

* Added several dependencies and moved db connection away from package startup to when first used, added a config for datbaseconnection that can use .Renviron

* Added .travis.yml for future support for continuous integration
