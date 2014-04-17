roxyPackage
===========

[![Build Status](https://travis-ci.org/saurfang/roxyPackage.svg?branch=master)](https://travis-ci.org/saurfang/roxyPackage)

Workflow utility to maintain R packages

This is directly based on the current version of roxyPackage http://reaktanz.de/?c=hacking&s=roxyPackage

Since neither the package author Michalke or anyone has published this codebase on Github, this repository has been created to facilitate experimental feature enhancements and bug fixes.


##License
The source code is distributed under GPL 3 as can be found in the LICENSE.txt


##What's New

* `archive.packages` now works under Windows
* `write_PACKAGES` wrapper has been added to speed up indexing on network drive under Windows


##How to Get It

####Stable Version
You may install the package directly from original Author Michalke's R repository:
```R
install.packages("roxyPackage", repo="http://R.reaktanz.de")
```

####Development Version
To get latest version on Github:
```R
devtools::install_github(c("saurfang/roxyPackage"))
```
