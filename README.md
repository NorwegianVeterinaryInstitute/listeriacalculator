
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Listeria Calculator

<!-- badges: start -->
<!-- badges: end -->

This is a R Shiny Golem app for calculating listeria growth in raw fish.

It is deployed to Posit Connect server from git using a `manifest.json`
file.

Since the app is a package, the `manifest.json` has to capture it in the
environment to be successfully deployed. Therefore before running the
`rsconnect::writeManifest()` command in an R session, the package needs
to be installed from Github with, for example:
`remotes::install_github("NorwegianVeterinaryInstitute/listeriacalculator")`.

The `manifest.json` has to be rebuild when a new dependency is added.

Refer to the function(s) documentations for the details about the
modelling.
