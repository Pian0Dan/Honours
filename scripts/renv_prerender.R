#!/usr/bin/env Rscript
# Pre-render hook (see _quarto.yml project.pre-render): ensures the renv
# project library matches renv.lock before any chapter is rendered.
#
# `gurobi` is excluded because it ships with the proprietary Gurobi
# Optimizer installer and cannot be fetched from a package repository.
# It must be installed manually (see README) from a local Gurobi install.

source("renv/activate.R")

lockfile <- renv::lockfile()
packages <- setdiff(names(lockfile$Packages), "gurobi")

renv::restore(packages = packages, prompt = FALSE)

if (!requireNamespace("gurobi", quietly = TRUE)) {
  warning(
    "The 'gurobi' package is not installed. It is not managed by renv ",
    "because it requires a licensed Gurobi Optimizer installation. ",
    "Install it manually from <GUROBI_HOME>/R before rendering chapters ",
    "that depend on it.",
    call. = FALSE
  )
}
