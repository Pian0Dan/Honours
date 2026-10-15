#!/usr/bin/env Rscript
# Pre-render hook (see _quarto.yml project.pre-render): ensures the renv
# project library matches renv.lock before any chapter is rendered.

source("renv/activate.R")
renv::restore(prompt = FALSE)
