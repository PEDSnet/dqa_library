#!/usr/bin/env Rscript

# Sys.setenv("HOME" = '/home/wieandk')
# Sys.setenv("USER" = 'wieandk')
# 
# Sys.setenv(PGPASS=file.path(Sys.getenv('HOME'), '.pgpass'))

args = commandArgs(trailingOnly=TRUE)

setwd(dirname(args[1]))
source(args[1]) 