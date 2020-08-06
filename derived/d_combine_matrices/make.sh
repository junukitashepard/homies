#!/bin/bash
R CMD BATCH 4_combine_all.R out/4_combine_all.Rout
R CMD BATCH 5_build_FD.R out/5_build_FD.Rout

