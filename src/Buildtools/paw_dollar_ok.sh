#!/bin/bash
################################################################################
##
##   paw_dollar_ok.sh
##
##   replaces dollar signs by double underscores. Used to preprocess fortran 
##   codes of the cppaw distribution in order to allow dollar signs in 
##   in the code, but to aboid the -fdollar ok option. The reason for using
##   this shell script is that thus function can easily identified in the 
##   files of the distribution.
##
##   The specification fo OPENMP are identified by !$OMP, which must be 
##   unchanged
##
##    Author: P. Bloechl, May 2024
################################################################################
sed -e "s/[$]/__/g" </dev/stdin | sed -e "s/!__[oO][mM][pP]/!\$OMP/g" >/dev/stdout
