#
SHELL=/bin/bash
#
#########################################################################
##                                                                     ##
##  make a paw executable and a paw library using gmake                ##
##  ===================================================                ##
##                                                                     ##
#########################################################################
##                                                                     ##
##  collective targets:                                                ##
##                                                                     ##
##    default           everything exept parallel                      ##
##    clean             cleans up everything                           ##
##    small             makes a set for the average user               ##
##                      (avoids none,dbg,prof)                         ##
##    all               everything exept parallel                      ##
##    all_new         everything except parallel with full dependencies##
##    tools          all tools                                         ##
##                                                                     ##
#########################################################################
##                                                                     ##
##  Individual targets                                                 ##
##                                                                     ##
##    none              paw executable (standard compilation)          ##
##    fast              paw executable (optimizer)                     ##
##    dbg               paw executable (debugging)                     ##
##    prof              paw executable (profiling)                     ##
##                                                                     ##
##    for the above four targets x there are versions                  ##
##      x_new           :  like x, but contains all dependencies       ##
##      x_parallel      :  like x, but for parallel executable         ##
##      x_parallel_new  : like x_new, but for parallel executable      ##
##      clean_x:        : clean up                                     ##
##      clean_x_parallel: clean up for parallel executable             ##
##                                                                     ##
##   the parallel versions exist only if a TPARALLEL=T has been        ##
##   specified in the parameter file                                   ##
##                                                                     ##
##    lib                the paw library                               ##
##                                                                     ##
##    newatom, atom, tra, wave, 1davpot, cmcwave, grab,dos, preopt     ##
##    converttra, cleantra, strc, tostrc, toxyz
##        (targets for tools)                                          ##
##                                                                     ##
##    scripts                                                          ##
##                                                                     ##
##    docs                documentation                                ##
##                                                                     ##
##    for all targets x listed above there is a function clean_x.      ##
##    Exeptions are default, clean and targets containing new          ##
##                                                                     ##
#########################################################################
##                                                                     ##
##  directory structure                                                ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl       containes executables                  ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/none_parallel                                ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/none  objects with simple compiler flags     ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/fast_parallel                                ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/fast  optimized objects                      ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/prof_parallel                                ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/prof  objects for profiling                  ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/dbg_parallel                                 ##
##    /Users/ptpb/Tree/PAW/devel/bin/osx_gfortran_mkl/dbg   objects for debugging                  ##
##    /Users/ptpb/Tree/PAW/devel/src              contains sources for CP-PAW main code  ##
##    /Users/ptpb/Tree/PAW/devel/src/Tools        contains sources of Tools              ##
##    /Users/ptpb/Tree/PAW/devel/src/Buildtools/F90PP contains sources for preprocessor etc  ##
##    /Users/ptpb/Tree/PAW/devel/docs             containes documentation                ##
##                                                                     ##
#########################################################################
##  makefile basics
#########################################################################
## $(addprefix prefix,list) returns list with prefix prepended to each member
## $(addsuffix suffix,list) returns list with suffix appended to each member
## $@                       name of the current target
#########################################################################
#
#########################################################################
## The following block of variables is set by the configure            ##
## script (AC_SUBST @...@ and sed-Commands var...)                     ##
#########################################################################
#___________________________ root directory of the PAW distribution
# 
export THISDIR=/Users/ptpb/Tree/PAW/devel
#___________________________ the hardware architecture
export ARCH=osx_gfortran_mkl
#___________________________ the way to compile the tools 
export TOOLCOMP=fast
#___________________________ the GNU make utility
export MAKE=make
#___________________________ parallel environment?
export PENV=T
#
##########################################################################
#  complete shortcuts to directories etc
##########################################################################
#___________________________ path to the object files
export OBJDIR=${THISDIR}/bin/${ARCH}/Objects
#___________________________ path to the binaries
export BINDIR=${THISDIR}/bin/${ARCH}
#
OBJTYPES_SCALAR=fast dbg none prof
OBJTYPES_PRIMARY_SCALAR=fast
ifeq (${PENV},T)
  OBJTYPES=${OBJTYPES_SCALAR} $(addsuffix _parallel,${OBJTYPES_SCALAR}) 
  OBJTYPES_PRIMARY=${OBJTYPES_PRIMARY_SCALAR} $(addsuffix _parallel,${OBJTYPES_PRIMARY_SCALAR}) 
else
  OBJTYPES=${OBJTYPES_SCALAR} 
  OBJTYPES_PRIMARY=${OBJTYPES_PRIMARY_SCALAR} 
endif
#TOOLS=newatom atom tra wave grab dos preopt converttra cleantra strc tostrc \
#       toxyz 1davpot cmcwave lattice test
#Oldtools: lattice cmcwave (should be included later)
TOOLS=bands grab murnaghan dos dosplot polyhedra preopt stpa stpreport \
      strc tostrc toxyz tra converttra cleantra wave 1davpot 
SCRIPTS=scripts
TESTS=$(addsuffix _tests,${OBJTYPES})
#
#########################################################################
## Definition of the collective targets                                ##
#########################################################################
#
default: all_new
#
clean: clean_all
#
#---------------------------------------------------------------------------
#-- "all" makes a complete installation. all_new includes all dependencies
#---------------------------------------------------------------------------
all: ${OBJTYPES} tools 
#
all_new: $(addsuffix _new,${OBJTYPES}) tools 
#
clean_all: $(addprefix clean_,${OBJTYPES} tools)
#
#---------------------------------------------------------------------------
#-- "small" is directed to the production user. just makes the optimized  --
#-- executables, tools and documentation. Uses all dependencies           --
#---------------------------------------------------------------------------
#
small: $(addsuffix _new,${OBJTYPES_PRIMARY}) tools 
#
clean_small: $(addprefix clean_,${OBJTYPES_PRIMARY} tools)
#
#---------------------------------------------------------------------------
#-- make all tools in one go                                              --
#---------------------------------------------------------------------------
tools: ${TOOLS} ${SCRIPTS}
#
clean_tools: $(addprefix clean_,${TOOLS} ${SCRIPTS})
#
#---------------------------------------------------------------------------
#-- make tests                                                            --
#---------------------------------------------------------------------------
tests: ${TESTS}
#
${TESTS}:
	cd ${OBJDIR}/$(subst _tests,,$@); ${MAKE} $@
#
#########################################################################
## specific target rules                                               ##
#########################################################################
#
#---------------------------------------------------------------------------
#--          make binaries                                                --
#-- (none,fast,dbg,prof,none_parallel,fast_parallel,dbg_parallel,prof_parallel)
#---------------------------------------------------------------------------
${OBJTYPES}:
	ALLDEP=F; export ALLDEP; cd ${OBJDIR}/$@; ${MAKE} f90pp; ${MAKE} 
#
$(addsuffix _new,${OBJTYPES}):
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/$(subst _new,,$@); \
                                                        ${MAKE} f90pp; ${MAKE} 
#
$(addprefix clean_,${OBJTYPES}):
	cd ${OBJDIR}/$(subst clean_,,$@); ${MAKE} clean
#
#---------------------------------------------------------------------------
#--          PAW library libpaw.a                                         --
#---------------------------------------------------------------------------
lib:
	ALLDEP=F; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}/libpaw.a
#
lib_new:
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}/libpaw.a
#
#---------------------------------------------------------------------------
#--          PAW library libppaw.a                                       --
#---------------------------------------------------------------------------
lib_parallel:
	ALLDEP=F; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}_parallel; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}_parallel/libppaw.a
#
lib_new_parallel:
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}_parallel; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}_parallel/libppaw.a
#
#---------------------------------------------------------------------------
#--          PAW library libpawfull.a                                     --
#---------------------------------------------------------------------------
libfull:
	ALLDEP=F; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}/libpawfull.a EXCLUDE="${EXCLUDE}"
#
libfull_new:
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}/libpawfull.a EXCLUDE="${EXCLUDE}"
#
#---------------------------------------------------------------------------
#--          PAW library libppawfull.a                                    --
#---------------------------------------------------------------------------
libfull_parallel:
	ALLDEP=F; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}_parallel; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}_parallel/libppawfull.a EXCLUDE="${EXCLUDE}"
#
libfull_new_parallel:
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}_parallel; ${MAKE} -f Makefile ${OBJDIR}/${TOOLCOMP}_parallel/libppawfull.a EXCLUDE="${EXCLUDE}"
#
#---------------------------------------------------------------------------
#--          Tools and scripts                                            --
#---------------------------------------------------------------------------
${TOOLS}: lib_new
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}; ${MAKE} -f Makefile ${BINDIR}/paw_$@.x 
#
#---------------------------------------------------------------------------
#--          separate target for bands_parallel, which is the only tool,  --
#--          which is the only tool that can also be build as a parallel  --
#--          version                                                      --
#---------------------------------------------------------------------------
bands_parallel: lib_new_parallel
	ALLDEP=T; export ALLDEP; cd ${OBJDIR}/${TOOLCOMP}_parallel; ${MAKE} -f Makefile ${BINDIR}/ppaw_bands.x 
#
$(addprefix clean_,${TOOLS}):
	cd ${OBJDIR}/${TOOLCOMP}; ${MAKE} -f Makefile $@
#
scripts: 
	cp ${THISDIR}/src/Tools/Scripts/* ${BINDIR}
#
clean_scripts:
	rm -f ${BINDIR}/paw_show
	rm -f ${BINDIR}/paw_get
	rm -f ${BINDIR}/paw_collect
	rm -f ${BINDIR}/paw_resolve
	rm -f ${BINDIR}/paw_scan
	rm -f ${BINDIR}/paw_scanlat
	rm -f ${BINDIR}/paw_copy    
	rm -f ${BINDIR}/paw_ext_copy    
	rm -f ${BINDIR}/paw_mpeg_encode
#
#---------------------------------------------------------------------------
#--          Documentation                                                --
#---------------------------------------------------------------------------
docs: ${THISDIR}/doc/manual.pdf \
      ${THISDIR}/doc/history.pdf \
      ${THISDIR}/doc/install.pdf \
      ${THISDIR}/doc/style.pdf \
      ${THISDIR}/doc/Examples
#
${THISDIR}/doc/manual.pdf : ${THISDIR}/doc/manual.tex  \
                            ${THISDIR}/doc/doc.bib \
                            ${THISDIR}/doc/all.bib \
                            ${THISDIR}/doc/Figs
	cd ${THISDIR}/doc;  latex -halt-on-error manual
	cd ${THISDIR}/doc;  bibtex manual; \
                            makeindex manual.idx; \
                            latex manual;  \
                            latex manual; \
                            dvips manual.dvi; \
                            dvipdf manual.dvi
#
${THISDIR}/doc/history.pdf : ${THISDIR}/doc/history.tex \
                             ${THISDIR}/doc/doc.bib \
                             ${THISDIR}/doc/all.bib
	cd ${THISDIR}/doc; latex -halt-on-error history; 
	cd ${THISDIR}/doc; bibtex history; \
                           makeindex history.idx; \
                           latex history; \
                           latex history; \
                           dvips history.dvi; \
                           ps2pdf history.ps 
#
${THISDIR}/doc/install.pdf :  ${THISDIR}/doc/install.tex \
                              ${THISDIR}/doc/doc.bib \
                              ${THISDIR}/doc/all.bib
	cd ${THISDIR}/doc; latex -halt-on-error install
	cd ${THISDIR}/doc; bibtex install; \
                           makeindex install.idx; \
                           latex install; \
                           latex install; \
                           dvips install.dvi; \
                           ps2pdf install.ps
#
${THISDIR}/doc/style.pdf : ${THISDIR}/doc/style.tex \
                           ${THISDIR}/doc/doc.bib\
                           ${THISDIR}/doc/all.bib
	cd ${THISDIR}/doc; latex -halt-on-error style
	cd ${THISDIR}/doc; bibtex style; \
                           makeindex style.idx; \
                           latex style; \
                           latex style; \
                           dvips style.dvi; \
                           ps2pdf style.ps
#
#  dependencies for documentation
#
${THISDIR}/doc/manual.tex : ${THISDIR}/src/Docs/manual.tex 
	cp ${THISDIR}/src/Docs/manual.tex ${THISDIR}/doc/manual.tex
#
${THISDIR}/doc/install.tex :${THISDIR}/src/Docs/install.tex 
	cp ${THISDIR}/src/Docs/install.tex ${THISDIR}/doc/install.tex
#
${THISDIR}/doc/style.tex :${THISDIR}/src/Docs/style.tex 
	cp ${THISDIR}/src/Docs/style.tex ${THISDIR}/doc/style.tex
#
${THISDIR}/doc/history.tex :${THISDIR}/src/Docs/history.tex 
	cp ${THISDIR}/src/Docs/history.tex ${THISDIR}/doc/history.tex
#
${THISDIR}/doc/doc.bib :${THISDIR}/src/Docs/doc.bib
	cp ${THISDIR}/src/Docs/doc.bib ${THISDIR}/doc/doc.bib
#
${THISDIR}/doc/all.bib :${THISDIR}/src/Docs/all.bib
	cp ${THISDIR}/src/Docs/all.bib ${THISDIR}/doc/all.bib
#
${THISDIR}/doc/Figs :${THISDIR}/src/Docs/Figs
	cp -r ${THISDIR}/src/Docs/Figs ${THISDIR}/doc/
#
${THISDIR}/doc/Examples :${THISDIR}/src/Docs/Examples
	cp -r ${THISDIR}/src/Docs/Examples ${THISDIR}/doc/
#
clean_docs: 
	rm -f ${THISDIR}/doc/*
