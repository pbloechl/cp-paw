SUBDIRS = $(wildcard */.)
     
.PHONY: all $(SUBDIRS)
     
all: $(SUBDIRS)
     
$(SUBDIRS):
	for dir in $(SUBDIRS); do \
	  cd $$dir; \
	  $(MAKE); \
	  cd ..; \
	done
