# Vocabulink site

SUBDIRS := vocabulink.cgi articles

.PHONY : $(SUBDIRS) all

all : $(SUBDIRS)
	@echo built

$(SUBDIRS):
	@-$(MAKE) -C $@
