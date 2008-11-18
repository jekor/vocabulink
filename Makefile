# Vocabulink site

SUBDIRS := vocabulink.cgi articles static

.PHONY : $(SUBDIRS) all

all : $(SUBDIRS)
	@echo built

$(SUBDIRS):
	@-$(MAKE) -C $@
