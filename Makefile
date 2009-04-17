# Vocabulink site

SUBDIRS := vocabulink.cgi articles static
date:= $(shell date +%Y-%m-%d)

.PHONY : $(SUBDIRS) all

all : $(SUBDIRS)
	@echo built

backup : backup-database

backup-database :
	pg_dump --host localhost --username vocabulink --create vocabulink | gzip > vocabulink--$(date).sql.gz

$(SUBDIRS) :
	@-$(MAKE) -C $@
