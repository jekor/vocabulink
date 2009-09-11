# Vocabulink site

SUBDIRS := vocabulink.cgi articles static
date:= $(shell date +%Y-%m-%d)

.PHONY : $(SUBDIRS) all

all : $(SUBDIRS)
	@echo built

backup : backup-database

backup-database :
	pg_dump --host localhost --username vocabulink --create vocabulink | gzip > vocabulink--$(date).sql.gz

sync :
	rsync -avzk --exclude 'graphics' --exclude 's/icons' --exclude 's/pack/image/' --exclude 'vocabulink.cgi/vocabulink.cgi' --exclude 'tests' --exclude '*.hi' --exclude '*.o' --exclude '*.pdf' --exclude '*.hi-boot' --exclude '.o-boot' --exclude 'vocabulink--*.gz' --exclude 'vocabulink--*.gpg' --exclude 'dist/' --delete . chris@efektiva:vocabulink/

$(SUBDIRS) :
	@-$(MAKE) -C $@
