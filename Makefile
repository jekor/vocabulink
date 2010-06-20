# Vocabulink site

SUBDIRS := vocabulink.cgi articles static
date := $(shell date +%Y-%m-%d)
sync_options := -avz --exclude 'cgi/dist' --exclude '*.muse' --exclude '*.ccss' --exclude 'articles/Makefile' --exclude '*.el' --exclude 'css/Makefile' --exclude 'js/Makefile' --exclude 'cgi/*.pdf' --exclude 'cgi/TAGS' --exclude '*.aux' --exclude '*.tex' --exclude '*.ptb' --exclude '*.log' --exclude '*.out' --exclude '._*' --exclude '.sass-cache' --delete articles css etc img js s scripts upload postgresql vocabulink.cgi linode:vocabulink/

.PHONY : $(SUBDIRS) all

all : $(SUBDIRS)
	@echo built

backup : backup-database

backup-database :
	pg_dump --host localhost --username vocabulink --create vocabulink | gzip > vocabulink--$(date).sql.gz

sync :
	rsync $(sync_options)

sync-test :
	rsync --dry-run $(sync_options)

$(SUBDIRS) :
	@-$(MAKE) -C $@
