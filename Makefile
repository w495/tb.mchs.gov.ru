SUBDIRS	=	mochiweb sablotron-1.0 web 
DISTRIB = .distrib
TARBALL = "`date +admin_%F`.tar.bz2"

release:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Building RELEASE version $$d ===============" && \
	        cd $$d && \
	        $(MAKE) --no-print-directory $@ ) || exit 1 ; fi ; \
	  done

debug:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Building DEBUG version $$d ===============" && \
	        cd $$d && \
	        $(MAKE) debug --no-print-directory $@ ) || exit 1 ; fi ; \
	  done

clean:
	@set -e ; \
	  for d in $(SUBDIRS) ; do \
	    if [ -f $$d/Makefile ]; then \
	      ( echo "\n=============== Cleaning $$d ===============" && \
	        cd $$d && \
	        $(MAKE) clean --no-print-directory $@ ) || exit 1 ; fi ; \
	  done

install:
	@set -e ; \
	./Einstall

distrib: dirs
	@set -e ; \
	echo "\n== Building distrib =="; \
	if [ -f ctl ]; then \
	  cp ctl $(DISTRIB) ; \
	fi; \
	if [ -f start.sh ]; then \
	  cp start.sh $(DISTRIB) ; \
	fi; \
	for d in $(SUBDIRS) ; do \
	  cp -r $$d/ebin   $(DISTRIB)/$$d ; \
	  cp -r $$d/priv   $(DISTRIB)/$$d ; \
	  if [ -f $$d/app.config ]; then \
	    cp $$d/app.config $(DISTRIB)/$$d ; \
	  fi; \
	  if [ -f $$d/ctl ]; then \
	    cp $$d/ctl $(DISTRIB)/$$d ; \
	  fi; \
	  if [ -f $$d/start.sh ]; then \
	    cp $$d/start.sh $(DISTRIB)/$$d ; \
	  fi; \
	  if [ -d $$d/deps ]; then \
	    cp -r $$d/deps $(DISTRIB)/$$d ; \
	  fi; \
	  if [ -d $$d/js/build ]; then \
	    cp -r $$d/js/build $(DISTRIB)/$$d/js ; \
	  fi; \
	done ; \
	if [ -f $(TARBALL) ]; then \
	  rm -rf $(TARBALL); \
	fi; \
	cd $(DISTRIB); \
	echo "branch: `git branch | head -n 1 | awk '{print $$2}'`">CURRENT_VERSION; \
	echo "commit: `git log -1 --pretty=format:"%H - %an, %ad : %s"`">>CURRENT_VERSION; \
	find . -name *.log  -print | xargs /bin/rm -f; \
	find . -name .keep  -print | xargs /bin/rm -f; \
	find . -name *.dump -print | xargs /bin/rm -f; \
	find . -name .svn   -print | xargs /bin/rm -rf; \
	find . -name .gitignore  -print | xargs /bin/rm -f; \
	echo "== Building distrib tarball $(TARBALL) =="; \
	tar cjf $(TARBALL) $(SUBDIRS) ctl CURRENT_VERSION; \
  mv $(TARBALL) ..


dirs:
	@set -e ; \
	rm -rf $(DISTRIB); \
	for d in $(SUBDIRS) ; do \
	  mkdir -p $(DISTRIB)/$$d/ebin; \
	  mkdir -p $(DISTRIB)/$$d/priv/logs; \
	  if [ -d $$d/db ]; then \
	    mkdir -p $(DISTRIB)/$$d/db ; \
	  fi; \
	  if [ -d $$d/data ]; then \
	    mkdir -p $(DISTRIB)/$$d/data ; \
	  fi; \
	  if [ -d $$d/deps ]; then \
	    mkdir -p $(DISTRIB)/$$d/deps ; \
	  fi; \
	  if [ -d $$d/js/build ]; then \
	    mkdir -p $(DISTRIB)/$$d/js/build ; \
	  fi; \
	  if [ -d $$d/priv/tracker_http_log ]; then \
	    mkdir -p $(DISTRIB)/$$d/priv/tracker_http_log ; \
	  fi; \
	  if [ -d $$d/priv/lib ]; then \
	    mkdir -p $(DISTRIB)/$$d/priv/lib ; \
	  fi; \
	done
