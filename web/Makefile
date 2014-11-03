# default

release:
	$(call make_c_code); \
	$(call make_ebin); \
	$(call make_js,$(1),$(2),build);


debug:
	$(call make_c_code); \
	(cd src && $(MAKE) debug) || exit 1; \
	(cd js && ./generate.py source) || exit 1

clean:
	(cd src;$(MAKE) clean)

make_c_code = \
	g++ -o cbin/libxslt_adapter csrc/libxslt_adapter.c -I/usr/include/libxml2 -lxml2 -lxslt
#	gcc -o cbin/libxslt_adapter csrc/libxslt_adapter.c -I/usr/include/libxml2 -lxml2 -lxslt

make_ebin =  \
	(cd src && $(MAKE) $(1)) || exit 1

make_js =                                                \
	(cd js &&                                              \
	 ./generate.py $(3)) || exit 1
