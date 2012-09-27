all:
	gprbuild -p -k -P cairoada_tests.gpr
	export LIBRARY_TYPE=relocatable; export LIBRARY_KIND=relocatable; gprbuild -p -k -P cairoada_tests.gpr

cairoada:
	gprbuild -p -k -P cairoada.gpr
	export LIBRARY_KIND=relocatable; gprbuild -p -k -P cairoada.gpr

cairoada_rsvg:
	gprbuild -p -k -P cairoada_rsvg.gpr
	export LIBRARY_TYPE=relocatable; export LIBRARY_KIND=relocatable; gprbuild -p -k -P cairoada_rsvg.gpr

cairoada_gdk:
	gprbuild -p -k -P cairoada_gdk.gpr
	export LIBRARY_TYPE=relocatable; export LIBRARY_KIND=relocatable; gprbuild -p -k -P cairoada_gdk.gpr

doc: all
	doc/genhtml.sh

clean-doc:
	rm -fr doc/html

clean-tests:
	rm -f tests/test-*

clean: clean-tests clean-doc
	gprclean -P cairoada_tests.gpr

tests: all
	@cd tests; echo "================================"; ../bin/cairo_test_snippets;
	@cd tests; echo "================================"; ../bin/cairo_test_user_fonts;
	@cd tests; echo "================================"; ../bin/cairo_gdk_main

snapshot:
	utils/gensnapshot.sh
