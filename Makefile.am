## File used to generate Makefile.in
EXTRA_DIST=autogen.sh scripts
SUBDIRS=dart src tests

dist_bin_SCRIPTS = dspaces_config

do_subst = sed -e 's|%incdir[%]|$(includedir)|g' \
        -e 's|%libdir[%]|$(libdir)|g'

dspaces_config: scripts/dspaces_config.makesrc
	$(do_subst) < scripts/dspaces_config.makesrc > dspaces_config
	chmod +x dspaces_config
