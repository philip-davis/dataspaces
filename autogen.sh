aclocal -I config
autoconf
autoheader
libtoolize
automake --add-missing --copy
