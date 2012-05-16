tutorial.html: tutorial.lhs Makefile
	echo '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"'   > $@
	echo '          "http://www.w3.org/TR/html4/strict.dtd">' >> $@
	echo '<html><head><title>Formettes Tutorial</title><link rel="stylesheet" type="text/css" href="hscolour.css"></head><body>' >>  $@
#	cat $< | sed 's/^] /> /' | HsColour -partial -css -lit | markdown -html4tags >> $@
	cat $< | HsColour -partial -css -lit | markdown -html4tags >> $@
	echo '</body></html>'                                                        >> $@
	validate -w --verbose --emacs $@
	realpath $@

all: tutorial.html

clean:
	-rm -f tutorial.html

.PHONY: clean