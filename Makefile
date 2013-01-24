PLANET_MAJOR=1
PLANET_MINOR=0
PLANET_VERSION=$(PLANET_MAJOR).$(PLANET_MINOR)
PLANET_NAME=nat-pmp

all:

$(PLANET_NAME).plt: clean
	mkdir planet-build-temp
	(cd planet-build-temp; git clone .. $(PLANET_NAME))
	(cd planet-build-temp/$(PLANET_NAME); git checkout $(PLANET_NAME).plt-${PLANET_VERSION})
	(cd planet-build-temp; raco planet create $(PLANET_NAME))
	mv planet-build-temp/$(PLANET_NAME).plt .
	rm -rf planet-build-temp

manual.html: manual.scrbl
	raco scribble $<

clean:
	rm -f manual.html racket.css scribble-common.js scribble-style.css scribble.css
	rm -rf planet-docs
	rm -f $(PLANET_NAME).plt

link:
	raco planet link tonyg $(PLANET_NAME).plt $(PLANET_MAJOR) $(PLANET_MINOR) $(CURDIR)

unlink:
	raco planet unlink tonyg $(PLANET_NAME).plt $(PLANET_MAJOR) $(PLANET_MINOR)
