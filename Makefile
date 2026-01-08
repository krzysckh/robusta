FEATHER=/tmp/feather

.PHONY: doc doc/* pubcpy

doc: doc/robusta.html doc/robusta.pdf
open-doc: doc
	firefox doc/robusta.html
doc/robusta.html: doc/robusta.md
	pandoc --toc -s -f gfm -t html ./doc/robusta.md -o ./doc/robusta.html
doc/robusta.pdf: doc/robusta.md
	pandoc -s -f gfm -t pdf ./doc/robusta.md -o ./doc/robusta.pdf \
		--pdf-engine=pdfroff
doc/robusta.md: $(FEATHER)
	$(FEATHER) -s -o doc/robusta.md --title "(robusta)" \
		doc/prelude.md \
		`find ./robusta -type f -iname '*.scm'` \
		doc/examples.md
$(FEATHER):
	wget -O $(FEATHER) https://gitlab.com/owl-lisp/owl/-/raw/master/bin/feather
	chmod +x $(FEATHER)
pubcpy: doc
	cd doc && yes | pubcpy robusta.html
clean:
	rm -f doc/robusta.md doc/robusta.html doc/robusta.pdf
