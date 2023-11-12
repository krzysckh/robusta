FEATHER=/tmp/feather

.PHONY: doc doc/*

doc: doc/robusta.html
open-doc: doc
	firefox doc/robusta.html
doc/robusta.html: doc/robusta.md
	pandoc -s -f gfm -t html ./doc/robusta.md -o ./doc/robusta.html
doc/robusta.md: $(FEATHER)
	$(FEATHER) -o doc/robusta.md --title "(robusta)" `find ./robusta -type f` \
		doc/examples.md
$(FEATHER):
	wget -O $(FEATHER) https://gitlab.com/owl-lisp/owl/-/raw/master/bin/feather
	chmod +x $(FEATHER)