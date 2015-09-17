#
# Module resolution for EcmaScript 6
# (c) 2012 Andreas Rossberg
#

NAME = jsmodules
MODULES = \
  source syntax parser lexer \
  types resolve translate \
  main
NOMLI = syntax main
PARSERS = parser
LEXERS = lexer
TESTJS = test external

MLS = $(MODULES:%=%.ml)
MLIS = $(filter-out $(NOMLI:%=%.mli), $(MODULES:%=%.mli))
MLYS = $(PARSERS:%=%.mly)
MLLS = $(LEXERS:%=%.mll)
CMOS = $(MODULES:%=%.cmo)
CMXS = $(MODULES:%=%.cmx)
JSS = $(TESTJS:%=%.js)

$(NAME): $(CMOS) Makefile
	ocamlc $(CMOS) -g -o $@

$(NAME).opt: $(CMXS) Makefile
	ocamlopt $(CMXS) -o $@

$(filter-out $(NOMLI:%=%.cmo), $(CMOS)): %.cmo: %.cmi
$(filter-out $(NOMLI:%=%.cmx), $(CMXS)): %.cmx: %.cmi

Makefile.depend: $(MLS) $(MLIS) Makefile
	ocamldep $^ >$@

-include Makefile.depend

zip: $(MLS) $(MLIS) $(MLYS) $(MLLS) $(JSS) Makefile
	mkdir tmp tmp/$(NAME)
	cp $^ tmp/$(NAME)
	rm -f $(NAME).zip
	(cd tmp; zip -r ../$(NAME).zip $(NAME))
	rm -r tmp

clean:
	rm -f *.cmi *.cmo *.cmx *.o *.output *.depend
	rm -f *.native *.byte $(NAME) $(NAME).opt $(NAME).zip
	rm -f *~

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c -g $<

%.cmx: %.ml
	ocamlopt -c $<

%.ml: %.mly
	ocamlyacc -v $<

%.mli: %.mly
	ocamlyacc -v $<

%.ml: %.mll
	ocamllex $<

.PRECIOUS: %.ml %.mli
