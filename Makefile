XSLTPROC  		= xsltproc
MESSAGES_FILE 	= $(if $(file),$(file),spec/messages.xml)

LANGUAGES		= $(shell $(XSLTPROC) --stringparam lang 1    spec/i18n_languages.xsl $(MESSAGES_FILE))
MODULES			= $(shell $(XSLTPROC) --stringparam modules 1 spec/i18n_languages.xsl $(MESSAGES_FILE))
NOW				= $(shell date +'%Y-%m-%d %T')

GENERATED_FILES	= $(MODULES:%=src/%.erl)
ERL_MODULES		= i18n_generate i18n_iso639 i18n_trans_server
ERL_SRC_FILES   = $(foreach p,$(ERL_MODULES),src/$(p).erl)

GEN_TARGETS		= include/i18n_constants.hrl src/i18n.erl $(GENERATED_FILES)
TARGETS			= $(ERL_SRC_FILES:src/%.erl=ebin/%.beam)

all:
	@rebar -v compile

gen: $(GEN_TARGETS)
	@rebar -v compile

info:
	@echo "LANGUAGES = $(LANGUAGES)"
	@echo "MODULES   = $(MODULES)"
	@echo "ERL_SRC   = $(ERL_SRC_FILES)"

clean:
	rm -f $(GEN_TARGETS)
	@rebar clean

include:
	mkdir -p $@

include/i18n_constants.hrl: $(MESSAGES_FILE) spec/i18n_constants_hrl.xsl include
	@echo "Applying $(word 2,$^)  to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(NOW)" \
		$(word 2,$^) $< > $@

src/i18n.erl: $(MESSAGES_FILE) spec/i18n_trans_erl.xsl
	@echo "Applying $(word 2,$^)      to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(NOW)" \
		$(word 2,$^) $< > $@

src/i18n_trans_%.erl: $(MESSAGES_FILE) spec/i18n_trans_lang_erl.xsl
	@echo "Applying $(word 2,$^) to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(NOW)" \
		--stringparam lang $(patsubst src/i18n_trans_%.erl,%,$@) \
		$(word 2,$^) $< > $@

src/%.erl: $(MESSAGES_FILE) spec/i18n_trans_lang_erl.xsl
	@echo "Applying $(word 2,$^) to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(NOW)" \
		$(word 2,$^) $< > $@

