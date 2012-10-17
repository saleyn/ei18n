XSLTPROC	   ?= xsltproc
I18N_LANGUAGES	= $(shell $(XSLTPROC) --stringparam lang 1 \
 					$(I18N_ROOT)/spec/i18n_languages.xsl $(I18N_MSG_FILE))
I18N_MODULES	= $(shell $(XSLTPROC) --stringparam modules 1 \
					$(I18N_ROOT)/spec/i18n_languages.xsl $(I18N_MSG_FILE))
I18N_LANG_FILES	= $(I18N_MODULES:%=src/%.erl)
I18N_TARGETS 	= include/i18n.hrl src/i18n.erl $(I18N_LANG_FILES)
I18N_NOW		= $(shell date +'%Y-%m-%d %T')


include/i18n.hrl: $(I18N_MSG_FILE) $(I18N_ROOT)/spec/i18n_constants_hrl.xsl include
	@echo "Applying $(word 2,$^)  to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(I18N_NOW)" \
		$(word 2,$^) $< > $@

src/i18n.erl: $(I18N_MSG_FILE) $(I18N_ROOT)/spec/i18n_trans_erl.xsl
	@echo "Applying $(word 2,$^)      to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(I18N_NOW)" \
		$(word 2,$^) $< > $@

src/i18n_trans_%.erl: $(I18N_MSG_FILE) $(I18N_ROOT)/spec/i18n_trans_lang_erl.xsl
	@echo "Applying $(word 2,$^) to $< -> $@"
	@$(XSLTPROC) --stringparam user "$(USERNAME)" \
        --stringparam email "$(shell git config --global --get user.email)" \
        --stringparam xslfile "$(notdir $(word 2,$^))" \
		--stringparam xmlfile "$(notdir $<)" \
        --stringparam now "$(I18N_NOW)" \
		--stringparam lang $(patsubst src/i18n_trans_%.erl,%,$@) \
		$(word 2,$^) $< > $@
