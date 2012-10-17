I18N_ROOT		= .
I18N_MSG_FILE 	= $(if $(file),$(file),spec/messages.xml)

ERL_MODULES		= i18n_generate i18n_iso639 i18n_trans_server
ERL_SRC_FILES   = $(foreach p,$(ERL_MODULES),src/$(p).erl)

TARGETS			= $(ERL_SRC_FILES:src/%.erl=ebin/%.beam)

DEFAULT: all

include i18n.mk

all:
	@rebar -v compile

gen: $(I18N_TARGETS)
	@rebar -v compile

info:
	@echo "LANGUAGES 	= $(I18N_LANGUAGES)"
	@echo "MODULES   	= $(I18N_MODULES)"
	@echo "I18N_TARGETS	= $(I18N_TARGETS)"
	@echo "ERL_SRC   	= $(ERL_SRC_FILES)"

clean:
	rm -f $(I18N_TARGETS)
	@rebar clean

include:
	mkdir -p $@


