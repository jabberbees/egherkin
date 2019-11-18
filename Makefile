PROJECT = egherkin

PROJECT_DESCRIPTION = Gherkin parser for Erlang

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

include ct-helpers.mk
