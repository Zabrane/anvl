BUILD_DIR := $(CURDIR)/_build
CONCUERROR := $(BUILD_DIR)/Concuerror/bin/concuerror
CONCUERROR_RUN := $(CONCUERROR) \
	-x code -x code_server -x error_handler \
	--treat_as_normal shutdown --treat_as_normal normal \
	--pa $(BUILD_DIR)/concuerror+test/lib/snabbkaffe/ebin \
	--pa $(BUILD_DIR)/concuerror+test/lib/anvl_make/test \
  --pa $(BUILD_DIR)/concuerror+test/lib/anvl_make/ebin

.PHONY: all
all: doc build

.PHONY: build
build:
	rebar3 do dialyzer,eunit,escriptize

concuerror = $(CONCUERROR_RUN) -f $(BUILD_DIR)/concuerror+test/lib/anvl_make/test/concuerror_tests.beam -t $(1) || \
	{ cat concuerror_report.txt; exit 1; }

.PHONY: concuerror_test
concuerror_test: $(CONCUERROR)
	rebar3 as concuerror eunit
	$(call concuerror,parallel_test)
	$(call concuerror,dependency_test)

$(CONCUERROR):
	mkdir -p _build/
	cd _build && git clone https://github.com/parapluu/Concuerror.git
	$(MAKE) -C _build/Concuerror/

## Documentation
PICS=$(patsubst %.uml,%.png,$(wildcard apps/*/doc/*.uml))

.PHONY: doc
doc: $(PICS)
	rebar3 edoc

%.png: %.uml
	plantuml $<
