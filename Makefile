EMACS  ?= emacs
PREFIX ?= $(HOME)/.local
BIN     = $(PREFIX)/bin/finances

.PHONY: test test-batch test-verbose clean-test install uninstall

test: test-batch

test-batch:
	$(EMACS) -Q -batch \
	  -L . -L adapters -L domain -L tools -L view -L tests \
	  -l tests/all-tests.el \
	  -f ert-run-tests-batch-and-exit

test-verbose:
	$(EMACS) -Q -batch \
	  -L . -L adapters -L domain -L tools -L view -L tests \
	  --eval "(setq ert-batch-print-level 10 ert-batch-print-length 100)" \
	  -l tests/all-tests.el \
	  -f ert-run-tests-batch-and-exit

clean-test:
	rm -rf tests/.tmp

install:
	@mkdir -p $(dir $(BIN))
	@ln -sfn $(abspath bin/finances) $(BIN)
	@echo "linked $(BIN) -> $(abspath bin/finances)"

uninstall:
	@rm -f $(BIN)
	@echo "removed $(BIN)"
