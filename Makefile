CASK ?= cask
EMACS ?= emacs
ELS = nerd-fonts.el
ELCS = nerd-fonts.elc
TEST_ELS = \
	test/test-helper.el \
	test/nerd-fonts-test.el \
	test/nerd-fonts-helm-test.el \
	test/nerd-fonts-ivy-test.el

# If the first argument is "test"...
ifeq (test, $(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SELECTOR):;@:)
endif

all: clean compile test

%.elc:%.el
	$(EMACS) -batch -L . -f batch-byte-compile $(ELS)

compile:$(ELCS)

.PHONY: test

test:$(ELCS)
ifeq ($(SELECTOR),)
	$(EMACS) -Q --batch -L . $(addprefix -l , $(ELCS) $(TEST_ELS)) -f ert-run-tests-batch-and-exit
else
	$(EMACS) -Q --batch -L . $(addprefix -l , $(ELCS) $(TEST_ELS)) --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"
endif

help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make clean

clean:
	@rm -f *.elc
