.PHONY: test

EMACS = $(shell which emacs)

test:
	${EMACS} -Q -L $(PWD) -l ert -l elfirm-tests.el -f ert-run-tests-batch-and-exit
