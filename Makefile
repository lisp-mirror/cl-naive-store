all: test

.PHONY: test tests test-load-systems test-run-tests run-tests-ccl run-tests-sbcl

CCL_LISP = ccl  --batch --quiet --no-init
CCL_QUIT = --eval '(ccl:quit)'

SBCL_LISP = sbcl --non-interactive --no-userinit
SBCL_QUIT = --quit

# NAME=ccl
# LISP=$(CCL_LISP)
# QUIT=$(CCL_QUIT)

NAME ?= sbcl
LISP ?= $(SBCL_LISP)
QUIT ?= $(SBCL_QUIT)

test tests: test-run-tests test-load-systems

test-load-systems:
	load_system=load_system_in_$(NAME) tests/test-load-systems

run-tests-ccl:  NAME=ccl
run-tests-ccl:  LISP=$(CCL_LISP)
run-tests-ccl:  QUIT=$(CCL_QUIT)
run-tests-ccl:  test-run-tests

run-tests-sbcl: NAME=sbcl
run-tests-sbcl: LISP=$(SBCL_LISP)
run-tests-sbcl: QUIT=$(SBCL_QUIT)
run-tests-sbcl: test-run-tests

test-run-tests:
	$(LISP) \
		--eval '(load "~/quicklisp/setup.lisp")' \
		--eval '(push #P"'"$$(pwd)"'/" asdf:*central-registry*)' \
		--eval '(setf *default-pathname-defaults* #P"'"$$(pwd)"'/tests/")' \
		--eval '(ql:quickload :cl-naive-store)' \
		--eval '(asdf:oos (quote asdf:test-op) :cl-naive-store.test)' \
		$(QUIT)
