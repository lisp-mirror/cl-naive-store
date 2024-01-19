all: test

.PHONY: test tests test-load-systems test-run-tests run-tests-ccl run-tests-sbcl docs documentation

# default values:
ARTDIR = tests/artifacts/
DEPENDENCYDIR = $(abspath $(CURDIR)/..)/
THISDIR = $(abspath $(CURDIR))/


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

test tests: test-run-tests 


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
		--eval '(load #P"~/quicklisp/setup.lisp")' \
		--eval '(push "$(DEPENDENCYDIR)" ql:*local-project-directories*)' \
		--eval '(push #P"$(THISDIR)" asdf:*central-registry*)' \
		--eval '(ql:quickload :cl-naive-store.tests)' \
		--eval '(cl-naive-tests:run)' \
		--eval '(cl-naive-tests:write-results cl-naive-tests:*suites-results* :format :text)' \
		--eval '(cl-naive-tests:save-results cl-naive-tests:*suites-results* :file "$(ARTDIR)junit-results.xml" :format :junit)' \
		--eval '(sb-ext:exit :code (if (cl-naive-tests:report) 0 200))' \
		$(QUIT)

docs:documentation
documentation:
	@case "$(DEPENDENCYDIR)" in (/*) dd="$(DEPENDENCYDIR)" ;; (*) dd="../$(DEPENDENCYDIR)" ;; esac ; \
	make -C docs "DEPENDENCYDIR=$$dd" check-documentation all

clean:
	@ : make -C docs clean
	@ : for d in  $(EXAMPLES) ; do make -C examples/$$d clean ; done
	@ find . -name \*.fasl -exec rm {} +

FMT = "make %-20s \# %s\n"
help:
	@ printf $(FMT) help                'Print this help.'
	@ printf $(FMT) tests               'Run all the tests:'
	@ printf $(FMT) test-run-tests      'Run the test properly.'
	@ printf $(FMT) documentation       'Build the documentation.'
	@ printf $(FMT) clean               'Remove old products.'
