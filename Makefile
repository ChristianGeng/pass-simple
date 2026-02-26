.PHONY: test lint

test:
	emacs -Q --batch \
	  -L . \
	  -L tests \
	  -l tests/run-tests.el

lint:
	emacs -Q --batch \
	  -L . \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(byte-compile-file \"pass-simple.el\")"
