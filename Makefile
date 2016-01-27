testwatch:
	fswatch src/*.elm test/*.elm | \
	while read f; do \
	    echo; echo; \
	    echo $$f; \
	    (cd test; elm make Test.elm --yes); \
	done
