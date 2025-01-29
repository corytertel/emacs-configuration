
emacs:
	cp --verbose *.el ~/.emacs.d/

clean:
	rm --verbose ~/.emacs.d/*.el 2>/dev/null

