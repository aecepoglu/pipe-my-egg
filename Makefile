MYEXE = _build/default/pipe2egg.exe
PREFIX = /usr/local/bin

$(MYEXE): *.ml
	dune build pipe2egg.exe

run: $(MYEXE)
	$(MYEXE)

clean:
	rm -rf _build/

install: $(MYEXE)
	cp _build/default/pipe2egg.exe $(PREFIX)/pomodoro

uninstall:
	rm $(PREFIX)/pomodoro
