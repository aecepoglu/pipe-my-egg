MYEXE = _build/default/pipemyegg.exe
PREFIX = /usr/local/bin

$(MYEXE): *.ml
	dune build pipemyegg.exe

run: $(MYEXE)
	$(MYEXE)

clean:
	rm -rf _build/

install: $(MYEXE)
	cp _build/default/pipemyegg.exe $(PREFIX)/pomodoro

uninstall:
	rm $(PREFIX)/pomodoro
