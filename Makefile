MYEXE = _build/default/cliapp/pipemyegg.exe
WEBDEMO = _build/default/webdemo/code.bc.js
PREFIX = /usr/local/bin

$(MYEXE): cliapp/* lib/*
	dune build cliapp/pipemyegg.exe

$(WEBDEMO): webdemo/* lib/*
	dune build webdemo/code.bc.js
	mkdir -p docs/
	cp $(WEBDEMO) docs/
	cp webdemo/index.html docs/

run: $(MYEXE)
	$(MYEXE)

clean:
	rm -rf _build/

install: $(MYEXE)
	cp _build/default/pipemyegg.exe $(PREFIX)/pomodoro

uninstall:
	rm $(PREFIX)/pomodoro
