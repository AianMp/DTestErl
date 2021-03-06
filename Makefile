ERL = /usr/bin/erl
APPS = kernel stdlib erts eunit
COMBO_PLT = $(HOME)/.dialyzer_plt

all: compile shell

compile: clean
	mkdir ./ebin
	$(ERL) -make

shell:  
	gnome-terminal -x $(ERL) -sname nodeA -setcookie clave -run dteste_peer start -pa ./ebin
	gnome-terminal -x $(ERL) -sname nodeB -setcookie clave -run dteste_peer start -pa ./ebin
	gnome-terminal -x $(ERL) -sname nodeC -setcookie clave -run dteste_peer start -pa ./ebin
	gnome-terminal -x $(ERL) -sname nodeD -setcookie clave -run dteste_initEscenario start -pa ./ebin

clean:
	rm -rf ebin/
	rm -rf doc/*.html
	rm -rf doc/edoc-info doc/erlang.png doc/stylesheet.css

edoc:
	$(ERL) -noinput -eval 'edoc:application(examples, "./", [{doc, "doc/"}, {files, "src/"}])' -s erlang halt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS)

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) ebin
