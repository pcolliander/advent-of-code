
# repl:
# 	clojure -A:nrepl-server

repl:
	clj -m nrepl.cmdline \
		--middleware "[cider.nrepl/cider-middleware]" \
		--interactive	

run:
	tmux split-window -c ${PWD} -p 10 "make repl"
	tmux select-pane -U
