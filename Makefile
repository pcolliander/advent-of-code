
# repl:
# 	clojure -A:nrepl-server

repl:
	clj -m nrepl.cmdline \
		--middleware "[cider.nrepl/cider-middleware]" \
		--interactive	
