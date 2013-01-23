weberl
======

An web-console to erlang VM

Try it out
* rebar get-deps
* rebar compile
* ./start.sh

Starts at http://localhost:8080/index.html

Not supported in this version -
- the '\t' for autocompletion (its not supported by a terminal erl executable)
- the cursor is free to move anywhere in the console might break if typed in a wrong place
- 'up' and 'dwon' arrows to browse through history is not supported
- IMPORTANT: after executing 'q().' you are on your own (refreshing browser might help).

Browser Support -
- IE 10
- Chrome
