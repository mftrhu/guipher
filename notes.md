Somehow handle malformed gophermaps

Sometimes - noticeable on some gopherhole linked by tomasino's - the gophermap does not include tabs when the line is an info line (that is, starting with 'i'), which triggers my error handling routines and renders the whole menu as text.

Now, this might be ok - **somewhat** when dealing with gophernicus' default settings (return a text page) - but it messes with the layout something fierce otherwise.

Proposal:
- shunt the page rendering over to text only when the line that triggered the error does not start with `i`
- possibly make this configurable/shunt it to a handler function that will examine the given line
- but that will require a rewrite using a let loop form, as I'll need to possibly escape from the loop early **or** continue :/
