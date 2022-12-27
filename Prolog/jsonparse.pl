%% entrypoint atom-to-string
jsonparse(AtomJSON, ParsedObject) :-
    atom(AtomJSON), !,
    atom_string(AtomJSON, StringJSON),
    jsonparse(StringJSON, ParsedObject).

%% entrypoint string-to-term + normalization
jsonparse(StringJSON, ParsedObject) :-
    string(StringJSON), !,
    normalize_space(string(NormalizedStringJSON), StringJSON),
    term_string(TermJSON, NormalizedStringJSON),
    jsonparse(TermJSON, ParsedObject).

%% parsing JSON terms
jsonparse(JSON, jsonobj(ParsedObject)) :-
    jsonobj(JSON, ParsedObject), !.

jsonparse(JSON, jsonarray(ParsedArray)) :-
    jsonarray(JSON, ParsedArray), !.

%% DEFINING PRIMITIVES
boolean(true).
boolean(false).

jsonprimitive(P, P) :- boolean(P), !.
jsonprimitive(P, P) :- string(P), !.
jsonprimitive(P, P) :- number(P), !.
jsonprimitive(P, ParsedP) :- 
    jsonparse(P, ParsedP), !.

jsonpair(Key:Value, (Key,ParsedValue)) :-
    string(Key),
    jsonprimitive(Value, ParsedValue).

jsonarray([],[]) :- !.
jsonarray([Head | Entries], [ParsedHead | ParsedEntries]) :-
    jsonprimitive(Head, ParsedHead),
    jsonparse(Entries, jsonarray(ParsedEntries)).

jsonobj({},[]) :- !.
jsonobj({Object}, [ParsedMember | ParsedMoreMember]) :- 
    Object =.. [',', Member, MoreMember],   % desctructuring object keypairs
    jsonpair(Member, ParsedMember),
    jsonparse({MoreMember}, jsonobj(ParsedMoreMember)), !.
jsonobj({Pair}, [ParsedPair]) :-
    jsonpair(Pair, ParsedPair).



/*  DEMOS:
jsonparse({"key1":123, "key2":"pippo", "key3":true}, X).
 X = jsonobj(["key1":123, "key2":"pippo", "key3":true]).

jsonparse({"key1":{"key1.1":false}, "key2":"pippo", "key3":["a", "b", 3, 4]}, X).


*/
