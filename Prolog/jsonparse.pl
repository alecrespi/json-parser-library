%%%% MODULES
:- use_module(library(readutil)).
%%%%

%%%  JSONPARSE/2
jsonparse({}, jsonobj([])) :- !.
jsonparse([], jsonarray([])) :- !.

%% entrypoint atom-to-string
jsonparse(AtomJSON, ParsedObject) :-
    atom(AtomJSON), !,
    not(jsonatom(AtomJSON)),
    atom_string(AtomJSON, StringJSON),
    jsonparse(StringJSON, ParsedObject).

%% entrypoint string-to-term + normalization
jsonparse(StringJSON, ParsedObject) :-
    string(StringJSON), !,
    normalize_space(string(NormalizedStringJSON), StringJSON),
    catch(term_string(TermJSON, NormalizedStringJSON), _, false),
    % term_string(TermJSON, NormalizedStringJSON),
    jsonparse(TermJSON, ParsedObject).

%% parsing JSON terms
jsonparse(JSON, jsonobj(ParsedObject)) :-
    jsonobj(JSON, ParsedObject), !.

jsonparse(JSON, jsonarray(ParsedArray)) :-
    jsonarray(JSON, ParsedArray), !.


%%% JSONACCESS/3
 
jsonparsable(jsonobj(JSON)) :-
    jsonobj(_,JSON), !.
jsonparsable(jsonarray(JSON)) :-
    jsonarray(_,JSON), !.
jsonparsable(JSON) :-
    jsonatom(JSON), !.

% jsonaccess(jsonarray(_), [], _) :- !, false.
jsonaccess(JSON, [], JSON) :- 
    jsonparsable(JSON), !.

% handle objects
jsonaccess(jsonobj(Object), [Key | Pattern], Result) :-
    string(Key),
    memberchk((Key, Content), Object),
    jsonaccess(Content, Pattern, Result),
    !.

%% handle arrays
jsonaccess(jsonarray(Array), [Index | Pattern], Result) :-
    number(Index),
    nth0(Index, Array, Content),
    jsonaccess(Content, Pattern, Result),
    !.

%% handle one-step-pattern
jsonaccess(JSON, OneStepPattern, Result) :-
    not(list(OneStepPattern)),
    jsonaccess(JSON, [OneStepPattern], Result).



%%%  JSONREAD/2  
%% jsonread(+Filename, -JSON)
jsonread(FileName, JSON) :-
    var(JSON),
    nonvar(FileName),
    access_file(FileName, read),
    read_file_to_string(FileName, Unparsed, []),
    jsonparse(Unparsed, JSON).




%%%  JSONDUMP/2
%% jsondump(+JSON, +FileName)
jsondump(JSON, FileName) :-
    nonvar(JSON),
    nonvar(FileName),
    access_file(FileName, write),
    jsonparse(RealJSON, JSON),
    term_string(RealJSON, StringifiedJSON),
    open(FileName, write, OutputStream, []),
    write(OutputStream, StringifiedJSON),
    close(OutputStream).






%%% <PRIMITIVES> 
boolean(false).
boolean(true).
nullable(null).

jsonatom(P) :- nullable(P), !.
jsonatom(P) :- number(P), !.
jsonatom(P) :- string(P), !.
jsonatom(P) :- boolean(P), !.

jsonprimitive(P, P) :- 
    jsonatom(P), !.
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




%%% UTILS
list([_ | _]).
list([]).

/* 
UTILS:
print all output : (BUG: string delimiter ' " ' will no more printed')
    set_prolog_flag(answer_write_options,[max_depth(0)]).
*/


%%% DEMOS
% jsonread("./testing/input.json", J), jsondump(J, "./testing/output.json").
% jsonread("./testing/input.json", J), jsondump(J, "./testing/output.json"), jsonread("./testing/input.json", J).