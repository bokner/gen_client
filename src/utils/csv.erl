-module(csv).
-import(lists, [reverse/1]).
-export([parse_file/3,parse_file/1,parse/3,parse/1]).

-record(ecsv,{
state = field_start, %%field_start|normal|quoted|post_quoted
cols = undefined, %%how many fields per record
current_field = [],
current_record = [],
fold_state,
fold_fun %%user supplied fold function
}).

%%
parse_file(FileName,InitialState,Fun) ->
{ok, Binary} = file:read_file(FileName),
parse(Binary,InitialState,Fun).

parse_file(FileName) ->
{ok, Binary} = file:read_file(FileName),
parse(Binary).

parse(Binary) ->
R = parse(Binary,[],fun(Fold,Record) -> [Record|Fold] end),
lists:reverse(R).

parse(Binary,InitialState,Fun) ->
do_parse(Binary,#ecsv{fold_state=InitialState,fold_fun=Fun}).

%% Field_start state
%%whitespace, loop in field_start state
do_parse(<<32, Rest/binary>>,S = #ecsv{state=field_start,current_field=Field})->
do_parse(Rest,S#ecsv{current_field=[Field]});

%%its a quoted field, discard previous whitespaces
do_parse(<<$",Rest/binary>>,S = #ecsv{state=field_start})->
do_parse(Rest,S#ecsv{state=quoted,current_field=[]});

%%anything else, is a unquoted field
do_parse(Bin,S = #ecsv{state=field_start})->
do_parse(Bin,S#ecsv{state=normal});

%% Quoted state
%%Escaped quote inside a quoted field
do_parse(<<$",$",Rest/binary>>,S = #ecsv{state=quoted,current_field=Field})->
do_parse(Rest,S#ecsv{current_field=[$"|Field]});

%%End of quoted field
do_parse(<<$",Rest/binary>>,S = #ecsv{state=quoted})->
do_parse(Rest,S#ecsv{state=post_quoted});

%%Anything else inside a quoted field
do_parse(<<X,Rest/binary>>,S = #ecsv{state=quoted,current_field=Field})->
do_parse(Rest,S#ecsv{current_field=[X|Field]});

do_parse(<<>>, #ecsv{state=quoted})->
throw({ecsv_exception,unclosed_quote});

%% -- Post_quoted state -------
%%consume whitespaces after a quoted field
do_parse(<<32,Rest/binary>>,S = #ecsv{state=post_quoted})->
do_parse(Rest,S);

%%---Comma and New line handling. ------
%%---Common code for post_quoted and normal state

%%EOF in a new line, return the records
do_parse(<<>>, #ecsv{current_record=[],fold_state=State})->
State;

%%EOF in the last line, add the last record and continue
do_parse(<<>>,S)->
do_parse(<<>>,new_record(S));

%% new record windows
do_parse(<<13, 10, Rest/binary>>, S = #ecsv{}) ->
do_parse(Rest, new_record(S));

%% new record Unix
do_parse(<<13, Rest/binary>>,S = #ecsv{}) ->
do_parse(Rest,new_record(S));

do_parse(<<10, Rest/binary>>,S = #ecsv{}) ->
	do_parse(Rest,new_record(S));

do_parse(<<$, ,Rest/binary>>,S = #ecsv{current_field=Field,current_record=Record})->
do_parse(Rest,S#ecsv{state=field_start,
current_field=[],
current_record=[lists:reverse(Field)|Record]});


%%Accumulate Field value
do_parse(<<X, Rest/binary>>,S = #ecsv{state=normal,current_field=Field})->
do_parse(Rest,S#ecsv{current_field=[X|Field]}).

%%check the record size against the previous, and actualize state.
new_record(S=#ecsv{cols=Cols,current_field=Field,current_record=Record,fold_state=State,fold_fun=Fun}) ->
NewRecord = lists:reverse([lists:reverse(Field)|Record]),
if
(length(NewRecord) =:= Cols) or (Cols =:= undefined) ->
NewState = Fun(State,NewRecord),
S#ecsv{state=field_start,cols=length(NewRecord),
current_record=[],current_field=[],fold_state=NewState};

(length(NewRecord) =/= Cols) ->
throw({ecsv_exception,bad_record_size})
end.
