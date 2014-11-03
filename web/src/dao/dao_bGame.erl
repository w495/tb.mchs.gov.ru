%%% @file dao_bGame.erl
%%%
%%%     Администрирование флеш-игры.
%%%

-module(dao_bGame).
-compile(export_all).

updateMap({null, Name, Data}) ->
    Q = "insert into game_map (name, data) values ($1, $2);",
    dao:simple(Q, [Name, Data]);
updateMap({Id, Name, Data}) ->
    Q = "update game_map set name=$1, data=$2 where id=$3;",
    dao:simple(Q, [Name, Data, Id]).

getMaps(_) ->
    Q = "select id, name, data from game_map;",
    dao:simple(Q).

getMap(Id) ->
    Q = "select id, name, data from game_map where id=$1;",
    dao:simple(Q, [Id]).

%% 
%% Строка в base 64.
%% [12:10:34] kozhevnikov_serge: load_game_map
%% [12:10:40] kozhevnikov_serge: web_cb
%% 

