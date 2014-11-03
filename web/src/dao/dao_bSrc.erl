%%% @file dao_bSrc.erl
%%%
%%%     Администрирование сторонних источников информации
%%%

-module(dao_bSrc).
-compile(export_all).

updateSrc({null, Name, Url, Regexp}) ->
    Q = "insert into inf_src (name, url, regexp, value) values ($1, $2, $3, '');",
    dao:simple(Q, [Name, Url, Regexp]);
updateSrc({Id, Name, Url, Regexp}) ->
    Q = "update inf_src set name=$1, url=$2, regexp=$3, value='' where id=$4;",
    dao:simple(Q, [Name, Url, Regexp, Id]).

updateSrcValue(SId, Value) ->
    Id = utils:to_integer(SId),
    Q = "update inf_src set value = $1 where id = $2; ",
    case dao:simple(Q, [convertHtml(Value), Id]) of
        ok -> ok;
        _ ->
            dao:simple(Q, [convertHtml(iconvCp1251(Value)), Id])
    end.

getSrcs(_) ->
    Q = "select id, name, url, regexp, update_datetime from inf_src;",
    dao:simple(Q).

getSrcsV(_) ->
    Q = "select id, name, url, regexp, update_datetime, value from inf_src;",
    dao:simple(Q).

getSrcsFV(_) ->
    Q = "select id, name, url, value from inf_src;",
    dao:simple(Q).
    
getSrc(Id) ->
    Q = "select id, name, url, regexp from inf_src where id=$1;",
    dao:simple(Q, [Id]).

deleteSrc(Id) ->
    Q = "delete from inf_src where id=$1;",
    dao:simple(Q, [Id]).

getSrcValByName(Name) ->
    Q = "select value from inf_src where name=$1;",
    dao:simple(Q, [Name]).

compileSrcs(_) ->
    % dao_bSrc:compileSrcs([])
    {ok, Srcs} = getSrcs([]),
    [ compileSrc(Src) || Src <- Srcs ].
    
compileSrc(SrcProplist) ->
    Id       = proplists:get_value("id", SrcProplist),
    Regexp       = proplists:get_value("regexp", SrcProplist),
    Url      = proplists:get_value("url", SrcProplist),
    {ok, {Header1, Header2, Content }} = http:request(Url),
    Options = [global, dotall, {capture, all, list}],
	case re:run(Content, Regexp, Options) of
		{match, [[_, Result]]}->
            try
                updateSrcValue(Id, Result)
            catch
                A:B ->
                    io:format("~nError  = ~p~n", [B])
            end;            
		_ ->
            updateSrcValue(Id, "")
	end,
    ok.

convertHtml(Html) ->
    R1 = re:replace(Html, "&", "\\&amp;",[global, {return,list}]),
    R2 = re:replace(R1, "<", "\\&lt;",[global, {return,list}]),
    R3 = re:replace(R2, ">", "\\&gt;",[global, {return,list}]),
    Res = R3,
    Res.

iconvCp1251(Str) ->
    ExStr = string:join(["echo ", Str, " | iconv -f cp1251 -t utf8"], "\""),
    os:cmd(ExStr).
    
