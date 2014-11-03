%%% @file dao_bSrc2.erl
%%%
%%%     Администрирование сторонних источников информации
%%%

-module(dao_bSrc2).
-compile(export_all).

-include("../include/common.hrl").

getEncoding(_) ->
    Q = "select name as encoding from web_encoding;",
    dao:simple(Q).

updateSrc({null, Name, Url, Regexp, Encoding}) ->
    Q = "insert into inf_src (name, url, regexp, value, encoding) values ($1, $2, $3, '', $4);",
    dao:simple(Q, [Name, Url, Regexp, Encoding]);

updateSrc({Id, Name, Url, Regexp, Encoding}) ->
    Q = "update inf_src set name=$1, url=$2, regexp=$3, value='', encoding=$4 where id=$5;",
    dao:simple(Q, [Name, Url, Regexp, Encoding, Id]).

updateSrcValue(Id, Value) ->
    Q = "update inf_src set value = $1 where id = $2;",
    dao:simple(Q, [Value, Id]).

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
    Q = "select id, name, url, regexp, encoding from inf_src where id=$1;",
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
    io:format(":::~p~n~n", [SrcProplist]),
    Id       = utils:to_integer(proplists:get_value("id", SrcProplist)),
    Regexp       = proplists:get_value("regexp", SrcProplist),
    Url      = proplists:get_value("url", SrcProplist),
    Encoding = proplists:get_value("encoding", SrcProplist),

    {ok, BinContent} = file:read_file("/tmp/1.html"),
    Content = binary_to_list(BinContent),
    Options = [global, dotall, {capture, all, list}],

    case re:run(Content, Regexp, Options) of
        {match, [[_, Result]]}->
            try
                Res = iconv(convertHtml(Result), Encoding),
                updateSrcValue(Id, Res)
            catch
                Error ->
                    flog:error(?FMT("compileSrc Error  = ~p~n", [Error]))
            end;            
        Error ->
            flog:error([{compileSrc, Content, SrcProplist, Error}])
    end,
    ok.

convertHtml(Html) ->
    R1 = re:replace(Html, "&", "\\&amp;",[global, {return,list}]),
    R2 = re:replace(R1, "<", "\\&lt;",[global, {return,list}]),
    R3 = re:replace(R2, ">", "\\&gt;",[global, {return,list}]),
    Res = R3,
    Res.

win_to_utf_2(Str) ->
    ExStr = string:join(["echo ", Str, " | iconv -f cp1251 -t utf8"], "\""),
    os:cmd(ExStr).
    

iconv(Str, "utf8") ->
    Str;
iconv(Str, Encoding) ->
    file:write_file("/tmp/aaa.tmp", Str), %TODO убрать промежуточный файл!! передавать инфу в iconv напрямую
    %os:cmd(?FMT("echo \"~s\" | iconv -f windows-1251 -t utf8", [Str])).
    os:cmd(?FMT("iconv -c -f ~s -t utf8 /tmp/aaa.tmp", [Encoding])).

