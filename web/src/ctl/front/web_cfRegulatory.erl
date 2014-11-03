%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfRegulatory).
-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

-compile(export_all).

%% Разрешенные действия:
%%
%% index/1, index/2,
%% list/1, list/2,
%% details/1, details/2,
%% edit/1, edit/2

test() ->
    utils:test(fun() ->
        dao_fDirectory:getDirA(3),
        dao_fDirectory:getDirs({3}),
        dao_fDirectory:getDocs({3})
    end, 100).

list(_Req, {_, Param, [SelfParent_, ItemParent | _ ],   [XslRoot | _ ]}) ->
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/regulatory/list-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/regulatory/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/regulatory/list.xsl"
            end
    end,
    TargetId = utils:getTargetId(Param, ?REG_ID),
    
    
%    io:format("XXX: ~p~n", [TargetId]),
%T0 = utils:utime(),    
    {ok, Dir, Attaches} = dao_fDirectory:getDirA(TargetId),
    
    case Param of
        "" ->
            SelfParent = SelfParent_,
            IsRoot = true;
        _  ->
            [DirS] = Dir,    
            Parent_dir_id = proplists:get_value("parent_dir_id", DirS),
            SelfParent = string:join([SelfParent_,  utils:to_string(Parent_dir_id), "/" ], ""),
            IsRoot = false
    end,
%{Dir, Attaches} ={{}, []},
%    io:format("ID:::::::::::::  ~p~n", [TargetId]),

    
    Attaches_ = [
        [ {"size", web_file:getAttachSizeString(Attach)} | Attach ]
        || Attach <- Attaches
    ],
    

%T1 = utils:utime(),
    {ok, DirList} = dao_fDirectory:getDirs2({TargetId}),
    {ok, DocList} = dao_fDirectory:getDocs({TargetId}),


    
%DirList = [],
%DocList = [],

%T2 = utils:utime(),
%    DDLIST = lists:append([
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,

%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,

%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList,
%        DocList
%    ]),




    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"is-root", IsRoot},
            {"self-parent-path", SelfParent},
            {"item-parent-path", ItemParent},
            
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {one,  Dir,     "doc"},         % сама директория
            {list, DirList, "dirs"},        % список резделов
            {list, DocList, "docs"},        % список документов
%            {list, DDLIST, "docs"},        % список документов
            {list, Attaches_, "attaches"},   % список вложений
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
%T3 = utils:utime(),
%    io:format(":::::::::: ~p - ~p - ~p~n", [T1-T0, T2-T1, T3-T2]),
%    io:format("~p~n", [[DocList]]),
%    io:format("XML LENGTH:~p~n", [length(XMLTerms)]),
%    file:write_file("/tmp/1", XMLTerms),
    Outty = xslt:apply(XslPath, XMLTerms),
%    Outty = "",
    {"text/html", [], [Outty]}.
