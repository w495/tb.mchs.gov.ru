%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfTerms).
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


getFirstLeters(List)->
    %% select substring(name from 1 for 1) from document;
    
    Names = [ [Val || {"name", Val} <- Item] || Item <- List ],
    lists:usort([[{"leter", string:left(X, 2)}] || [X] <- Names]).
    
    

list(_Req, {common, Leter, Links_Options, Template_Options}) ->
    listAll(_Req, {?TERM_ID + ?T_ALL, Leter, Links_Options, Template_Options});

list(_Req, {air, Leter, Links_Options, Template_Options}) ->
    listAll(_Req, {?TERM_ID + ?T_AIR, Leter, Links_Options, Template_Options});

list(_Req, {water, Leter, Links_Options, Template_Options}) ->
    listAll(_Req, {?TERM_ID + ?T_WATER, Leter, Links_Options, Template_Options});
    
list(_Req, {surface, Leter, Links_Options, Template_Options}) ->
    listAll(_Req, {?TERM_ID + ?T_SURFACE, Leter, Links_Options, Template_Options}).


listAll(_Req, {Id, Leter, [SelfParent, ItemParent, ListParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/terms/list-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/terms/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/terms/list.xsl"
            end
    end,

    io:format("ItemParent = ~p~n", [ItemParent]),
    
    {ok, List} = dao_fDirectory:getDocsRecursive({Id, Leter}),
    
    FirstLeters = getFirstLeters(List),
    
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            {"item-parent-path", ItemParent},
            {"list-parent-path", ListParent},
            
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            {list, List, "terms"},          % список термнов 
            {list, FirstLeters, "leters"},  % список букв
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
       
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.
