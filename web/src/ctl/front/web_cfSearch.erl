%%% @file web_cfSearch.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfSearch).
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



list(Req, {_, Param,  Links_Options, Template_Options}) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            list_get(Req, {foo, Param, Links_Options, Template_Options});
        'POST' ->
            list_post(Req, {foo, bar, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.
    
list_get(Req, {_, Param,  [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
        
    case utils:to_integer(proplists:get_value("print", Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/search/list-print.xsl";
        _   ->  case Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/search/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/search/list.xsl"
            end
    end,
        
    case Param of
        [] -> Search_param = [];
        _  ->   {Search_param, _ } = lists:split(length(Param) - 1, Param)
    end,


    Meta = [[
            {"current-path",        Req:get(raw_path)},
            {"self-parent-path",    SelfParent},
            {"item-parent-path",    ItemParent},
            {"search-param",        Search_param},
            
            {"is-spec",             Req:get_cookie_value(?SPEC)},
            {"spec-variant",        Req:get_cookie_value(?VARIANT)},
            {"spec-color",          Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {one, Meta, "meta"}             % описание запроса
        ]
    ),    
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.

list_post(Req, {_, _,  [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    Data = Req:parse_post(),
    
    %% -------------------------------------------------------------
    %% Это очень плохой вариант.
    %% 
    %% -------------------------------------------------------------
    
    Query = proplists:get_value("q", Data),
    throw({redirect, lists:concat([Req:get(path), Query, "/" ]), []}).

