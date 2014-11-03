%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfNews).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").


rss_list(_Req, {_, _,  [SelfParent, ItemParent| _ ],   [XslRoot | _ ]}) ->

    XslPath = XslRoot ++ "/news/list.xsl",

    {ok, List} = dao_fDirectory:getDocsLimitedFull({news, docs, 10}),

    io:format("List  = ~p~n~n", [List]),
    
    Meta = [[
            {"current-path",        _Req:get(path)},
            {"parent-path",         SelfParent},
            {"self-parent-path",    SelfParent},
            {"item-parent-path",    ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            {list, List, "news"},          % список новостей
            {one, Meta, "meta"}             % описание запроса
        ]
    ),

    io:format("XMLTerms  = ~s~n~n", [XMLTerms ]),
    
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.

%%% 
%%% Список новостей
%%%

list(_Req, {_, Param,  [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    Items_per_page = 10,
    % io:format("~nParam =~p~n", [Param]),
  
    case _Req:get_cookie_value(?SPEC) of
        "true" ->
            XslPath = XslRoot ++ "/news/list-spec.xsl";
        _ ->
            XslPath = XslRoot ++ "/news/list.xsl"
    end,
    
    case Param of
        [] ->
            Page = 0,
            Prev_page = 0;
        Val ->
            Page  = utils:to_integer(Val),
            case Page - 1 of
                -1 ->
                    Prev_page = undefined;
                Prev_page ->
                    Prev_page = Prev_page
            end
    end,
    
    {ok, List} = dao_fDirectory:getDocsLimited({news, docs, Items_per_page, Page}),
    {ok,[[{"count", Count}]]} = dao_fDirectory:getDocsCount({news, docs}),
        
    Pages = Count div Items_per_page ,
    
    
    io:format("Pages  = ~p", [Pages ]),
    
    
    

    case List of
        [] ->
            Next_page = undefined;
        _ ->
            Next_page = Page + 1
    end,

    
    
    Meta = [[
            {"current-path",        _Req:get(path)},

            {"prev-id", Prev_page},
            {"cur-id",  Page},
            {"next-id", Next_page},
            
            {"count",   Count},
            {"pages",   Pages},
            
            {"self-parent-path",    SelfParent},
            {"item-parent-path",    ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            {list, List, "news"},          % список новостей
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
    
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.


details(_Req, {_, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/news/details-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/news/details-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/news/details.xsl"
            end
    end,
    
    {ok, Doc, Attaches} = dao_fDirectory:getDoc(Id),
    
    Attaches_ = [
        [ {"size", web_file:getAttachSizeString(Attach)} | Attach ]
        || Attach <- Attaches
    ],
    
    Meta = [[
            {"current-path", _Req:get(path)},

            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            {"item-parent-path", ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            {one, Doc, "doc"},          % список термнов
            {list, Attaches_, "attaches"},          % список термнов 
            %{list, FirstLeters, "leters"},  % список букв
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
       
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.
