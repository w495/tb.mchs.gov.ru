%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfHome).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

%%% 
%%% Головная станица сайта
%%% 
index(_Req, {_, _,  [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    case _Req:get_cookie_value(?SPEC) of
        "true" ->
            XslPath = XslRoot ++ "/home/index-spec.xsl";
        _ ->
            XslPath = XslRoot ++ "/home/index.xsl"
    end,
    {ok, List} = dao_fDirectory:getDocsLimited({news, docs, 3}),
    {ok, Dir} = dao_fDirectory:getHeadDir(?HEAD_ID),
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
            {one,  Dir,  "doc"},             % описание запроса
            {one,  Meta, "meta"}             % описание запроса
        ]
    ),
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html; charset=UTF-8", [{"a", "b"}], [Outty]}.


%% ============================================================================
%% Для слабовидящих
%% В параметре Param передаем путь с которого кликнули сюда.
%%
%% ============================================================================

spec(_Req, {on, Param,  [SelfParent | _ ],   [XslRoot | _ ]}) ->
    Variant_ = _Req:get_cookie_value(?VARIANT),
    case  Variant_ of
        undefined ->    Variant = "big";
         _ ->           Variant = Variant_
    end,
    Color_ = _Req:get_cookie_value(?COLOR),
    case  Variant_ of
        undefined ->    Color = "white";
         _ ->           Color = Color_
    end,
    throw({redirect, Param , [
            cookie(?SPEC,       "true",     ?F_COOKIEOPTIONS),
            cookie(?VARIANT,    Variant,      ?F_COOKIEOPTIONS),
            cookie(?COLOR,      Color,    ?F_COOKIEOPTIONS)
        ]}
    );

spec(_Req, {{color, Color}, Param,  [SelfParent | _ ],   [XslRoot | _ ]}) ->
    CookieOptions = [{max_age, ?EXPCOOKIE}, {path, "/"}],
    throw({redirect, Param , [
            cookie(?SPEC,       "true",     ?F_COOKIEOPTIONS),
            cookie(?COLOR,      Color,      ?F_COOKIEOPTIONS)
        ]}
    );

spec(_Req, {{variant, Variant}, Param,  [SelfParent | _ ],   [XslRoot | _ ]}) ->

    io:format("Param = ~s~n", [Param]),
    
    throw({redirect, Param , [
            cookie(?SPEC,       "true",     ?F_COOKIEOPTIONS),
            cookie(?VARIANT,    Variant,    ?F_COOKIEOPTIONS)
        ]}
    );
    
spec(_Req, {off, Param,  [SelfParent | _ ],   [XslRoot | _ ]}) ->
    throw({redirect, Param , [
            cookie(?SPEC,       "false",     ?F_COOKIEOPTIONS)
        ]}
    ).
