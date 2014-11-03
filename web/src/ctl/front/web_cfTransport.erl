%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfTransport).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

-include_lib("kernel/include/file.hrl").

index(_Req, {air, Id, Links_Options, Template_Options}) ->
    index(_Req, {{"air", ?AIR_ID}, Id, Links_Options, Template_Options});
    
index(_Req, {water, Id, Links_Options, Template_Options}) ->
    index(_Req, {{"water", ?WATER_ID}, Id, Links_Options, Template_Options});

index(_Req, {surface, Id, Links_Options, Template_Options}) ->
    index(_Req, {{"surface", ?SURFACE_ID}, Id, Links_Options, Template_Options});
    
index(_Req, {{Type, Type_id}, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/transport/details-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/transport/details-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/transport/details.xsl"
            end
    end,
    
    {ok, Dir} = dao_fDirectory:getHeadDir(Type_id),
    
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
            {one, Dir, "doc"},             % описание запроса
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
       
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.
