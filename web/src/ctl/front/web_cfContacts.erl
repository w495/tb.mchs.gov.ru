%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfContacts).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

%%% 
%%% Головная станица игры
%%%

list(_Req, {_, _,  [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/contacts/list-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/contacts/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/contacts/list.xsl"
            end
    end,
    
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            {"self-retpath", _Req:get(path)},
            {"item-parent-path", ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
    
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.


