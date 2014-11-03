%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfDoc).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").




%% Разрешенные действия:
%%
%% index/1, index/2,
%% list/1, list/2,
%% details/1, details/2,
%% edit/1, edit/2



details(_Req, {_, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/doc/details-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/doc/details-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/doc/details.xsl"
            end
    end,

    {ok, Doc, Attaches} = dao_fDirectory:getDoc(Id),
    
    Attaches_ = [
        [ {"size", web_file:getAttachSizeString(Attach)} | Attach ]
        || Attach <- Attaches
    ],

    "
        SelfParent можно изменить следующим образом:
            1) /Dir/{dir_id}
                --- в этом случае можно создать дефолтный обработчик директорий
            2) Проверять по Refere из какого из корневых разделов пришли,
                И подставлять:
                    /Secure/List/{dir_id}/
                    /Regulatory/List/{dir_id}/
                    /Regulatory/News/
    ",
    
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
