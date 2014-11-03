%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfHotline).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

% convertHtml(Html) ->
%     R1 = re:replace(Html, "&", "\\&amp;",[global, {return,list}]),
%     R2 = re:replace(R1, "<", "\\&gt;",[global, {return,list}]),
%     R3 = re:replace(R2, ">", "\\&lt;",[global, {return,list}]),
%     Res = R3,
%     Res.
% 
% convertSrcs(Srcs) ->
%     [ convertSrc(Src) || Src <- Srcs ].
% 
% convertSrc(SrcProplist)
%     Id       = proplists:get_value("id", SrcProplist),
%     Regexp   = convertHtml(proplists:get_value("regexp", SrcProplist)),
%     Value    = convertHtml(proplists:get_value("Value", SrcProplist)),
%     
%     [{"id", Id}, {"regexp", Regexp}, {"Value", Value},].
  
index(_Req, {_, Param,  [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/hotline/index-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/hotline/index-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/hotline/index.xsl"
            end
    end,
    
    {ok, SrcsV} = dao_bSrc:getSrcsFV([]),
    
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
            {list, SrcsV, "srcs"},
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
    
    io:format("XMLTerms = ~ts", [XMLTerms]),
    
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.


