%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfTestRoot).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").


list(_Req, {all, Param, Links_Options, Template_Options}) ->
    index(_Req, {all, Param, Links_Options, Template_Options});
    
list(_Req, {air, Param, Links_Options, Template_Options}) ->
    listAll(_Req, {root_test_air, Param, Links_Options, Template_Options});

list(_Req, {water, Param, Links_Options, Template_Options}) ->
    listAll(_Req, {root_test_water, Param, Links_Options, Template_Options});
    
list(_Req, {surface, Param, Links_Options, Template_Options}) ->
    listAll(_Req, {root_test_surface, Param, Links_Options, Template_Options}).

listAll(_Req, {Varaint, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    Login = authorization:auth_getlogin(_Req),

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/tests/list-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/tests/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/tests/list.xsl"
            end
    end,
    {ok, Tests} = dao_fDirectory:getTestsSubDirs({Varaint}),
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            {"self-retpath", _Req:get(path)},
            
            {"item-parent-path", ItemParent},
            {"login", Login},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {list, Tests, "dirs"},            
            {one, Meta, "meta"}                        
        ]
    ),
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.    

index(_Req, {all, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    Login = authorization:auth_getlogin(_Req),
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/tests/index-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/tests/index-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/tests/index.xsl"
            end
    end,
    TargetId = utils:getTargetId([], ?TEST_ID),
    {ok, TestClassTList} = dao_fDirectory:getDirsCntSubDirs({TargetId}),
    {ok, SurfaceCnt} = dao_fDirectory:getTestsCntSubDirs({root_test_surface}),
    {ok, AirCnt} = dao_fDirectory:getTestsCntSubDirs({root_test_air}),
    {ok, WaterCnt} = dao_fDirectory:getTestsCntSubDirs({root_test_water}),
    Meta = [[
            {"current-path", _Req:get(path)},
            {"self-parent-path", SelfParent},
            {"self-retpath", _Req:get(path)},
            
            {"item-parent-path", ItemParent},
            {"login", Login},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    
    io:format("Login = ~s~n", [Login]),
    
    XMLTerms  = pg2xml:encodeData(
        [
            {one, SurfaceCnt, "surface-count"},
            {one, AirCnt, "air-count"},
            {one, WaterCnt, "water-count"},
            {one, Meta, "meta"}                        
        ]
    ),
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.

