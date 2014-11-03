%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfUsersR).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").
-define(COOKIE, config:get(cookiename, "logistics")).




changePass(_Req, {_, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/users/change-pass-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/users/change-pass-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/users/change-pass.xsl"
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




logout(Req, {_, RetPath, Links_Options, Template_Options}) ->
    auth_biz:logout(Req:get_cookie_value(?AUTHCOOKIE)),
    % если мы выходим, то перенаправляемся.
    throw({redirect, RetPath, []}).

login(Req, {normal, Param, Links_Options, Template_Options}) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            login_(Req, {{get, norm}, Param, Links_Options, Template_Options});
        'POST' ->
            login_(Req, {{post, norm}, Param, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end;
    
login(Req, {ajax, Param, Links_Options, Template_Options}) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            login_(Req, {{get, norm}, Param, Links_Options, Template_Options});
        'POST' ->
            login_ajax(Req, {{post, norm}, Param, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.

login_ajax(Req, {{post, _}, RetPath, Links_Options, Template_Options}) ->
    io:format("login_ajax(Req, {{post, _}, RetPath, Links_Options, Template_Options})"),
    Data = Req:parse_post(),
    Login = proplists:get_value("login", Data),
    Password = proplists:get_value("password", Data),
    try
        Val = auth_biz:login(Login, Password),
        
        io:format("Login  = ~p~n", [Login]),
        io:format("Password = ~p~n", [Password]),
    
        {_, Cookie } = cookie(?COOKIE, Val, ?F_COOKIEOPTIONS),
    
        throw({ok, {auth_ajax, {ok, Cookie}}})
    catch
        throw:{ok, Ret} -> throw(Ret);
        throw:Error ->
            io:format("login_ajax(Req, {{post, _}, RetPath, Links_Options, Template_Options}) Error  = ~p~n", [Error ]),
            throw({auth_ajax, {error, Error}});
        _:Error->
            io:format("login_ajax(Req, {{post, _}, RetPath, Links_Options, Template_Options}) Error  = ~p~n", [Error ]),
            throw({auth_ajax, {error, Error}})
    end.
    
    
login_(Req, {{post, _}, RetPath, Links_Options, Template_Options}) ->

    Data = Req:parse_post(),
    Login = proplists:get_value("login", Data),
    Password = proplists:get_value("password", Data),

    io:format("Login  = ~p~n", [Login]),
    io:format("Password = ~p~n", [Password]),
    
    try
        Val = auth_biz:login(Login, Password),
        throw({ok, {redirect, RetPath, 
            [cookie(?COOKIE, Val, ?F_COOKIEOPTIONS)]}})
    catch
        throw:{ok, Ret} -> throw(Ret);
        throw:Error ->
            login_(Req, {{get, {error, Error}}, RetPath, Links_Options, Template_Options});
        _:Error->
            login_(Req, {{get, {error, Error}}, RetPath, Links_Options, Template_Options})
    end;

login_(_Req, {{get, Varaint}, RetPath, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/users/login-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/users/login-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/users/login.xsl"
            end
    end,
        
    case Varaint of
        {error, Mess} ->
            HasErrors = "true",
            ErrorMess = Mess;
        _  ->
            HasErrors = "false",
            ErrorMess = ""
    end,
    
    Meta = [[
            {"current-path",        _Req:get(path)},
            {"self-parent-path",    SelfParent},
            {"self-retpath",        RetPath},
            {"item-parent-path",    ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)},
            {"has-errors",          HasErrors},
            {"error-mess",          ErrorMess}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            % {one, Dir, "doc"},             % описание запроса
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
       
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.

%% ========================================================================
%%
%%
%% ========================================================================

edit(Req, {_, Param, Links_Options, Template_Options}) ->

    io:format("Param   = {~s}~n", [Param]),
    
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            edit_(Req, {{get, norm}, Param, Links_Options, Template_Options});
        'POST' ->
            edit_(Req, {{post, norm}, Param, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.

edit_(Req, {{post, _}, Param, Links_Options, Template_Options}) ->
    %% #web_session{customer_id=UID} = authorization:auth_required(Req),
    UID = 1,
    Data = Req:parse_post(),
    
    Pass =  proplists:get_value("password",     Data, ""),
    PassC = proplists:get_value("passwordC",    Data, ""),
    
    case Pass of
        PassC ->
            case Pass of
                "null" ->
                    Pashash = null;
                _ when length(Pass) /= 0 ->
                    Pashash = lists:flatten([io_lib:format("~2.16.0B", [X])
                        || X <- binary_to_list(erlang:md5(Pass))]);
                _ ->
                    Pashash = null
            end,
            E = norm:extr(Data, [{"id", [nullable, integer]},
                                 {"firstname", [string]},
                                 {"lastname", [string]},
                                 {"patronimic", [string]},
                                 {"login", [string]},
                                 {"email", [nullable, string]},
                                 {"city", [nullable, string]},
                                 {"organization", [nullable, string]},
                                 {"position", [nullable, string]}]),

            %% GroupList = [utils:to_integer(X) || X <- proplists:get_all_values("groups", Data)],
            Res = dao_fCustomer:updateCustomer({E, Pashash, [], UID}),
            case Res  of
                {ok, UserId} ->
                    %throw({redirect, "/User/Details/" ++ utils:to_string(UserId) ++ "/" , []});
                    throw({redirect, Param , []});
                _ ->
                    %% В случае ошибки. Пока не ввыводим ничегою
                    edit_(Req, {{get, Res}, Param, Links_Options, Template_Options})
            end;        
        _ ->
            io:format("PassC  = [~s]~n", [PassC]),
            edit_(Req, {{get, {error, <<"not conf">>}}, Param, Links_Options, Template_Options})
    end;


edit_(_Req, {{get, Varaint}, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/users/edit-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/users/edit-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/users/edit.xsl"
            end
    end,
    
    case Varaint of
        {error, Mess} ->
            HasErrors = "true",
            ErrorMess = Mess;
        _  ->
            HasErrors = "false",
            ErrorMess = ""
    end,
                
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            
            {"item-parent-path", ItemParent},
            
            {"self-retpath", Param},
            
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)},
            {"has-errors",          HasErrors},
            {"error-mess",          ErrorMess}
    ]],
    
    XMLTerms  = pg2xml:encodeData(
        [
            % {one, Dir, "doc"},             % описание запроса
            {one, Meta, "meta"}             % описание запроса
        ]
    ),
       
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.
