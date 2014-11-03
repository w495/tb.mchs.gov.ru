-module(web_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).
-include_lib("xmerl/include/xmerl.hrl").
-include("../include/common.hrl").

-compile(export_all).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    io:format("~p: ~p got stop signal~n", [erlang:localtime(), ?MODULE]),
    supervisor:terminate_child(web_sup, ?MODULE),
    supervisor:delete_child(web_sup, ?MODULE).

serve_static_inner(P, T, Req, ExtraHeaders) ->
    try
        {Tmp, <<".js">>} = split_binary(list_to_binary(T), length(T) - 3),
        Fname = binary_to_list(Tmp) ++ ".gz.js",
        true = lists:member("gzip", string:tokens(Req:get_header_value("Accept-Encoding"), ", ")),
        {ok, _} = file:read_file_info(P ++ "/" ++ Fname),
        Req:serve_file(Fname, P, [{"content-encoding", "gzip"} | ExtraHeaders])
    catch
        _:_ -> 
            %% io:format("*************** ~p ~p~n", [T, P]),
            Req:serve_file(T, P, ExtraHeaders)
    end.

serve_static(P, T, Req) ->
    serve_static_inner(P, T, Req, []).

serve_static(P, T, Req, ExtraHeaders) ->
    serve_static_inner(P, T, Req, ExtraHeaders).

start_controller(Module, Action, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) ~p:~p ~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Module, Action])),
    Exports = Module:module_info(exports),
    % % CALL BEFORE
    _NReq = bacRunner(Exports, call_before, Module, Req),
    % % CALL CONTROLLER
    Result = Module:Action(Req),
    % % CALL AFTER
    {_, NResult} = bacRunner(Exports, call_after, Module, {Req, Result}),
    Req:ok(NResult).

start_controller(Module, Action, Req, Param) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) ~p:~p ~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Module, Action])),
    Exports = Module:module_info(exports),
    % % CALL BEFORE
    _NReq = bacRunner(Exports, call_before, Module, Req),
    % % CALL CONTROLLER
    Result = Module:Action(Req, Param),
    % % CALL AFTER
    {_, NResult} = bacRunner(Exports, call_after, Module, {Req, Result}),
    Req:ok(NResult).


bacRunner([{M, _}|T], Method, Module, Param) ->
    case M =:= Method of
        true -> Module:Method(Param);
        false -> bacRunner(T, Method, Module, Param)
    end;
bacRunner([], _Method, _Module, Param) ->
    Param.


save_start_controller(Module, Action, Req) ->
    try
        start_controller(Module, Action, Req)
    catch throw:Val ->
        processControllerException(throw, Val, Req)
    end.

save_start_controller(Module, Action, Req, T) ->
    try
        start_controller(Module, Action, Req, T)
    catch throw:Val ->
        processControllerException(throw, Val, Req)
    end.

%% ========================================================================
%% ПРОБЛЕМЫ И ОШИБКИ
%% ========================================================================

processControllerException(throw, nothing_to_be_done, _Req) ->
    [];
    
processControllerException(throw, {serve_static, Filename, Path, ExtraHeaders}, Req)->
    serve_static(Path, Filename, Req, ExtraHeaders);
    
processControllerException(throw, {js_redirect, Url, _Cookie}, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) JS Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    V = {struct, [{<<"REDIRECT">>, list_to_binary(Url)}]},
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});

%%
%% Перенаправление без Cookie, как для аутентификации.
%%
processControllerException(throw, {redirect, Url, []}, Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    Req:respond({302, [{"Location", Url}, {"Content-Type", "text/html; charset=UTF-8"}], ""});

%%
%% Перенаправление с новыми Cookie, как для слепых.
%%
processControllerException(throw, {redirect, Url, Cookie}, Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) Redirect to ~p~n", [?MODULE, ?LINE, Req:get(method), Req:get(path), Url])),
    Req:respond({302, [{"Location", Url}, {"Content-Type", "text/html; charset=UTF-8"}] ++ Cookie,""});
    
processControllerException(throw, not_found, Req) ->
    flog:info(?FMT("~p:~p 404 ~p REQUEST (~p) Not Found~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    Req:not_found();

processControllerException(throw, auth_required, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    %V = {struct, [{<<"REDIRECT">>, <<"/login.html">>}]},
    V = {struct, [{<<"REDIRECT">>, <<"/login">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});

%%
%% Перенаправление без аутентификации.
%%
processControllerException(throw, {auth_required_front, RetPath} , Req) ->
    flog:debug(?FMT("~p:~p 302 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    processControllerException(throw, {redirect, "/Users/Login" ++ RetPath, []}, Req);

%%
%% application/json 
%%
processControllerException(throw, {auth_ajax, State} , Req) ->
    {Iserr, Val} = State,
    case Iserr of
        ok      ->
            V = {struct, [{<<"mess">>, null }, {"val", list_to_binary(Val)}]};
        error   ->
            case Val of
                {M, _N} ->
                    ErrMess = list_to_binary(M);
                [_H | _] ->
                    ErrMess = list_to_binary(Val);
                _ ->
                    ErrMess = <<"unknown">> 
            end,
            V = {struct, [{<<"mess">>, ErrMess }, {"val", ErrMess}]}
    end,
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});

%%
%% application/json
%%
processControllerException(throw, {banners_ajax, State} , Req) ->
    Req:ok({"application/json", [], [mochijson2:encode(State)]});

processControllerException(throw, auth_required_dialog, Req) ->
    flog:debug(?FMT("~p:~p 200 ~p REQUEST (~p) AUTH REQUIRED~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
    V = {struct, [{<<"ERROR">>, <<"auth_required">>}]},
    DataOut = mochijson2:encode(V),
    Req:ok({"application/json", [], [DataOut]});
    
processControllerException(Type, Exc, Req) ->
    flog:error(?FMT("~p:~p Catch unknown exception (~p) on ~p request ~p ~n",[?MODULE, ?LINE, Exc, Req:get(method), Req:get(path)])),
    Type(Exc).


%% ========================================================================
%% СТАТИЧЕСКОЕ ОТОБРАЖЕНИЕ АДРЕСОВ
%% ========================================================================

serve_request("/"++?QOOXDOOBUILD++"/index.html", Req) ->
    io:format("~n goto INDEX ~n"),
    serve_request("INDEX", Req); % перенаправление в динамический мэппинг для проверки авторизации
    
serve_request("/" ++ ?QOOXDOOBUILD ++ "/" ++ T, Req) ->
    serve_static(?JSHOME, T, Req);
    
serve_request("/deps/qooxdoo/" ++ T, Req) ->
    serve_static("deps/qooxdoo/", T, Req);
    
serve_request("/resource/" ++ T, Req) ->
    serve_static(?JSHOME ++ "/resource/", T, Req);

serve_request("/favicon.ico", Req) ->
    serve_static("static/site-media/favicon.ico", "", Req);
    
serve_request("/static/" ++ T, Req) ->
    % % io:format("TTT: ~p~n", [T]),
    serve_static("static", T, Req);

%% ========================================================================
%% КОНТРОЛЛЕРЫ  
%% ========================================================================
    
serve_request(Path, Req) ->

    %%%
    %%% TODO: ВЫНЕСТИ В HRL.
    %%%
    
    Links_TransOptions = ["/", "/Transport/", "/Home/Index/"],
    
    Links_RegulatoryOptions = ["/Regulatory/List/", "/Regulatory/List/"],
    Links_SecureOptions =     ["/Secure/List/",     "/Transport/"],
    Links_NewsOptions =       ["/News/List/",       "/Transport/"],
    
    Links_ConfOptions =     ["/Conf/List/", "/Transport/"],

    Links_TermsOptions = ["/Terms/List/", "/Terms/List/", "/Term/" ],
    Links_TestsOptions = ["/Tests/List/", "/Tests/List/"],
    
    Template_NormalOptions  = ["xsl/normal"],
    Template_WapOptions  = ["xsl/wap"],
    Template_MobileOptions  = ["xsl/mobile"],
    Template_RssOptions  = ["xsl/rss"],
    
    case Path of
    
        %%% Неявные  Перенаправления
        
            "/" ->
                serve_request("/Home/Index/" , Req);
                "/Wap/" ->
                    serve_request("/Wap/Home/Index/" , Req);
                "/Mobile/" ->
                    serve_request("/Mobile/Home/Index/" , Req);
                    
            "/Home/" ->
                serve_request("/Home/Index/" , Req);
                "/Wap/Home/" ->
                    serve_request("/Wap/Home/Index/" , Req);
                "/Mobile/Home/" ->
                    serve_request("/Mobile/Home/Index/" , Req);                
                
        %%% /Home/Spec --- переключение на версии для слепых.

            "/Home/Spec/On" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfHome, spec, Req, {on, Param, Links_TransOptions, Template_NormalOptions });
    
            "/Home/Spec/Big" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                io:format("Param = ~s~n", [Param]),
                save_start_controller(web_cfHome, spec, Req, {{variant, "big"}, Param, Links_TransOptions, Template_NormalOptions });
    
            "/Home/Spec/Medium" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                io:format("Param = ~s~n", [Param]),
                save_start_controller(web_cfHome, spec, Req, {{variant, "medium"}, Param, Links_TransOptions, Template_NormalOptions });
                
            "/Home/Spec/Small" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
            
                save_start_controller(web_cfHome, spec, Req, {{variant, "small"}, Param, Links_TermsOptions, Template_NormalOptions });
                
            "/Home/Spec/Black" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfHome, spec, Req, {{color, "black"}, Param, Links_TransOptions, Template_NormalOptions });
                
            "/Home/Spec/Blue" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfHome, spec, Req, {{color, "blue"}, Param, Links_TransOptions, Template_NormalOptions });
                
            "/Home/Spec/White" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfHome, spec, Req, {{color, "white"}, Param, Links_TransOptions, Template_NormalOptions });

            "/Home/Spec/Off" ++ Param-> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfHome, spec, Req, {off, Param, Links_TransOptions, Template_NormalOptions });

        %%% Домашняя страница сайта
        
            "/Home/Index/" ->    
                save_start_controller(web_cfHome, index, Req, {foo, bar, Links_TransOptions, Template_NormalOptions });

                "/Wap/Home/Index/" ->    
                    save_start_controller(web_cfHome, index, Req, {foo, bar, Links_TransOptions, Template_WapOptions });
                "/Mobile/Home/Index/" ->    
                    save_start_controller(web_cfHome, index, Req, {foo, bar, Links_TransOptions, Template_MobileOptions });
    
        %%% Контакты
        
            "/Contacts/List/" ->    
                save_start_controller(web_cfContacts, list, Req, {foo, bar, Links_TransOptions, Template_NormalOptions });
                "/Wap/Contacts/List/" ->    
                    save_start_controller(web_cfContacts, list, Req, {foo, bar, Links_TransOptions, Template_WapOptions   });
                "/Mobile/Contacts/List/" ->    
                    save_start_controller(web_cfContacts, list, Req, {foo, bar, Links_TransOptions, Template_MobileOptions });

            "/Hotline/Index/" ->    
                save_start_controller(web_cfHotline, index, Req, {foo, bar, Links_TransOptions, Template_NormalOptions });
                "/Wap/Hotline/Index/" ->    
                    save_start_controller(web_cfHotline, index, Req, {foo, bar, Links_TransOptions, Template_WapOptions  });
                "/Mobile/Hotline/Index/" ->    
                    save_start_controller(web_cfHotline, index, Req, {foo, bar, Links_TransOptions, Template_MobileOptions });
                    
        %%% Cтраница игры
        
            "/Game/Index/" ->    
                save_start_controller(web_cfGame, index, Req, {foo, bar, Links_TransOptions, Template_NormalOptions });
                "/Wap/Game/Index/" ->    
                    save_start_controller(web_cfGame, index, Req, {foo, bar, Links_TransOptions, Template_WapOptions });
                "/Mobile/Game/Index/" ->    
                    save_start_controller(web_cfGame, index, Req, {foo, bar, Links_TransOptions, Template_MobileOptions });

            "/Game/Details/" ->    
                save_start_controller(web_cfGame, details, Req, {foo, bar, Links_TransOptions, Template_NormalOptions });
                "/Wap/Game/Details/" ->    
                    save_start_controller(web_cfGame, details, Req, {foo, bar, Links_TransOptions, Template_WapOptions });
                "/Mobile/Game/Details/" ->    
                    save_start_controller(web_cfGame, details, Req, {foo, bar, Links_TransOptions, Template_MobileOptions });                    
                    
        %%% Транспорт
            "/Transport/Air/" ->    
                save_start_controller(web_cfTransport, index, Req, {air, bar, Links_TransOptions,  Template_NormalOptions});

                "/Wap/Transport/Air/" ->    
                    save_start_controller(web_cfTransport, index, Req, {air, bar, Links_TransOptions,  Template_WapOptions});
                "/Mobile/Transport/Air/" ->    
                    save_start_controller(web_cfTransport, index, Req, {air, bar, Links_TransOptions,  Template_MobileOptions});
                
            "/Transport/Surface/" ->    
                save_start_controller(web_cfTransport, index, Req, {surface, bar, Links_TransOptions, Template_NormalOptions });

                "/Wap/Transport/Surface/" ->    
                    save_start_controller(web_cfTransport, index, Req, {surface, bar, Links_TransOptions, Template_WapOptions});
                "/Mobile/Transport/Surface/" ->    
                    save_start_controller(web_cfTransport, index, Req, {surface, bar, Links_TransOptions, Template_MobileOptions});
                
            "/Transport/Water/" ->    
                save_start_controller(web_cfTransport, index, Req, {water, bar, Links_TransOptions, Template_NormalOptions });

                "/Wap/Transport/Water/" ->    
                    save_start_controller(web_cfTransport, index, Req, {water, bar, Links_TransOptions, Template_WapOptions});
                "/Mobile/Transport/Water/" ->    
                    save_start_controller(web_cfTransport, index, Req, {water, bar, Links_TransOptions, Template_MobileOptions});


        %%% Тесты
        
            "/Tests/Index/" ->    
                save_start_controller(web_cfTestRoot, index, Req, {all, bar, Links_TestsOptions, Template_NormalOptions });

            "/Tests/List/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {all, bar, Links_TestsOptions, Template_NormalOptions });

            "/Tests/Index/Air/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {air, bar, Links_TestsOptions, Template_NormalOptions });
                
            "/Tests/List/Air/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {air, bar, Links_TestsOptions, Template_NormalOptions });

            "/Tests/Index/Surface/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {surface, bar, Links_TestsOptions, Template_NormalOptions });
                
            "/Tests/List/Surface/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {surface, bar, Links_TestsOptions, Template_NormalOptions });

            "/Tests/Index/Water/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {water, bar, Links_TestsOptions, Template_NormalOptions });
                
            "/Tests/List/Water/" ->    
                save_start_controller(web_cfTestRoot, list, Req, {water, bar, Links_TestsOptions, Template_NormalOptions });

        %%% Тест
        
            "/Test/Details/" ++ Param ->    
                save_start_controller(web_cfTest, details, Req, {foo, killSlash(Param), Links_TestsOptions, Template_NormalOptions });                

            "/TEST/" ++ PARAM->    
                save_start_controller(web_cfTest, index, Req, {foo, bar, Links_TransOptions, Template_NormalOptions});

        %%% Конференции

            "/Conf/List/" ++ Param ->    
                save_start_controller(web_cfConf, list, Req, {all, killSlash(Param), Links_ConfOptions, Template_NormalOptions });
                
        %%% Конференция
        
            "/Conf/Details/" ++ Param ->    
                save_start_controller(web_cfConf, details, Req, {all, killSlash(Param), Links_TransOptions, Template_NormalOptions });                

            "/Conf/MergeQuestions" ++ Param -> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfConf, mergeQuestions, Req, {all, Param, Links_TransOptions, Template_NormalOptions });

            "/Conf/Answer" ++ Param -> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfConf, answer, Req, {all, Param, Links_TransOptions, Template_NormalOptions });
            
            "/Conf/Ask" ++ Param -> % !!! ОБРАТИТЬ ВНИМАНИЕ, НЕТ ЗАВЕРШАЮЩЕГО СЛЕША. ТАК И НАДО
                save_start_controller(web_cfConf, ask, Req, {all, Param, Links_TransOptions, Template_NormalOptions });
            
        %%% Страница со списком новостей
            
            "/News/Rss/" ++ Page ->
                save_start_controller(web_cfNews, rss_list, Req, {foo, foo, Links_NewsOptions, Template_RssOptions });
            
            "/News/List/" ++ Page ->     
                save_start_controller(web_cfNews, list, Req, {tail, killSlash(Page), Links_NewsOptions, Template_NormalOptions });
                "/Wap/News/List/" ++ Page ->     
                    save_start_controller(web_cfNews, list, Req, {tail, killSlash(Page), Links_NewsOptions, Template_WapOptions });
                "/Mobile/News/List/" ++ Page ->     
                    save_start_controller(web_cfNews, list, Req, {tail, killSlash(Page), Links_NewsOptions, Template_MobileOptions });
                    
            "/News/" ++ Param -> 
                start_controller(web_cfNews, details, Req, {common, killSlash(Param), Links_NewsOptions, Template_NormalOptions});
                "/Wap/News/" ++ Param -> 
                    start_controller(web_cfNews, details, Req, {common, killSlash(Param), Links_NewsOptions, Template_WapOptions});
                "/Mobile/News/" ++ Param -> 
                    start_controller(web_cfNews, details, Req, {common, killSlash(Param), Links_NewsOptions, Template_MobileOptions});

        %%% Страница со списком нормативных документов
        
            % =============================================================
            % =============================================================
            % ЖУТЧАЙШИЙ КОСТЫЛЬ
            % =============================================================
            % =============================================================
            
            "/Regulatory/List/Surface/" ->
                serve_request("/Regulatory/List/44/" , Req);

            "/Regulatory/List/Air/" ->
                serve_request("/Regulatory/List/42/" , Req);

            "/Regulatory/List/Water/" ->
                serve_request("/Regulatory/List/50/" , Req);
                
            % =============================================================
            %  / ЖУТЧАЙШИЙ КОСТЫЛЬ
            % =============================================================
            
            "/Regulatory/List/" ++ Param ->
               save_start_controller(web_cfRegulatory, list, Req, {foo, killSlash(Param),   Links_RegulatoryOptions , Template_NormalOptions});               
                "/Wap/Regulatory/List/" ++ Param ->
                   save_start_controller(web_cfRegulatory, list, Req, {foo, killSlash(Param),   Links_RegulatoryOptions , Template_WapOptions});
                "/Mobile/Regulatory/List/" ++ Param ->
                   save_start_controller(web_cfRegulatory, list, Req, {foo, killSlash(Param),   Links_RegulatoryOptions , Template_MobileOptions});
                   
        %%% Страница со списком документов о безопасности
        
            "/Secure/List/" ++ Param ->
               save_start_controller(web_cfSecure, list, Req, {foo, killSlash(Param),   Links_SecureOptions, Template_NormalOptions});
                "/Wap/Secure/List/" ++ Param ->
                   save_start_controller(web_cfSecure, list, Req, {foo, killSlash(Param),   Links_SecureOptions, Template_WapOptions});
                "/Mobile/Secure/List/" ++ Param ->
                   save_start_controller(web_cfSecure, list, Req, {foo, killSlash(Param),   Links_SecureOptions, Template_MobileOptions});
               
         %%% /Term/ --- списки терминов.
         
            "/Term/" ++ Param ->    serve_request("/Doc/" ++ Param, Req);
                "/Wap/Term/" ++ Param ->        serve_request("/Wap/Doc/" ++ Param, Req);
                "/Mobile/Term/" ++ Param ->     serve_request("/Mobile/Doc/" ++ Param, Req);
                    
         %%% /Terms/ --- списки терминов.
         
            % Список терминов назмного транспорта
            "/Terms/List/Surface/" ++ Letter ->
                save_start_controller(web_cfTerms, list, Req, {surface, killSlash(Letter),    Links_TermsOptions, Template_NormalOptions});
                "/Wap/Terms/List/Surface/" ++ Letter ->
                    save_start_controller(web_cfTerms, list, Req, {surface, killSlash(Letter),    Links_TermsOptions, Template_WapOptions});
                "/Mobile/Terms/List/Surface/" ++ Letter ->
                    save_start_controller(web_cfTerms, list, Req, {surface, killSlash(Letter),    Links_TermsOptions, Template_MobileOptions});
                    
            % Список терминов водного транспорта
            "/Terms/List/Water/" ++ Letter ->
                save_start_controller(web_cfTerms, list, Req, {water, killSlash(Letter),      Links_TermsOptions, Template_NormalOptions});
                "/Wap/Terms/List/Water/" ++ Letter ->
                    save_start_controller(web_cfTerms, list, Req, {water, killSlash(Letter),      Links_TermsOptions, Template_WapOptions});
                "/Mobile/Terms/List/Water/" ++ Letter ->
                    save_start_controller(web_cfTerms, list, Req, {water, killSlash(Letter),      Links_TermsOptions, Template_MobileOptions});
                    
            % Список терминов воздушного транспорта
            "/Terms/List/Air/" ++ Letter ->
                save_start_controller(web_cfTerms, list, Req, {air, killSlash(Letter),        Links_TermsOptions, Template_NormalOptions});
                "/Wap/Terms/List/Air/" ++ Letter ->
                    save_start_controller(web_cfTerms, list, Req, {air, killSlash(Letter),        Links_TermsOptions, Template_WapOptions});
                "/Mobile/Terms/List/Air/" ++ Letter ->
                    save_start_controller(web_cfTerms, list, Req, {air, killSlash(Letter),        Links_TermsOptions, Template_MobileOptions});
                    
            % Список всех терминов 
            "/Terms/List/" ++ Param ->
                % % Попытка сделать двойной аргумент для терминов.
                % % Адрес /Terms/List/Ы/Water/ ---> /Terms/List/Water/Ы/
                MatchOptions = [{capture, all, list}, bsr_unicode],
                case re:run(Param, "(.*)/(.*)/", MatchOptions) of
                    {match, [_, Letter, "Water"]}->
                        Str = string:join(["/Terms/List/Water/", Letter, "/"], ""),
                        serve_request(Str, Req);
                    {match, [_, Letter, "Air"]} ->
                        Str = string:join(["/Terms/List/Air/", Letter, "/"], ""),
                        serve_request(Str, Req);
                    {match, [_, Letter, "Surface"]} ->
                        Str = string:join(["/Terms/List/Surface/", Letter, "/"], ""),
                        serve_request(Str, Req);
                    _ ->
                        % Если нет совпадений, то применяем простой
                        % /Terms/List/Ы/
                        save_start_controller(web_cfTerms, list, Req, {common, killSlash(Param),  Links_TermsOptions, Template_NormalOptions })
                end;                
                "/Wap/Terms/List/" ++ Param ->
                    MatchOptions = [{capture, all, list}, bsr_unicode],
                    case re:run(Param, "(.*)/(.*)/", MatchOptions) of
                        {match, [_, Letter, "Water"]}->
                            Str = string:join(["/Wap/Terms/List/Water/", Letter, "/"], ""),
                            serve_request(Str, Req);
                        {match, [_, Letter, "Air"]} ->
                            Str = string:join(["/Wap/Terms/List/Air/", Letter, "/"], ""),
                            serve_request(Str, Req);
                        {match, [_, Letter, "Surface"]} ->
                            Str = string:join(["/Wap/Terms/List/Surface/", Letter, "/"], ""),
                            serve_request(Str, Req);
                        _ ->    save_start_controller(web_cfTerms, list, Req, {common, killSlash(Param),  [Path, "/Term/", "/Home/Index/"], Template_WapOptions })
                    end;
                    "/Mobile/Terms/List/" ++ Param ->
                        MatchOptions = [{capture, all, list}, bsr_unicode],
                        case re:run(Param, "(.*)/(.*)/", MatchOptions) of
                            {match, [_, Letter, "Water"]}->
                                Str = string:join(["/Mobile/Terms/List/Water/", Letter, "/"], ""),
                                serve_request(Str, Req);
                            {match, [_, Letter, "Air"]} ->
                                Str = string:join(["/Mobile/Terms/List/Air/", Letter, "/"], ""),
                                serve_request(Str, Req);
                            {match, [_, Letter, "Surface"]} ->
                                Str = string:join(["/Mobile/Terms/List/Surface/", Letter, "/"], ""),
                                serve_request(Str, Req);
                            _ ->    save_start_controller(web_cfTerms, list, Req, {common, killSlash(Param),  Links_TermsOptions, Template_MobileOptions })
                        end;
                
        %% /Users/ --- опирации с пользователями
                
            "/Users/ChangePass" ++ RetPath ->
                save_start_controller(web_cfUsersR, changePass, Req, {foo, RetPath, Links_TransOptions, Template_NormalOptions});
            
            "/Users/Registration" ++ RetPath ->
                save_start_controller(web_cfUsersR, edit, Req, {foo, RetPath, Links_TransOptions, Template_NormalOptions});
                
            "/Users/Login" ++ RetPath ->
                save_start_controller(web_cfUsersR, login, Req, {normal, RetPath, Links_TransOptions, Template_NormalOptions});

            "/!/Users/Login" ++ RetPath ->
                save_start_controller(web_cfUsersR, login, Req, {ajax, RetPath, Links_TransOptions, Template_NormalOptions});
                
            "/Users/Logout" ++ RetPath ->
                save_start_controller(web_cfUsersR, logout, Req, {foo, RetPath, Links_TransOptions, Template_NormalOptions});

        %% /!/Banners/ --- баннеры
        
            "/!/Banners/" ->
                save_start_controller(web_cfBanners, details, Req, {ajax, common, Links_TransOptions, Template_NormalOptions});

        %% /Search/ --- баннеры

            "/Search/" ++ Param ->
                save_start_controller(web_cfSearch, list, Req, {foo, Param, Links_TransOptions, Template_NormalOptions});        
                "/Mobile/Search/" ++ Param ->
                    save_start_controller(web_cfSearch, list, Req, {foo, Param, Links_TransOptions, Template_MobileOptions});
                
                
        %% /Doc/ --- документы
        
                
            "/Doc/" ++ Param -> case Param of [] -> serve_request("/Home/Index/" , Req);
                    _  -> save_start_controller(web_cfDoc, details, Req, {common, killSlash(Param), Links_TransOptions, Template_NormalOptions})
            end;
                "/Wap/Doc/" ++ Param -> case Param of [] -> serve_request("/Wap/Home/Index/" , Req);
                        _  -> save_start_controller(web_cfDoc, details, Req, {common, killSlash(Param), Links_TransOptions, Template_WapOptions})
                end;
                "/Mobile/Doc/" ++ Param -> case Param of [] -> serve_request("/Mobile/Home/Index/" , Req);
                        _  -> save_start_controller(web_cfDoc, details, Req, {common, killSlash(Param), Links_TransOptions, Template_MobileOptions})
                end;
                
        _   ->
                try simple_map_controllers(Path) of
                    {Module, Action} ->
                        save_start_controller(Module, Action, Req)
                catch
                    _:_ ->
                    flog:info(?FMT("~p:~p 404 ~p REQUEST (~p) ERROR! Controller NOT FOUND~n", [?MODULE, ?LINE, Req:get(method), Req:get(path)])),
                    Req:not_found()
                end
                            
        
    end.

%% ========================================================================
%% Простой map контроллеров (испрользуется в админке)
%%      Оставлен по историческим причинам.
%% ========================================================================

simple_map_controllers(Path) ->
    case Path of
        "/get-encoding" -> {web_cb, get_encoding};
    

    % сторонние источники информации
        "/get-srcs" -> {web_cb, get_srcs};
        "/get-src-info" -> {web_cb, get_src_info};
        "/update-src" -> {web_cb, update_src};
        "/delete-src" -> {web_cb,  delete_src};

    % game-xml
        "/get-game-xml" -> {web_cb, get_game_xml};
        "/update-game-xml" -> {web_cb, update_game_xml};

    % log
        "/get-logs" -> {web_cb, get_logs};
        "/get-log-info" -> {web_cb, get_log_info};

    % advertising company
        "/get-banner-places" -> {web_cb, get_banner_places};
        "/get-adv-coms" -> {web_cb, get_adv_coms};
        "/get-adv-com" -> {web_cb, get_adv_com};
        "/update-adv-com/upload-image" -> {web_file, upload_adv_com_image};
        "/update-adv-com" -> {web_cb, update_adv_com};

    % game editor
        "/update-game-map" ->   {web_cb, update_game_map};
        "/get-game-maps" ->     {web_cb, get_game_maps};
        "/load-game-map" ->     {web_cb, load_game_map};
    
    % customer-groups *
        "/get-customer-groups" ->       {web_cb, get_customer_groups};
        "/get-customer-group-info" ->   {web_cb, get_customer_group_info};
        "/update-customer-group" ->     {web_cb, update_customer_group};
        "/delete-customer-group" ->     {web_cb, delete_customer_group};

    % customers *
        "/get-customers" ->         {web_cb, get_customers};
        "/get-experts" ->           {web_cb, get_experts};
        
        "/get-customer-info" ->     {web_cb, get_customer_info};
        "/update-customer" ->       {web_cb, update_customer};
        "/update-customer/upload-image" ->       {web_file, upload_customer_image};
        
        "/delete-customer" ->       {web_cb, delete_customer};
        "/get-permissions" ->       {web_cb, get_permissions};

    % directories *
        "/get-dirs" ->              {web_cb, get_dirs};
        "/get-conf-questions" ->     {web_cb, get_conf_questions};
        
        "/get-dirs-without" ->      {web_cb, get_dirs_without};
        "/get-dir-sons" ->          {web_cb, get_dir_sons};
        "/get-dir-info" ->          {web_cb, get_dir_info};
        "/get-qdir-info" ->         {web_cb, get_qdir_info};
        "/get-conf-info" ->         {web_cb, get_conf_info};
        "/get-dir-parent-info" ->   {web_cb, get_dir_parent_info};
        "/update-dir" ->            {web_cb, update_dir};
        "/update-conf" ->           {web_cb, update_conf};
        
        
        "/approve-conf-question" ->     {web_cb, approve_conf_question};
        "/delete-conf-question" ->      {web_cb, delete_conf_question};
        
        "/delete-dir" ->            {web_cb, delete_dir};
        "/merge-dirs" ->            {web_cb, merge_dirs};
        
    % docs *
        "/get-docs" ->                  {web_cb, get_docs};
        "/get-test-answers" ->          {web_cb, get_test_answers};
        "/get-docs-recursive" ->        {web_cb, get_docs_recursive};
        "/get-doc-info" ->              {web_cb, get_doc_info};
        "/get-test-answer-info" ->           {web_cb, get_test_answer_info};
        "/delete-doc" ->                {web_cb, delete_doc};
        "/delete-answer" ->             {web_cb, delete_answer};
        "/update-doc" ->                {web_cb, update_doc};
        "/update-test-answer" ->             {web_cb, update_answer};
        "/get-attach-list" ->           {web_cb, get_attach_list};
        
        "/update-doc/upload-image" ->   {web_file, upload_doc_image};
        
        "/update-doc/upload-attach" ->  {web_file, upload_doc_attach};
        "/update-doc/delete-attach" ->  {web_file, delete_doc_attach};
        
    % do_*
        "/do_login" ->          {authorization, do_login};
        "/do_logout" ->         {authorization, do_logout};
        "/do_change_pass" ->    {authorization, do_change_pass};
        "/login" ->             {authorization, login};
            
        "INDEX" ->
            {index, index};
        _ ->
            throw(not_found)
    end.


loop(Req, _DocRoot) ->
    serve_request(Req:get(path), Req).

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

iterKill(0, _) ->
    ok;
iterKill(X, F) ->
    F(),
    iterKill(X-1, F).

test(Fun, X) ->
    Start = utils:utime(),
    iterKill(X, Fun),
    Stop = utils:utime(),
    io:format("::: ~p~n", [Stop - Start]).

test1() ->
    test(fun() -> 
        killSlash("/jopa/kita/pechen/treski/") end, 
    10000),
    test(fun() -> 
        lists:sublist("/jopa/kita/pechen/treski/", 1, length("/jopa/kita/pechen/treski/") - 1) 
    end, 10000).

% ---------------------------------------------------------------------------

killSlash(Path) ->
    killSlash1(Path).

killSlash1("") ->
    "";

killSlash1(Path) ->
    {Res, _ } = lists:split(length(Path) - 1, Path),
    Res.

killSlash2(Path) ->
    Options = [{capture, all, list}],
	case re:run(Path, "(.*)/", Options) of
		{match, [_, Param]}->
			Param;
		nomatch ->
			"" % Пустая строка
	end.
