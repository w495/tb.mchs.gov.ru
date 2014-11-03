-module(sendmail).

-compile(export_all).


%%
%% Английский вариант отправки почты
%%

sendOne([{Destination1}]) ->
    spawn(fun() ->
        D = io_lib:format("~p",[binary_to_list(Destination1)]),
        ResString = utils:sformat("echo \"Ваш вопрос был заблокирован\" | mail -s \"~ts\" ~ts", ["Ваш вопрос был заблокирован!", D]),
        io:format("SEND1 = ~ts ~n", [ResString]),
        os:cmd(ResString)
    end).


%% ============================================================================
%% Нерабочие \ кривые варианты
%% ============================================================================

%% 
%% Destination: список ящиков
%%             [<<"cff@cff.org">>, ...]
%% Subject:    <<"Тема письма">>
%% Body:       <<"Тело письма">>
%% ContentType:<<"text/html">>
%% 

send(Destination, Subject, Body) ->
    spawn(fun() ->
        D = string:join( lists:map( fun(Addr) -> binary_to_list(Addr) end, Destination ), " " ),
        %S = io_lib:format("~p",[binary_to_list(Subject)]),
        %B = io_lib:format("~p",[binary_to_list(Body)]),
        %CT = binary_to_list(ContentType),
        os:cmd("echo " ++ Body ++ " | mail -s " ++ Subject ++ " " ++ D)
    end).

%% --------------------------------------------------------------------------

sendOne([{Destination1}], Subject, Body) ->
    %spawn(fun() ->
        D = io_lib:format("~p",[binary_to_list(Destination1)]),
        S = io_lib:format("~p",[binary_to_list(Subject)]),
        B = io_lib:format("~p",[binary_to_list(Body)]),
        ResString = utils:sformat("echo \"~ts \" | mail -s \"~ts\" ~ts", [B, S, D]),
        os:cmd(ResString),
        io:format("SEND1 = ~s ~n", [ResString ]).
    %end).

%% --------------------------------------------------------------------------

sendOne2([{Destination1}], Subject, Body) ->
    %spawn(fun() ->
        D = io_lib:format("~p",[binary_to_list(Destination1)]),
        ResString = utils:sformat("echo \"~ts \" | mailx -s \"~ts\" ~ts", [Body, Subject, D]),
        os:cmd(ResString),
        io:format("SEND2 = ~s ~n", [ResString]).
    %end).

%% --------------------------------------------------------------------------