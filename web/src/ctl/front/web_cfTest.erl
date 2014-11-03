%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%
%%% TODO: Переписать этот неэффективный жуткий быдлокод. 
%%% Оправдание:
%%%       писал не давно, не сильно понимая что происходит.
%%%       сейчас разобраться не смог.
%%% Вывод: нужно переписать с 0 весь модуль.
%%% Это вариант отличается от *2.erl тем, что возможег только 1 вар ответов.
%%%
    

-module(web_cfTest).
-compile(export_all).

-import(mochiweb_cookies, [cookie/2, cookie/3]).

-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").
    
stripOk(Module, Method, Value)->
    case Module:Method(Value) of
        {ok, Res, _ } -> Res;
        {ok, Res} ->  Res;
        Res -> Res
    end.

details(Req, {Type, Id, Links_Options, Template_Options}) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            details_(Req, {{get, norm}, Id, Links_Options, Template_Options});
        'POST' ->
            details_(Req, {{post, norm}, Id, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.
    
contais({Question, Doc}, List)->
    List_ = proplists:get_value(utils:to_string(proplists:get_value("id", Question)), List),
    io:format("List_  = ~p~n",[List_ ]),
    case List_ of
        undefined -> false;
        QuestionList ->
            lists:any(fun(X) -> utils:to_string(proplists:get_value("id", Doc)) == X end, QuestionList)
    end.

details_(Req, {{post, norm}, Id, Links_Options, Template_Options}) ->
    Data = Req:parse_post(),
    QuestionIds = proplists:get_all_values("questions", Data),    
    UserQuestionAns = [{Question, proplists:get_all_values(Question, Data)} || Question <- QuestionIds],
    details_(Req, {{get, {answer, UserQuestionAns}}, Id, Links_Options, Template_Options});

details_(_Req, {{get, Variant}, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    %
    % TODO: Переписать этот неэффективный жуткий быдлокод. 
    % Оправдание:
    %       писал не давно, не сильно понимая что происходит.
    %       сейчас разобраться не смог.
    % Вывод: нужно переписать с 0 весь модуль.
    %
    
    #web_session{login=Login, customer_id = UID} = authorization:auth_required_front(_Req),
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/test/index-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/test/index-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/test/index.xsl"
            end
    end,
    {ok, Test} = dao_fDirectory:getDir(Id),
    [STest] = Test,
    {ok, Questions} = dao_fDirectory:getTestQuestions({Id}),
    
    case Variant of
        {answer, UserQuestionAns } ->
            QuestionsAns_ =  [ lists:append(Question ,
                [{answers,
                    [
                        lists:flatten([
                            stripOk(dao_fDirectory, getTestAnswer, proplists:get_value("id", ADoc))
                            | [{"user_ch", contais({Question, ADoc}, UserQuestionAns) }]
                        ])
                        || ADoc <- stripOk(dao_fDirectory, getDocs, {proplists:get_value("id", Question)})
                    ]                        
                }]) || Question <- Questions
            ],
            io:format("QuestionsAns = ~p~n", [QuestionsAns_]),
            
            IsRight = fun(VBool, Question) ->
                % вычисляет степень правильность ответа на данный вопрос.
                lists:sum([
                    case {proplists:get_value("user_ch", Answer),
                        proplists:get_value("correct_flag", Answer)} of
                        {VBool, VBool} -> 1; Some ->  0 end
                    || Answer <- proplists:get_value(answers, Question)]
                ) 
            end,

            Glob_r = lists:sum([ IsRight(true, Question) || Question <- QuestionsAns_ ]),
            dao_fCustomer:updateTest2Customer({UID, Id, Glob_r}),
            
            io:format("~n~nGlob_r = ~p~n~n", [Glob_r]),
            
            QuestionsAns = QuestionsAns_ ,
            Test2CustomerLastRes = [[{times, undefined}]],
            Rightness = [[{times, Glob_r}]];
            
        _ ->
            
            Test2CustomerLastRes = dao_fCustomer:getTest2CustomerLastRes({UID, Id}),
            io:format("Test2CustomerLastRes = ~p", [Test2CustomerLastRes]),
            Rightness = [[{times, undefined}]],

            QuestionsAns =  [
                    lists:flatten([Question | [{answers, stripOk(dao_fDirectory, getDocs, {proplists:get_value("id", Question)})}]])
                    || Question <- Questions
            ]
    end,
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            {"self-retpath", _Req:get(path)},
            {"login", Login},
            {"item-parent-path", ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {one, Test, "doc"},                     % описание теста
            {one, Test2CustomerLastRes, "test-last-res"},     % глобальная правильность теста
            {one, Rightness,            "test-rightness"},     % глобальная правильность теста
            {list, QuestionsAns, "questions"},      % список вопросов
            {one, Meta, "meta"}                     % описание запроса
        ]
    ),
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.


index(_Req, {Type, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    #web_session{login=Login} = authorization:auth_required_front(_Req),

    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/test/index-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/test/index-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/test/index.xsl"
            end
    end,
    
    Meta = [[
            {"current-path", _Req:get(path)},
            {"parent-path", SelfParent},
            {"self-parent-path", SelfParent},
            {"self-retpath", _Req:get(path)},
            
            {"login", Login},
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
