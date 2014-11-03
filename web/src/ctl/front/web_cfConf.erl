%%% @file web_cb.erl
%%%
%%%     Контроллеры front end
%%%

-module(web_cfConf).
-include("../include/web_session.hrl").
-include("../include/common.hrl").
-include("../include/dao.hrl").

-compile(export_all).

%% Разрешенные действия:
%%
%% index/1, index/2,
%% list/1, list/2,
%% details/1, details/2,
%% edit/1, edit/2


stripOk(Module, Method, Value)->
    case Module:Method(Value) of
        {ok, Res, _ } -> Res;
        {ok, Res} ->  Res;
        Res -> Res
    end.

contais({Question, Doc}, List)->
    List_ = proplists:get_value(utils:to_string(proplists:get_value("id", Question)), List),
    io:format("List_  = ~p~n",[List_ ]),
    case List_ of
        undefined -> false;
        QuestionList ->
            lists:any(fun(X) -> utils:to_string(proplists:get_value("id", Doc)) == X end, QuestionList)
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

is_Expert(Login, [Expert | Experts_Tail ]) ->
    case proplists:get_value("login", Expert) of
        Login -> true;
        _ -> is_Expert(Login, Experts_Tail )
    end;
is_Expert(Login, []) -> false.

details_(Req, {{post, norm}, Id, Links_Options, Template_Options}) ->
    Data = Req:parse_post(),
    QuestionIds = proplists:get_all_values("questions", Data),    
    UserQuestionAns = [{Question, proplists:get_all_values(Question, Data)} || Question <- QuestionIds],
    details_(Req, {{get, {answer, UserQuestionAns}}, Id, Links_Options, Template_Options});

details_(_Req, {{get, Variant}, Id, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    Login = authorization:auth_getlogin(_Req),
    
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/conf/details-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/conf/details-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/conf/details.xsl"
            end
    end,
    
    {ok, Conf, Attaches, ExpertIds} = dao_fDirectory:getConfA(Id),
    [SConf] = Conf,
    
    ConfId = proplists:get_value("id", SConf),
    {ok, RawQuestions} = dao_fDirectory:getDirsWithCustomerP({ConfId}),
    
    
    %% вопросы с полем same --- внтури вложенные вопросы
    QuestionsWithSame = [
        lists:flatten([
            RawQuestion | [{
                "same", stripOk(dao_fDirectory, getDirsWithCustomerP, {proplists:get_value("id", RawQuestion)})
            }]
        ])
        || RawQuestion <- RawQuestions
    ],
    
    %% вопросы с полем ответа 
    QuestionsWithSameWithAns = [
        lists:flatten([
            QuestionWithSame | [{
                "answer", stripOk(dao_fDirectory, getDocsWithCustomerA, {proplists:get_value("id", QuestionWithSame)})
            }]
        ])
        || QuestionWithSame <- QuestionsWithSame
    ],
    
    Experts = [ lists:flatten(stripOk(dao_fCustomer, getCustomer, ExpertId)) || ExpertId <- ExpertIds ],
    
    Meta = [[
            {"current-path", _Req:get(path)},
            {"self-parent-path", SelfParent},
            {"self-retpath", _Req:get(path)},
            
            {"login", Login},
            {"login-is-expert", is_Expert(Login, Experts)},
            {"item-parent-path", ItemParent},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {one, Conf, "doc"},                     % описание теста
            {list, Experts, "experts"},
            {list, QuestionsWithSameWithAns, "questions"},
            {one, Meta, "meta"}                     % описание запроса
        ]
    ),

    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.



list(_Req, {_, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    Login = authorization:auth_getlogin(_Req),
    
    case utils:to_integer(proplists:get_value("print", _Req:parse_qs())) of
        1   ->                   XslPath = XslRoot ++ "/conf/list-print.xsl";
        _   ->  case _Req:get_cookie_value(?SPEC) of
                "true"  ->       XslPath = XslRoot ++ "/conf/list-spec.xsl";
                _       ->       XslPath = XslRoot ++ "/conf/list.xsl"
            end
    end,
    
    TargetId = utils:getTargetId(Param, ?CONF_ID),
    {ok, DirList} = dao_fDirectory:getConfs({TargetId}),
    
    Meta = [[
            {"current-path", _Req:get(path)},
            {"self-parent-path", SelfParent},
            {"item-parent-path", ItemParent},
            {"self-retpath", _Req:get(path)},
            
            {"login", Login},
            {"is-spec",             _Req:get_cookie_value(?SPEC)},
            {"spec-variant",        _Req:get_cookie_value(?VARIANT)},
            {"spec-color",          _Req:get_cookie_value(?COLOR)}
    ]],
    XMLTerms  = pg2xml:encodeData(
        [
            {list, DirList, "dirs"},    % список папок
            {one, Meta, "meta"}         % описание запроса
        ]
    ),
    Outty = xslt:apply(XslPath, XMLTerms),
    {"text/html", [], [Outty]}.

%%
%% Позволяет задать на вопрос
%%
ask(Req, {Var, Param, Links_Options, Template_Options}) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            throw({redirect, Param, []});
        'POST' ->
            ask_(Req, {Var, Param, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.

ask_(Req, {_, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    case authorization:auth_getUID(Req) of
        undefined ->
            UID = undefined,
            throw({auth_required_front,  Req:get(path)});
        Uid -> UID = Uid
    end,
    Data = Req:parse_post(),
    Conf_id = utils:to_integer(proplists:get_value("id", Data)),
    Content = proplists:get_value("content", Data),
    
    io:format("~n~n@@@@updateConfQuestion= ~s ~s~n~n", [Content, Content]),
    
    dao_fDirectory:updateConfQuestion({{null, Content, Content}, {Conf_id}, UID}),
    throw({redirect, Param, []}).

%%
%% Позволяет ответить на вопрос
%%
answer(Req, {Var, Param, Links_Options, Template_Options}) ->
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            throw({redirect, Param, []});
        'POST' ->
            answer_(Req, {Var, Param, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.

answer_(Req, {_, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    FileHandler = fun(Filename, ContentType) -> web_file:handle_file(Filename, ContentType) end,
    
    FileForm = mochiweb_multipart:parse_form(Req, FileHandler),

    Question_id = proplists:get_value("id", FileForm),
    Content = proplists:get_value("content", FileForm),
    
    File_types = proplists:get_all_values("file-type", FileForm),
    Files = proplists:get_all_values("file", FileForm),
    
    %io:format("~nQuestion_id = ~p~n", [Question_id]),
    %io:format("~nFiles = ~p~n", [Files]),
    %io:format("~nFile_types = ~p~n", [File_types]),
        
    AnsId = dao_fDirectory:updateConfAnswer({{null, Content, Content}, {Question_id}, UID}),
    
    case Files of
        [{[], _ , _ }]
            -> ok;
        _
            -> web_file:upload_answer_files(Files, File_types, AnsId, UID)
    end,
    
    throw({redirect, Param, []}).

%%
%% Объединяет вопросы в один.
%%
mergeQuestions(Req, {Var, Param, Links_Options, Template_Options}) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            throw({redirect, Param, []});
        'POST' ->
            mergeQuestions_(Req, {Var, Param, Links_Options, Template_Options});
        _ ->
            Req:respond({501, [], []})
    end.

mergeQuestions_(Req, {_, Param, [SelfParent, ItemParent | _ ],   [XslRoot | _ ]}) ->
    #web_session{customer_id=UID} = authorization:auth_required(Req),
    Data = Req:parse_post(),

   case catch(norm:extr(Data, [{"current-id", [integer]}, {"choosen-id", [integer]} ])) of
        {error, _} -> ok;
        Info  ->
            io:format("~n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~nData = ~p~n<<<<<<<<<<<<<<<<<<<<~n", [Info]),
            Res = dao:daoCall(dao_fDirectory, mergeDirs, Info)
    end,
    throw({redirect, Param, []}).
