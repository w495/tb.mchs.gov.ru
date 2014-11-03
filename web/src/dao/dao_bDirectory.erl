%%% @file dao_bDirectory.erl
%%%
%%%     Доступ к данным для сущностей директория и документ.
%%%     Версия для админки
%%%

-module(dao_bDirectory).

-include("../include/common.hrl").
-include("../include/dao.hrl").

-compile(export_all).

getConfQuestions({Parent_dir_id}) ->
    Q1 = "select directory.id, directory.name, directory.parent_dir_id, "
            " directory.datatime, directory.updater, "
            " document.published as published, "
            " directory.doc_description_id, directory.dir_type_id "
            " from directory, document "
            " where directory.deleted = false "
            "   and directory.Parent_dir_id = $1 "
            "   and document.doc_type_id = $2 "
            "   and document.dir_id = directory.id "
            "   and directory.Parent_dir_id != directory.id; ",
    dao:simple(Q1, [utils:to_integer(Parent_dir_id), utils:to_integer(?DESCR_ID)]).

getDirs({Parent_dir_id}) ->
    Q1 = "select directory.id, directory.name, directory.parent_dir_id, "
            "directory.datatime, directory.updater, "
            "directory.doc_description_id, directory.dir_type_id "
            "from directory "
            "where directory.deleted = false "
            "   and directory.Parent_dir_id = $1"
            "   and directory.Parent_dir_id != id;",
    dao:simple(Q1, [utils:to_integer(Parent_dir_id)]);

getDirs({Parent_dir_id, Cur_id}) ->
    Q1 = "select directory.id, directory.name, directory.parent_dir_id, "
            "directory.datatime, directory.updater, "
            "directory.doc_description_id, directory.dir_type_id "
            "from directory "
            "where directory.deleted = false "
            "   and directory.Parent_dir_id = $1 "
            "   and directory.id != $2 "
            "   and directory.Parent_dir_id != id;",
    dao:simple(Q1, [utils:to_integer(Parent_dir_id), utils:to_integer(Cur_id)]).
    
    

%%
%% Возвращает директории-детей директории с описанием (content) каждой из них.
%%
getDirSons({Id}) ->
    Q1 = "select d.id, d.name, d.parent_dir_id, "
            " d.datatime, d.updater, "
            " d.doc_description_id, d.dir_type_id "
            " from directory as pd, directory as d"
            " where pd.parent_dir_id = $1 "
                " and d.deleted = false "
                " and pd.deleted = false "
                " and d.parent_dir_id != pd.parent_dir_id "
                " and d.parent_dir_id = pd.id "
                " and d.id != pd.id; ",
    dao:simple(Q1, [utils:to_integer(Id)]).


%%
%% Возвращает директорию с описанием (content).
%%
getDir(Id) ->
    Q1 = " select directory.id, directory.name, directory.parent_dir_id, "
            " directory.datatime, directory.updater, "
            " directory.doc_description_id, directory.dir_type_id, "
            " document.content as content, "
            " document.pic_url as pic_url "
            "from directory "
            "inner join document "
                "on directory.id = $1 "
                    "and document.dir_id = directory.id "
                    "and document.doc_type_id = $2; ",
    dao:simple(Q1, [utils:to_integer(Id), ?DESCR_ID]).

%%
%% Возвращает директорию с описанием (content) и аттачами
%%
getDirA(Id) ->
    Q1 = " select directory.id, directory.name, directory.parent_dir_id, "
        " directory.datatime, directory.updater, "
        " directory.doc_description_id, directory.dir_type_id, "
        " document.content as content, "
        " document.pic_url as pic_url "
        "from directory "
        "inner join document "
            "on directory.id = $1 "
                "and document.dir_id = directory.id "
                "and document.doc_type_id = $2; ",
    Res  = dao:simple(Q1, [utils:to_integer(Id), ?DESCR_ID]),
    case Res   of
        {ok, R1Val} ->
            [R1ValS] = R1Val,
            Doc_description_id = proplists:get_value("doc_description_id", R1ValS),
            Q2 = "select "
                ?ATTACHE_COMMON
                "  from attach2doc "
                "where doc_id  = $1",
            case dao:simple(Q2, [utils:to_integer(Doc_description_id)]) of
                {ok, R2Val} -> {ok, R1Val, R2Val};
                E2 -> E2
            end;
        E1 -> E1
    end.

getQDirA(Id) ->
    Q1 = "select directory.id, directory.name, directory.parent_dir_id, "
        "directory.datatime, directory.updater, "
        "directory.doc_description_id, directory.dir_type_id, "
        "document.content as content, "
        " document.pic_url as pic_url "
        "from directory "
        "inner join document "
            "on directory.id = $1 "
                "and document.dir_id = directory.id "
                "and document.doc_type_id = $2; ",
    Res  = dao:simple(Q1, [utils:to_integer(Id), ?DESCR_ID]),
    case Res   of
        {ok, R1Val} ->
            [R1ValS] = R1Val,
            Doc_description_id = proplists:get_value("doc_description_id", R1ValS),
            Q2 = "select "
                ?ATTACHE_COMMON
                "  from attach2doc "
                "where doc_id  = $1",
            case dao:simple(Q2, [utils:to_integer(Doc_description_id)]) of
                {ok, R2Val} ->
                    Q3 = "select count(*) as docs from document where dir_id = $1 and doc_type_id = 2;",
                    case dao:simple(Q3, [utils:to_integer(Id)]) of
                        {ok, R3Val} ->  {ok, R1Val, R2Val, R3Val};
                        E3 -> E3
                    end;
                E2 -> E2
            end;
        E1 -> E1
    end.

getConfA(Id) ->
    Q1 = " select directory.id, directory.name, directory.parent_dir_id, "
        " directory.datatime, directory.updater, "
        " directory.doc_description_id, directory.dir_type_id, "
        " conference.start as start, "
        " conference.stop as stop, "
        " document.content as content, "
        " document.pic_url as pic_url "
        " from directory, conference, document "
        " where "
            " directory.id = $1 "
                " and conference.conf_id = directory.id "
                " and document.dir_id = directory.id "
                " and document.doc_type_id = $2; ",
                
    io:format("Q1 = ~p~n", [Q1]),
    Res  = dao:simple(Q1, [utils:to_integer(Id), ?DESCR_ID]),
    io:format("Res = ~p~n", [Res]),
    case Res   of
        {ok, R1Val} ->
            [R1ValS] = R1Val,
            Doc_description_id = proplists:get_value("doc_description_id", R1ValS),
            Q2 = "select "
                ?ATTACHE_COMMON
                "  from attach2doc "
                "where doc_id  = $1",
            case dao:simple(Q2, [utils:to_integer(Doc_description_id)]) of
                {ok, R2Val} -> % {ok, R1Val, R2Val};
                    Q3 = "select cg.customer_id as id  from  "
                            " permission as p_conf,  "
                            " customer_group as c_group, "
                            " customer2group as cg, "
                            " permission2group as pg "
                        " where "
                            " p_conf.entity_id = $1 "
                            " and pg.perm_id = p_conf.id "
                            " and pg.group_id = c_group.id "
                            " and cg.group_id = c_group.id;",
                    io:format("Q3 = ~p~n", [Q3]),
                    case dao:simple(Q3, [utils:to_integer(Id)]) of
                        {ok, R3Val} ->
                            io:format("~nR3Val = ~p~n", [R3Val]),
                            {ok, R1Val, R2Val, [X || [{"id", X}] <- R3Val]};
                        E3 -> E3
                    end;
                E2 -> E2
            end;
        E1 -> E1
    end.


getDirMinInfo(Id) ->
    Q1 = "select directory.id, directory.name, directory.dir_type_id "
            "from directory where id = $1; ",
    dao:simple(Q1, [utils:to_integer(Id)]).

%%
%% Создает пустую директорию с описанием
%%
%% Id = null
%% Doc_description_id = null
%%

updateDir({{null, Name, Content}, {Pic_url}, {Parent_dir_id, Dir_type_id, null}, UID}) ->
    Q1 =    "insert into directory "
                "(name, parent_dir_id, dir_type_id, updater, deleted) "
            "values ($1, $2, $3, $4, false) returning directory.id;",
    Q2 =    "insert into document "
                "(name, content, dir_id, doc_type_id, updater, published, pic_url) "
            "values ($1, $2, $3, $4, $5, true, $6) returning document.id;",
    Q3 =    "update directory set Doc_description_id  = $2,"
            " updater = $3 where id = $1;",
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1, _, [{Dir_Id}]} = pgsql:equery(Con, Q1,
                [Name, Parent_dir_id, Dir_type_id, UID]),
            {ok, 1, _, [{Doc_Id}]} = pgsql:equery(Con, Q2,
                [?DESCR_NAME_PREFFIX ++ Name, Content, Dir_Id, ?DESCR_ID, UID, Pic_url]),
            {ok, 1} = pgsql:equery(Con, Q3, [Dir_Id, Doc_Id, UID]),

            % log new directory. не важно какая скорость, так что честно берем объект из базы.
            {ok, [Dir]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Dir_Id])),
            dao_bLog:logDirCreate(Dir, UID, Con),
            ok
        end),
    dao:processPGRet2(Ret);

updateDir({{null, Name, Content, Published}, {Pic_url}, {Parent_dir_id, Dir_type_id, null}, UID}) ->
    Q1 =    "insert into directory "
                "(name, parent_dir_id, dir_type_id, updater, deleted) "
            "values ($1, $2, $3, $4, false) returning directory.id;",
            
    Q2 =    "insert into document "
                "(name, content, dir_id, doc_type_id, updater, published, pic_url) "
            "values ($1, $2, $3, $4, $5, $7, $6) returning document.id;",
    Q3 =    "update directory set Doc_description_id  = $2,"
            " updater = $3 where id = $1;",
            
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1, _, [{Dir_Id}]} = pgsql:equery(Con, Q1,
                [Name, Parent_dir_id, Dir_type_id, UID]),
            {ok, 1, _, [{Doc_Id}]} = pgsql:equery(Con, Q2,
                [?DESCR_NAME_PREFFIX ++ Name, Content, Dir_Id, ?DESCR_ID, UID, Pic_url, Published]),
            {ok, 1} = pgsql:equery(Con, Q3, [Dir_Id, Doc_Id, UID]),

            % log new directory. не важно какая скорость, так что честно берем объект из базы.
            {ok, [Dir]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Dir_Id])),
            dao_bLog:logDirCreate(Dir, UID, Con),
            ok
        end),
    dao:processPGRet2(Ret);

updateDir({{Id, Name, Content}, {Pic_url}, {Parent_dir_id, Dir_type_id, Doc_description_id}, UID}) ->
    %
    % Doc_description_id берется из аргументов. Так проще и быстрее.
    %      Иначе мы можем его вычислять по directory.id.
    %      Но считаем, что у директории уже есть описание.

    Q1 =    "update directory set name = $2, parent_dir_id = $3, "
                "dir_type_id = $4, doc_description_id = $5, updater = $6 "
            "where id = $1;",
    Q2 =    "update document set content = $2, updater = $3, pic_url = $4 "
            "where id = $1;",

    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, [Old]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Id])),

            {ok, 1} = pgsql:equery(Con, Q1, [Id, Name, Parent_dir_id,
                Dir_type_id, Doc_description_id, UID]),
            {ok, 1} = pgsql:equery(Con, Q2, [Doc_description_id, Content, UID, Pic_url]),

            % log update directory. не важно какая скорость, так что честно берем объект из базы.
            {ok, [New]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Id])),
            dao_bLog:logDirUpdate(Old, New, UID, Con),
            ok
        end),
    dao:processPGRet2(Ret);

updateDir({{Id, Name, Content, Published}, {Pic_url}, {Parent_dir_id, Dir_type_id, Doc_description_id}, UID}) ->
    %
    % Doc_description_id берется из аргументов. Так проще и быстрее.
    %      Иначе мы можем его вычислять по directory.id.
    %      Но считаем, что у директории уже есть описание.

    Q1 =    "update directory set name = $2, parent_dir_id = $3, "
                "dir_type_id = $4, doc_description_id = $5, updater = $6 "
            "where id = $1;",
    Q2 =    "update document set content = $2, updater = $3, pic_url = $4, published = $5"
            "where id = $1;",

    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, [Old]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Id])),

            {ok, 1} = pgsql:equery(Con, Q1, [Id, Name, Parent_dir_id,
                Dir_type_id, Doc_description_id, UID]),
            {ok, 1} = pgsql:equery(Con, Q2, [Doc_description_id, Content, UID, Pic_url, Published]),

            % log update directory. не важно какая скорость, так что честно берем объект из базы.
            {ok, [New]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Id])),
            dao_bLog:logDirUpdate(Old, New, UID, Con),
            ok
        end),
    dao:processPGRet2(Ret).

mergeDirs({CurentId, CoosenId}) ->
    Qselect  = "select id from document where doc_type_id = 2 and dir_id = $1",
    QupdateP = "update directory set parent_dir_id = $1 where id = $2;",
    QupdateC = "update directory set parent_dir_id = $1 where parent_dir_id  = $2;",
    
    io:format("~n~n~n~n~n~n(~p) => (~p)~n~n~n~n~n~n", [CoosenId, CurentId]),

    Ret = dao:withTransactionMchs(fun(Con) ->
            case pgsql:equery(Con, Qselect, [CurentId]) of
                {ok, _, []} ->
                    {ok, 1} = pgsql:equery(Con, QupdateP, [CoosenId, CurentId]),
                    {ok, _} = pgsql:equery(Con, QupdateC, [CoosenId, CurentId]);
                SSS -> io:format("=> (~p)~n~n~n~n~n~n", [SSS])
            end
        end),
    dao:processPGRet2(Ret).
    
updateConf({{null, Name, Content, Start, Stop}, {Parent_dir_id, Dir_type_id, null}, UID, ExpertList}) ->
    % Конференция
    Q1 =    "insert into directory "
                "(name, parent_dir_id, dir_type_id, updater, deleted) "
            "values ($1, $2, $3, $4, false) returning directory.id;",
    % Конференция
    Q11 =    "insert into conference "
                "(conf_id, start, stop) "
            "values ($1, $2, $3);",
    % Дескриптор
    Q2 =    "insert into document "
                "(name, content, dir_id, doc_type_id, updater, published) "
            "values ($1, $2, $3, $4, $5, true) returning document.id;",
    Q3 =    "update directory set Doc_description_id  = $2,"
            " updater = $3 where id = $1;",
    % Право эксперта
    Qp =    "insert into permission "
                " (name, description, entity_id, perm_type_id) "
            " values ($1, $2, $3, "
                " (select id from permission_type where name='expert_conf'))"
                "returning permission.id;" ,
    % Группа экспертов
    Qg =    "insert into customer_group "
                " (name, description) "
            " values ($1, $2) returning customer_group.id;",
    % Право и группа
    Qp2g =    "insert into permission2group "
                " (perm_id, group_id) "
            " values ($1, $2); ",
    % Аффтар и группа
    Qc2g =    "insert into customer2group "
                " (customer_id, group_id) "
            " values ($1, $2); ",
            
            
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1, _, [{Dir_Id}]} = pgsql:equery(Con, Q1,
                [Name, Parent_dir_id, Dir_type_id, UID]),
            {ok, 1, _, [{Doc_Id}]} = pgsql:equery(Con, Q2,
                [?DESCR_NAME_PREFFIX ++ Name, Content, Dir_Id, ?DESCR_ID, UID]),
            {ok, 1} = pgsql:equery(Con, Q3, [Dir_Id, Doc_Id, UID]),
            {ok, 1, _, [{Perm_Id}]} = pgsql:equery(Con, Qp,
                ["expert_conf_" ++ utils:to_string(Dir_Id), "эксперт " ++ Name, Dir_Id]),
            {ok, 1, _, [{Group_Id}]} = pgsql:equery(Con, Qg,
                ["эксперты конференции " ++ Name, "группа экспертов конференции " ++ Name]),
            case length(ExpertList) of
                0 -> ok;
                ExpertListL ->
                    Qexp = "insert into customer2group (customer_id, group_id) values " ++
                        string:join([lists:flatten(io_lib:format("(~p, ~p)",
                            [X, Group_Id])) || X <- ExpertList], ", "),
                    {ok, ExpertListL} = pgsql:equery(Con, Qexp, [])
            end,
            
            {ok, _} = pgsql:equery(Con, Q11, [Dir_Id,
                utils:unixtime_to_localDate(Start),
                utils:unixtime_to_localDate(Stop)]),
            {ok, _ } = pgsql:equery(Con, Qp2g, [Perm_Id, Group_Id]),
            {ok, _ } = pgsql:equery(Con, Qc2g, [UID, Group_Id])
        end),
    dao:processPGRet2(Ret);

updateConf({{Id, Name, Content, Start, Stop}, {Parent_dir_id, Dir_type_id, Doc_description_id}, UID, ExpertList}) ->
    %
    % Doc_description_id берется из аргументов. Так проще и быстрее.
    %      Иначе мы можем его вычислять по directory.id.
    %      Но считаем, что у директории уже есть описание.

    Q1 =    "update directory set name = $2, parent_dir_id = $3, "
                "dir_type_id = $4, doc_description_id = $5, updater = $6 "
            "where id = $1;",
    Q11 =   "update conference set start = $2, stop = $3 where conf_id = $1",
    Q2 =    "update document set content = $2, updater = $3 "
            "where id = $1;",
    Qgi =    " select c_group.id "
            " from "
                " permission as p_conf, "
                " customer_group as c_group, "
                " permission2group as pg "
            " where "
                " p_conf.entity_id = $1 "        
                 " and pg.perm_id = p_conf.id "
                 " and pg.group_id = c_group.id; ",
    Qdc2g = "delete from customer2group where group_id = $1;",
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1} = pgsql:equery(Con, Q1, [Id, Name, Parent_dir_id,
                Dir_type_id, Doc_description_id, UID]),
                
            {ok, 1} = pgsql:equery(Con, Q11, [Id,
                utils:unixtime_to_localDate(Start),
                utils:unixtime_to_localDate(Stop)
            ]),
                
            {ok, 1} = pgsql:equery(Con, Q2, [Doc_description_id, Content, UID]),
            
            io:format("Id = ~p~n", [Id]),
            io:format("Qgi = ~p~n", [Qgi]),
            io:format("ExpertList = ~p~n", [ExpertList]),
            
            {ok,_,[{Group_Id}]} = pgsql:equery(Con, Qgi, [Id]),
            {ok, _} = pgsql:equery(Con, Qdc2g, [Group_Id]),
            
            case length(ExpertList) of
                0 -> ok;
                ExpertListL ->
                    Q3 = "insert into customer2group (customer_id, group_id) values " ++
                        string:join([lists:flatten(io_lib:format("(~p, ~p)",
                            [X, Group_Id])) || X <- ExpertList], ", "),
                    {ok, ExpertListL} = pgsql:equery(Con, Q3, [])
            end
            
        end),
    dao:processPGRet2(Ret).

%%
%% Рекурсивное якобы удаление якобы директорий в рамках одной транзакции.
%%
deleteDir(Con, {Id, UID}) ->
    Q1 = "update directory set deleted = true, updater = $2 where id = $1;",
    {ok, 1} = pgsql:equery(Con, Q1, [Id, UID]),
    Q2 = "select id from directory where directory.parent_dir_id = $1;",
    {ok,_,List} = pgsql:equery(Con, Q2, [Id]),

    {ok, [Dir]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Id])),
    dao_bLog:logDirDelete(Dir, UID, Con),
    deleteDirs(Con, List, UID).

deleteDirs(Con, [{Id}|Rest], UID) ->
    deleteDir(Con, {Id, UID}),
    deleteDirs(Con, Rest, UID);

deleteDirs(_, [], _) -> ok.

deleteConfQuestion(Con, {Id, UID}) ->
    Q1 = "update directory set deleted = true, updater = $2 where id = $1;",
    {ok, 1} = pgsql:equery(Con, Q1, [Id, UID]),
    Q2 = "select id from directory where directory.parent_dir_id = $1;",
    {ok,_,List} = pgsql:equery(Con, Q2, [Id]),

    {ok, [Dir]} = dao:processPGRet2(pgsql:equery(Con, "select * from directory where id=$1;", [Id])),
    
    QCustomer = "select "
                " customer.email "
             " from "
                " customer, directory "
             " where "
                " customer.id = directory.updater "
                " and directory.id = $1; ",
    {ok,_, EmailList} = pgsql:equery(Con, QCustomer, [Id]),

    QCity = "select "
                " customer.city "
             " from "
                " customer, directory "
             " where "
                " customer.id = directory.updater "
                " and directory.id = $1; ",
    {ok,_, QCityList } = pgsql:equery(Con, QCity, [Id]),
        
    [{QCityI}] = QCityList,
    
    sendmail:sendOne(EmailList),
    
    io:format("~n~nEmailList = ~p~n~n", [EmailList]),
    
    dao_bLog:logDirDelete(Dir, UID, Con),
    deleteDirs(Con, List, UID).



%%
%% Удаляет директорию
%%
deleteDir({Id, UID}) ->
    Ret = dao:withTransactionMchs(fun(Con) -> deleteDir(Con, {Id, UID}) end),
    dao:processPGRet2(Ret).

deleteConfQuestion({Id, UID}) ->
    Ret = dao:withTransactionMchs(fun(Con) -> deleteConfQuestion(Con, {Id, UID}) end),
    dao:processPGRet2(Ret).

approveConfQuestion({Dir_id, UID}) ->
    QApprove = "update "
        " document "
        " set "
            " published = true "
        " where "
            " dir_id = $1 "
            " and doc_type_id = $2 ",
    dao:simple(QApprove, [utils:to_integer(Dir_id), utils:to_integer(?DESCR_ID)]).
    
%%
%% Возвращает описание директории
%%
getDocs({Dir_id, descr}) ->
    getDocs({Dir_id, ?DESCR_ID});

%%
%% Возвращает содержащиеся в директории news документы
%%
getDocs({news, doc}) ->
    getDocs({?NEWS_ID, ?DOC_ID });

%%
%% Возвращает содержащиеся в директории документы
%%
getDocs({term, doc}) ->
    getDocs({?TERM_ID, ?DOC_ID, termin});

%%
%% Возвращает содержащиеся в директории документы
%%
getDocs({Dir_id, doc}) ->
    getDocs({Dir_id, ?DOC_ID});

%%
%% 
%%
getDocs({Dir_id, Doc_type_id, termin}) ->
    Q1 = "select "
                ?DOC_MAIN_MIN ","
                ?DOC_PUD  ","
                ?DOC_URLS ","
                ?DOC_IDS 
            "from document, directory "
            "where "
                "document.dir_id = directory.id "
                "and (directory.id = $1 or directory.parent_dir_id = $1) "
                "and doc_type_id = $2;",
    dao:simple(Q1, [utils:to_integer(Dir_id), utils:to_integer(Doc_type_id)]);

%%
%% Возвращает содержащиеся в директории обощенные документы
%%
getDocs({Dir_id, Doc_type_id}) ->
    Q1 = "select"
                ?DOC_MAIN_MIN ","
                ?DOC_PUD  ","
                ?DOC_URLS ","
                ?DOC_IDS 
            "from document "
                "where " % deleted = false
                "   dir_id = $1"
                "   and doc_type_id = $2;",
    dao:simple(Q1, [utils:to_integer(Dir_id), utils:to_integer(Doc_type_id)]).
    
%%
%% Возвращает содержащиеся в директории обощенные документы
%%      и связанные с ними тесты
%%
getTestAnswers({Dir_id, doc}) ->
    getTestAnswers({Dir_id, ?DOC_ID});

getTestAnswers({Dir_id, Doc_type_id}) ->
    Q1 = "select"
                ?DOC_MAIN_MIN ","
                ?DOC_PUD  ","
                ?DOC_URLS ","
                ?DOC_IDS ","
                " quest_answer.correct_flag "
            " from document, quest_answer "
                " where " % deleted = false
                "   document.dir_id = $1 "
                "   and quest_answer.answer_id = document.id "
                "   and document.doc_type_id = $2; ",
    io:format("Q1 = ~s~n", [Q1]),
    dao:simple(Q1, [utils:to_integer(Dir_id), utils:to_integer(Doc_type_id)]).
    
% getDocsRecursive
% ============================================================================

%%
%% Рекурсивный обход подпапок и предоставдение списка документов.
%%
getDocsRecursive(Con, Id) ->
    Q1 = "select "
                ?DOC_MAIN_MIN ","
                ?DOC_PUD  ","            
                ?DOC_URLS ","
                ?DOC_IDS 
            "from document "
            "where " 
            "   dir_id = $1"
            "   and doc_type_id = 2;",

    {ok,FwdItem,DocList} = pgsql:equery(Con, Q1, [Id]),
    Q2 = "select directory.id "
            " from directory "
            " where "
                " directory.parent_dir_id = $1 "
                " and directory.deleted = false"
                " and directory.parent_dir_id != id; ",
    {ok,_,DirList} = pgsql:equery(Con, Q2, [Id]),
    ResList = lists:append([DocList, getDocsRecursiveRest(Con, DirList)]),
    {FwdItem, ResList}.

getDocsRecursiveRest(_, []) -> [];
getDocsRecursiveRest(Con, [{Id}|Rest]) ->
    {_, DocList} = getDocsRecursive(Con, Id),
    ResList = lists:append([DocList, getDocsRecursiveRest(Con, Rest)]),
    ResList.

getDocsRecursive_(Con, Id) ->
    {FwdItem, ResList} = getDocsRecursive(Con, Id),
    {ok, FwdItem, ResList}.


%%
%% Рекурсивный обход подпапок и предоставдение списка документов,
%%  которые начинаются с FirstLeter.
%%  Мы сознантельно не вводим переменную Condition, для таких случаев.
%%  Возмножно придется уменьшать число полей.
getDocsRecursive(Con, Id, FirstLeter) ->
    Q1 = string:join([
        "select "
          ?DOC_MAIN_MIN ","
          ?DOC_PUD  ","
          ?DOC_URLS ","
          ?DOC_IDS 
        "from document "
        " where " 
          " document.dir_id = $1"
          " and document.doc_type_id = 2 "
          " and document.name like '", FirstLeter, "%'; "], ""),

    io:format("Q1 = ~p~n", [Q1]),

    {ok,FwdItem,DocList} = pgsql:equery(Con, Q1, [Id]),
    Q2 = "select directory.id "
            " from directory "
            " where "
                " directory.parent_dir_id = $1 "
                " and directory.deleted = false"
                " and directory.parent_dir_id != id; ",
    {ok,_,DirList} = pgsql:equery(Con, Q2, [Id]),
    ResList = lists:append([DocList, getDocsRecursiveRest(Con, DirList, FirstLeter)]),
    {FwdItem, ResList}.

getDocsRecursiveRest(_, [], _) -> [];
getDocsRecursiveRest(Con, [{Id}|Rest], FirstLeter) ->
    {_, DocList} = getDocsRecursive(Con, Id, FirstLeter),
    ResList = lists:append([DocList, getDocsRecursiveRest(Con, Rest, FirstLeter)]),
    ResList.

getDocsRecursive_(Con, Id, FirstLeter) ->
    {FwdItem, ResList} = getDocsRecursive(Con, Id, FirstLeter),
    {ok, FwdItem, ResList}.


%%
%% Возвращает все документы директориии (общий случай)
%%
getDocsRecursive({Id}) ->
    Ret = dao:withTransactionMchs(fun(Con) -> getDocsRecursive_(Con, Id) end),
    dao:processPGRet2(Ret);

%%
%% Возвращает все документы директориии, которые начинаются с FirstLeter
%%    
getDocsRecursive({Id, FirstLeter}) ->
    Ret = dao:withTransactionMchs(fun(Con) -> getDocsRecursive_(Con, Id, FirstLeter) end),
    dao:processPGRet2(Ret).

%%
%% Возвращает документ с аттачами
%%
getDoc(Id) ->
    Q1 = "select "
            ?DOC_COMMON
            " from document "
                " where id = $1; ",
    case dao:simple(Q1, [utils:to_integer(Id)]) of
        {ok, R1Val} ->
            Q2 = "select "
                ?ATTACHE_COMMON
                "  from attach2doc "
                "where doc_id  = $1",
            case dao:simple(Q2, [utils:to_integer(Id)]) of
                {ok, R2Val} -> {ok, R1Val, R2Val};
                E2 -> E2
            end;
        E1 -> E1
    end.

getTestAnswer(Id) ->
    Q1 = "select "
            ?DOC_COMMON
            ", quest_answer.correct_flag as correct_flag "
            " from document, quest_answer"
                " where id = $1 "
                "   and quest_answer.answer_id = id ; ",    
    case dao:simple(Q1, [utils:to_integer(Id)]) of
        {ok, R1Val} ->
            Q2 = "select "
                ?ATTACHE_COMMON
                "  from attach2doc "
                "where doc_id  = $1",
            case dao:simple(Q2, [utils:to_integer(Id)]) of
                {ok, R2Val} -> {ok, R1Val, R2Val};
                E2 -> E2
            end;
        E1 -> E1
    end.

updateDoc({{null, Name, Content, Published}, {Pic_url}, {Dir_id, Doc_type_id}, UID}) ->
    
    Q1 =    "insert into document "
                "(name, content, dir_id, doc_type_id, updater, published) "
            "values ($1, $2, $3, $4, $5, $6) returning document.id;",

    io:format("Q1 =~p~n", [Q1 ]),
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1, _, [{DocId}]} = pgsql:equery(Con, Q1, [Name, Content, Dir_id, Doc_type_id, UID, Published]),
            TempFilename = Pic_url,
            OriginalFilename = filename:basename(Pic_url),
            Destination = string:join(["static/data/docs", utils:to_list(DocId), OriginalFilename], "/"),
            filelib:ensure_dir(Destination), % TODO заменить file:rename на web : moveFile
            case file:rename(TempFilename, Destination) of
                ok ->
                    Q2 = "update document set  pic_url = $2  where id = $1;",
                    pgsql:equery(Con, Q2, [DocId, Destination]),
                    % % ---------------------------
                    % создаем тубнэил к картинке
                    img_converter:convert(Destination),
                    % % ---------------------------
                    file:del_dir(filename:dirname(TempFilename));
                {error, Error} ->
                    ?ERROR(?FMT("Couldn't open ~p for writing, error: ~p~n", [Destination, Error]))
            end,
            % log new document. не важно какая скорость, так что честно берем объект из базы.
            {ok, [Doc]} = dao:processPGRet2(pgsql:equery(Con, "select * from document where id=$1;", [DocId])),
            dao_bLog:logDocumentCreate(Doc, UID, Con),
            {ok, DocId} 
        end),
    Result = dao:processPGRet2(Ret),
    {ok, Id} = Ret,
    pdf_converter:convert(Id),
    Result;

updateDoc({{Id, Name, Content, Published}, {Pic_url},  {Dir_id, Doc_type_id}, UID}) ->
    Q1 = "update document set "
                "name = $2, content = $3, "
                "dir_id = $4, doc_type_id = $5, "
                "updater = $6 , published = $7, "
                "pic_url = $8 "
            "where id = $1;",
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, [OldDoc]} = dao:processPGRet2(pgsql:equery(Con, "select * from document where id=$1;", [Id])),
            % % ---------------------------
            % создаем тубнэил к картинке
             ImgRet = img_converter:convert(Pic_url),
            % % ---------------------------
            {ok, 1} = pgsql:equery(Con, Q1, [Id, Name, Content, Dir_id, Doc_type_id, UID, Published, Pic_url]),

            {ok, [NewDoc]} = dao:processPGRet2(pgsql:equery(Con, "select * from document where id=$1;", [Id])),
            dao_bLog:logDocumentUpdate(OldDoc, NewDoc, UID, Con),
            ok
        end),
    Result = dao:processPGRet2(Ret),
    pdf_converter:convert(Id),
    Result.

updateTestAnswer({{null, Name, Content, Published, Correct_flag}, {Pic_url}, {Dir_id, Doc_type_id}, UID}) ->
    
    Q1 =    "insert into document "
                "(name, content, dir_id, doc_type_id, updater, published) "
            "values ($1, $2, $3, $4, $5, $6) returning document.id;",
    Q2 = "update document set  pic_url = $2  where id = $1;",
    Q3 = "insert into quest_answer (answer_id , correct_flag) values ($1, $2)",

    io:format("Q1 =~p~n", [Q1 ]),
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1, _, [{DocId}]} = pgsql:equery(Con, Q1, [Name, Content, Dir_id, Doc_type_id, UID, Published]),
            {ok, 1} = pgsql:equery(Con, Q3, [DocId, Correct_flag]),
            io:format("DocId =~p~n", [DocId]),        
            TempFilename = Pic_url,
            OriginalFilename = filename:basename(Pic_url),
            Destination = string:join(["static/data/docs", utils:to_list(DocId), OriginalFilename], "/"),
            filelib:ensure_dir(Destination),
            io:format("1~n"),
            case file:rename(TempFilename, Destination) of
                ok ->
                    pgsql:equery(Con, Q2, [DocId, Destination]),
                    % % ---------------------------
                    % создаем тубнэил к картинке
                    img_converter:convert(Destination),
                    % % ---------------------------
                    file:del_dir(filename:dirname(TempFilename));
                {error, Error} ->
                    ?ERROR(?FMT("Couldn't open ~p for writing, error: ~p~n", [Destination, Error]))
            end
        end),
    Result = dao:processPGRet2(Ret),
    io:format("Ret=~p~n", [Ret]),
    Result;

updateTestAnswer({{Id, Name, Content, Published, Correct_flag}, {Pic_url},  {Dir_id, Doc_type_id}, UID}) ->
    Q1 = "update document set "
                "name = $2, content = $3, "
                "dir_id = $4, doc_type_id = $5, "
                "updater = $6 , published = $7, "
                "pic_url = $8 "
            "where id = $1;",
    Q2 = "delete from quest_answer where answer_id  = $1; ",
    Q3 = "insert into quest_answer (answer_id , correct_flag) values ($1, $2);",
    Ret = dao:withTransactionMchs(fun(Con) ->
            % % ---------------------------
            % создаем тубнэил к картинке
            img_converter:convert(Pic_url),
            % % ---------------------------
            {ok, 1} = pgsql:equery(Con, Q1, [Id, Name, Content, Dir_id, Doc_type_id, UID, Published, Pic_url]),
            {ok, _ } = pgsql:equery(Con, Q2, [Id]),
            {ok, 1} = pgsql:equery(Con, Q3, [Id, Correct_flag])            
    end),
    dao:processPGRet2(Ret).


deleteDocs({Id, UID}) ->
    Q = "update document set published = false, updater=$2 where id = $1;",
    Ret = dao:withTransactionMchs(fun(Con) ->
        R = pgsql:equery(Con, Q, [Id, UID]),
        {ok, [Doc]} = dao:processPGRet2(pgsql:equery(Con, "select * from document where id=$1;", [Id])),
        dao_bLog:logDocumentDelete(Doc, UID, Con),
        R
    end),
    dao:processPGRet2(Ret).
%    dao:simple(Q, [Id, UID]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Аттачи
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!!!!!
updateAttach({null, Name, Alt, Doc_id,  Attach_type_id, Url, UID}) ->
    Q1 =    "insert into  attach2doc"
                "(name, alt, doc_id, attach_type_id, url, updater) "
            "values ($1, $2, $3, $4, $5, $6) returning attach2doc.id; ",
    
    Ret = dao:withTransactionMchs(fun(Con) ->
            {ok, 1, _, [{Id}]} = pgsql:equery(Con, Q1, [Name, Alt,
                utils:to_integer(Doc_id),
                utils:to_integer(Attach_type_id),
                Url, utils:to_integer(UID)]),
            put("id", Id), % TODO убрать это. обеспечить возврат значения через {return, Value}

    %       io:format("Id = ~p~n", [Id])
            {ok, [Att]} = dao:processPGRet2(pgsql:equery(Con, "select * from attach2doc where id=$1;", [Id])),
            dao_bLog:logAttachCreate(Att, UID, Con),
            ok 
        end),
    io:format("Ret = ~p~n", [Ret]),
    {dao:processPGRet2(Ret), get("id")}.

deleteAttach_returningUrl({Id, UID}) ->
    Q1 =    "delete from attach2doc where id = $1 returning url;",

    Ret = dao:withTransactionMchs(fun(Con) ->
            case Id of
                undefined ->
                    io:format("Id  = ~p~n", [Id]);
                _ ->
                    {ok, [Att]} = dao:processPGRet2(pgsql:equery(Con, "select * from attach2doc where id=$1;", [Id])),
                    dao_bLog:logAttachDelete(Att, UID, Con),

                    {ok, 1, _, [{Url}]} = pgsql:equery(Con, Q1, [utils:to_integer(Id)]),
                    file:delete(Url),
                    io:format("Url = ~p~n", [Url])
            end
    end),
    dao:processPGRet2(Ret).
