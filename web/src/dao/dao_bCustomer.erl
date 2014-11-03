%%% @file dao_bCustomer.erl
%%%
%%%     Доступ к данным для сущности пользователя.
%%%     Описаны опирации с пользователями, группами и правами.
%%%     Версия для админки
%%%

-module(dao_bCustomer).
-compile(export_all).

-export([
    % PERMISSIONS
        getPermission/1,
    % CUSTOMER_GROUPS
        getCustomerGroups/1,
        getCustomerGroup/1,
        updateCustomerGroup/1,
        deleteCustomerGroup/1,
    % CUSTOMERS
        getCustomers/1,
        getCustomer/1,
        updateCustomer/1,
        deleteCustomer/1]).

getPermission(_) ->
    Q = "select p.id, p.name, p.description, p.type, p.perm_type_id, p.entity_id
         from permission as p;",
    dao:simple(Q).

% ============================================================================
% % CUSTOMER_GROUPS
% ============================================================================

getCustomerGroups(_) ->
    Q = "select id, name, description from customer_group WHERE customer_group.deleted = false;",
    dao:simple(Q).

getCustomerGroup(Id) ->
    Q1 = "select id, name, description from customer_group where customer_group.id = $1;",
    io:format("Q1 = ~p~n", [Q1]),
    case dao:simple(Q1, [utils:to_integer(Id)]) of
        {ok, R1Val} ->
            Q2 = "select perm_id from permission2group where group_id = $1;",
            io:format("Q2 = ~p~n", [Q2]),
            case dao:simple(Q2, [utils:to_integer(Id)]) of
                {ok, R2Val} -> {ok, R1Val, [X || [{"perm_id", X}] <- R2Val]};
                E2 -> E2
            end;
        E1 -> E1
    end.

deleteCustomerGroup({Id, UID}) ->
    Q = "update customer_group set deleted=true "
        "where "
            " issystem=false " % нельзя удалять системные группы.
            " and id = $1; ",
    dao:simple(Q, [Id]).

updateCustomerGroup({{null, Name, Descr}, PermissionList, UID}) ->
    Q1 = "insert into customer_group (name, description) values ($1, $2) returning customer_group.id;",
    Ret = dao:withTransactionMchs(fun(Con) ->
        {ok, 1, _, [{Id}]} = pgsql:equery(Con, Q1, [Name, Descr]) ,
        io:format("New CustomerGroupID: ~p~n", [Id]) ,
        case length(PermissionList) of
            0 -> ok;
            L ->
                Q2 = lists:append(["insert into permission2group (group_id, perm_id) values ",
                                    string:join([lists:flatten(io_lib:format("(~p, ~p)", [Id, X])) || X <- PermissionList], ", ")]),
                {ok, L} = pgsql:equery(Con, Q2, []),
                ok
        end
    end),
    dao:processPGRet2(Ret);

updateCustomerGroup({{Id, Name, Descr}, PermissionList, UID}) ->
    Q1 = "update customer_group set name = $1, description = $2 where id = $3;",
    Q2 = "delete from permission2group where group_id = $1;",
    Q3 = "insert into permission2group (group_id, perm_id) values " ++ 
            string:join([lists:flatten(io_lib:format("(~p, ~p)", [Id, X])) || X <- PermissionList], ", "),
    Ret = dao:withTransactionMchs(
        fun(Con) ->
             {ok, 1} = pgsql:equery(Con, Q1, [Name, Descr, Id]),
             {ok, _} = pgsql:equery(Con, Q2, [Id]),
             case length(PermissionList) of
                0 -> ok;
                L -> {ok, L} = pgsql:equery(Con, Q3, []), ok
            end
        end
    ),
    dao:processPGRet2(Ret).

% @depricated block
% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

% @depricated 
getPersonTypes(_) ->
    Q = "select id, name from person_type where name != 'Пользователь';",
    dao:simple(Q).

% @depricated 
getPersons(Params) ->
    Qs = "select person.id, person.firstname, person.lastname, person.patronimic, person.phone, person.type_id, person.updater, person.commentary, "
                 "person_type.name as type_name "
         "from person join person_type on person.type_id = person_type.id",
    TypeId = case proplists:get_value("type_id", Params, "null") of
        "null" -> [];
        V2 -> [{"person.type_id", utils:to_integer(V2)}]
    end,
    {Qw, P} = dao:collectWhereParams([{"person.deleted", false}|TypeId]),
    Q = lists:append([Qs, Qw, ";"]),
    dao:simple(Q, P).

% @depricated 
getPerson(Id) ->
    Q = "select person.id, person.firstname, person.lastname, person.patronimic, person.phone, person.type_id, person.updater, person.commentary, "
                "person_type.name as type_name, person.birthday "
         "from person join person_type on person.type_id = person_type.id where person.id=$1;",
    dao:simple(Q, [utils:to_integer(Id)]).

% @depricated 
updatePerson({{null, FN, LN, Pat, Phn, TId, Comm, Birthday}, UID}) ->
    Q = "insert into person (firstname, lastname, patronimic, phone, type_id, updater, commentary, birthday) values ($1, $2, $3, $4, $5, $6, $7, $8);",
    dao:simple(Q, [FN, LN, Pat, Phn, TId, UID, Comm, Birthday]);

% @depricated 
updatePerson({{Id, FN, LN, Pat, Phn, TId, Comm, Birthday}, UID}) ->
    Q = "update person set firstname = $1, lastname = $2, patronimic = $3, phone = $4, type_id=$5, updater=$6, commentary=$7, birthday=$8 where id = $9;",
    dao:simple(Q, [FN, LN, Pat, Phn, TId, UID, Comm, Birthday, Id]).

% @depricated
deletePerson({Id, UID}) ->
    Q = "update person set updater=$1, deleted = true where id = $2;",
    dao:simple(Q, [UID, utils:to_integer(Id)]).

% @depricated 
getFreePersons(_) ->
    Q = "select person.id, person.firstname, person.lastname, person.patronimic, person.phone, person.type_id, person.updater, person.commentary, person_type.name as type_name "
    "from person join person_type on person.type_id = person_type.id "
    "where not exists (select * from school_bus where school_bus.person_id = person.id ) and person.deleted = false;",
    dao:simple(Q).

% >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

% ============================================================================
% % CUSTOMERS
% ============================================================================

%+
getCustomers(_) ->
    Q = "select customer.id, customer.firstname, customer.lastname, customer.patronimic, customer.login, customer.pic_url, "
                "customer.password_hash "
         "from customer WHERE customer.deleted = false;",
    dao:simple(Q).

getExperts(_) ->
    Q = "select customer.id, customer.firstname, customer.lastname, customer.patronimic, customer.login, customer.pic_url, "
                "customer.password_hash "
         " from customer, customer2group, customer_group "
         " WHERE "
            " customer2group.customer_id = customer.id"  
            " and customer2group.group_id = customer_group.id "
            " and customer_group.name = 'expert' "
            " and customer.deleted = false; ",
    dao:simple(Q).

%-
getCustomer(Id) ->
    Q1 = "select customer.id, "
                "customer.firstname, customer.lastname, customer.patronimic, "
                "customer.city, customer.organization, customer.position, "
                "customer.email, customer.login, customer.pic_url, customer.password_hash "
            "from customer where customer.id=$1;",
    case dao:simple(Q1, [utils:to_integer(Id)]) of
        {ok, R1Val} ->
            Q2 = "select group_id from customer2group where customer_id = $1",
            case dao:simple(Q2, [utils:to_integer(Id)]) of
                {ok, R2Val} -> {ok, R1Val, [X || [{"group_id", X}] <- R2Val]};
                E2 -> E2
            end;
        E1 -> E1
    end.


%+
getCustomerByLogin(Login) ->
    Q1 = "select customer.id, customer.firstname, customer.lastname, customer.patronimic, "
            "customer.login, customer.password_hash "
         "from customer where customer.login=$1 and customer.deleted=false;",
    case dao:simple(Q1, [Login]) of
        {ok, R1Val} ->
            Q2 =    "select permission.name "
                    "from customer   join customer2group on customer.id=customer2group.customer_id "
                                    "join permission2group on permission2group.group_id=customer2group.group_id "
                                    "join permission on permission.id=permission2group.perm_id "
                    "where customer.login=$1;",
            case dao:simple(Q2, [Login]) of
                {ok, R2Val} -> {ok, R1Val, [X || [{"name", X}] <- R2Val]};
                E2 -> E2
            end;
        E1 -> E1
    end.

%%
%% Создает нового пользователя
%%
updateCustomer({{null, Firstname, Lastname, Patronimic, Login, Pic_url, Email, City,
                    Organization, Position}, Password_hash, GroupList, UID}) ->
    Q1 = "insert into customer (firstname, lastname, patronimic, "
            "login, pic_url, email, city, organization, position, password_hash) "
         "values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10) returning customer.id;",
         
    PGRet = dao:withTransactionMchs(
        fun(Con) ->
            {ok, 1, _, [{Id}]} = pgsql:equery(Con, Q1,
                [Firstname, Lastname, Patronimic, Login, Pic_url, Email,
                    City, Organization, Position, Password_hash]),
            case length(GroupList) of
                0 ->
                    ok;
                L ->
                    Q2 = "insert into customer2group (customer_id, group_id) values " ++
                        string:join([lists:flatten(io_lib:format("(~p, ~p)",
                            [Id, X])) || X <- GroupList], ", "),
                    {ok, L} = pgsql:equery(Con, Q2, [])
            end,
            ok
        end
    ),
    dao:processPGRet2(PGRet);

%%
%% Изменяет существующего пользователя
%%
updateCustomer({{Id, Firstname, Lastname, Patronimic, Login, Pic_url, Email, City,
                    Organization, Position}, Password_hash, GroupList, UID}) ->

    Q1 = "update customer set firstname = $1, lastname = $2, patronimic = $3, "
            "login = $4, pic_url = $5, email = $6,"
            "city = $7, organization = $8, position = $9 "
         "where id=$10;",

    Q2 = "delete from customer2group where customer_id = $1;",
    Q3 = "insert into customer2group (customer_id, group_id) values " ++ 
            string:join([lists:flatten(io_lib:format("(~p, ~p)",
                [Id, X])) || X <- GroupList], ", "),

    PGRet = dao:withTransactionMchs(
        fun(Con) ->
             {ok, 1} = pgsql:equery(Con, Q1,
                    [Firstname, Lastname, Patronimic, Login, Pic_url,
                        Email, City, Organization, Position, Id]),
             if Password_hash =/= null ->
                    {ok, 1} = pgsql:equery(Con, "update customer set password_hash=$1 "
                        "where id = $2;", [Password_hash, Id]);
                true ->
                    ok
             end,
             {ok, _} = pgsql:equery(Con, Q2, [Id]),
             case length(GroupList) of
                0 -> ok;
                L -> {ok, L} = pgsql:equery(Con, Q3, [])
            end,
            ok
        end
    ),
    dao:processPGRet2(PGRet).


deleteCustomer({Id, UID}) ->
    Q = "update customer set deleted=true where id = $1;",
    io:format("~n<(X_x)> Id = ~p;", [Id]),
    Id_int = utils:to_integer(Id),
    io:format("~n<(X_x)> Id_int = ~p;", [Id_int ]),
    dao:simple(Q, [Id_int]),
    io:format("~n<(X_x)> dao:simple;").


