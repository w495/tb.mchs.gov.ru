%%% @file dao_bLog.erl
%%%
%%%     API логирования действий над объектами системы.
%%%

-module(dao_bLog).
-compile(export_all).

getObjVal(null, _FieldName) ->
    null;
getObjVal(Obj, FieldName) ->
    proplists:get_value(FieldName, Obj, null).

logActionInner([{ActionType, FieldName, OldObj, NewObj}|T], Con, LogId) ->
    OldVal = getObjVal(OldObj, FieldName),
    NewVal = getObjVal(NewObj, FieldName),
    case OldVal =:= NewVal of
        true -> ok;
        false ->
            io:format("log_action ~p~n", [[LogId, ActionType, FieldName, OldVal, NewVal]]),
            Q = "insert into log_action (log_id, log_action_type_id, field_name, old_value, new_value) values "
                "($1, (select id from log_action_type where name=$2), $3, $4, $5);",
            pgsql:equery(Con, Q, [LogId, ActionType, FieldName, OldVal, NewVal])
    end,
    logActionInner(T, Con, LogId);
logActionInner([], _, _)->
    done.

logInner(ObjectType, OID, UID, Actions, Con) ->
    Q = "insert into log (log_object_id, log_object_type_id, customer_id) values "
        "($1, (select id from log_object_type where name=$2), $3) returning log.id;",
%    Ret = dao:withTransactionMchs(fun(Con) ->
    io:format("log ~p~n", [[OID, ObjectType, UID]]),
    {ok, 1, _, [{LogId}]} = pgsql:equery(Con, Q, [OID, ObjectType, UID]),
    logActionInner(Actions, Con, LogId).
%    end),
%    dao:processPGRet2(Ret).

logDocumentCreate(Doc, UID, Con) ->
    logInner("document", proplists:get_value("id", Doc), UID, [{"create", "name", null, Doc}, {"create", "content", null, Doc}], Con).

logDocumentDelete(Doc, UID, Con) ->
    logInner("document", proplists:get_value("id", Doc), UID, [{"delete", "name", Doc, null}, {"delete", "content", Doc, null}], Con).

logDocumentUpdate(Old, New, UID, Con) ->
    logInner("document", proplists:get_value("id", Old), UID, [{"update", "name", Old, New}, {"update", "content", Old, New}], Con).

logDirCreate(Dir, UID, Con) ->
    logInner("directory", proplists:get_value("id", Dir), UID, [{"create", "name", null, Dir}], Con).
logDirDelete(Dir, UID, Con) ->
    logInner("directory", proplists:get_value("id", Dir), UID, [{"delete", "name", Dir, null}], Con).
logDirUpdate(Old, New, UID, Con) ->
    logInner("directory", proplists:get_value("id", Old), UID, [{"update", "name", Old, New}], Con).

logAttachCreate(Att, UID, Con) ->
    logInner("attach", proplists:get_value("id", Att), UID, [{"create", "name", null, Att}], Con).
logAttachDelete(Att, UID, Con) ->
    logInner("attach", proplists:get_value("id", Att), UID, [{"delete", "name", Att, null}], Con).

getLogs(_) ->
    Q = "select l.id, c.login, l.datetime, lot.alias as log_object_type_alias, l.log_object_id "
        "from log l join customer c on l.customer_id = c.id "
        "join log_object_type lot on lot.id = l.log_object_type_id;",
    dao:simple(Q).

getLog1(Id) ->
    Q = "select l.id, c.login, l.datetime, lot.alias as log_object_type_alias, l.log_object_id "
        " from  log l "
            " join customer c "
                " on l.id = $1 and l.customer_id = c.id "
            " join log_object_type lot "
                " on lot.id = l.log_object_type_id;",
    dao:simple(Q, [Id]).


getLog(Id) ->
    case getLog1(Id) of
        {ok, R1Val} ->
            Q2 = "select field_name, old_value, new_value from log_action la where la.log_id  = $1",
            case dao:simple(Q2, [utils:to_integer(Id)]) of
                {ok, R2Val} ->  {ok, R1Val, R2Val};
                E2 -> E2
            end;
        {ok, []} -> [];
        E1 -> E1
    end.
