-module(auth_biz).
-compile(export_all).
-export([login/2, get_session/1, logout/1]).

-include("../../common/include/customer.hrl").
-include("../../common/include/permission.hrl").

get_session(UID) ->
    web_session_DAO:get(UID).

gen_UID(Login) ->
    UID = uuid:to_string(uuid:v4()) ++ Login,
    case web_session_DAO:get(UID) of
        [] -> UID;
        _ -> gen_UID(Login)
    end.

create_session(Id, Login, Permissions, PasswordHash) ->
    UID = gen_UID(utils:to_list(Login)),
    web_session_DAO:create(UID, Id, Login, Permissions, PasswordHash),
    io:format("~nUID = [~p]~n", [UID] ),
    UID.

get_customer(Login) ->
    case dao_bCustomer:getCustomerByLogin(Login) of
        {ok, [Customer], Permissions} ->
            io:format("Customer, Permissions"),
            {Customer, Permissions}; %[binary_to_list(X) || X <-Permissions]};
        {ok, _, _} ->
            null; % no customer
        {error, E} ->
            io:format("get_customer error: ~p ~n", [E]),
            null
    end.

login(Login, Password) ->
    PasswordHash = list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(erlang:md5(Password))])),
    MaxAuthError = config:get(max_auth_error, 10),
    EC = 0,
    case get_customer(Login) of
        null -> throw("bad_customer");
        {Customer, Permissions} -> %, auth_error_counter = EC} ->
            P = proplists:get_value("password_hash", Customer),
            io:format("~nP  = [~p]~n", [P] ),
            io:format("~nPasswordHash  = [~p]~n", [PasswordHash] ),
            if
                EC >= MaxAuthError ->
                    throw({"auth_count_overflow", MaxAuthError});
                P =/= PasswordHash ->
                    % ???? csrv:set_auth_count(Login, EC + 1),
                    if 
                        MaxAuthError - (EC + 1) > 0 ->
                            throw({"bad_password", integer_to_list(MaxAuthError - (EC + 1))});
                        true ->
                            throw({"auth_count_overflow", MaxAuthError})
                    end;
                true  -> 
                    if
                        %??? EC > 0 -> csrv:set_auth_count(Login, 0);
                        true -> ok
                    end,
                    Id = proplists:get_value("id", Customer),
                    io:format("~nId  = [~p]~n", [Id] ),
                    create_session(Id, Login, Permissions, PasswordHash)
            end
    end.

logout(UID) ->
    web_session_DAO:remove(UID).

%change_pass(Login, Password) ->
%    csrv:set_password(Login, Password).

