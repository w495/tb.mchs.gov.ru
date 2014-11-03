-module(index).
%-export([call_before/1, index/1]).
-include_lib("../include/common.hrl").

-compile(export_all).

call_before(Req) ->
    try authorization:auth_required(Req)
        catch throw:auth_required -> throw({redirect, "/login", []})
    end.

index(_Req) ->
    throw({serve_static, "index.html", ?JSHOME, [{"Cache-Control", "no-cache "}]}).

