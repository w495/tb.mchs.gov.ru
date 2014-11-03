-module(attachCtl).
-compile(export_all).
-include("../include/common.hrl").

% ..../dir/DirId/AttId/Filename

%delAttach(Filename, DirId, AttId) ->

mkAttach(Filename, DirId, AttId, Content) ->
    FilePath = string:join(["static/docs", utils:to_list(DirId), utils:to_list(AttId), Filename], "/"),
    filelib:ensure_dir(FilePath),
    case file:open(FilePath, [raw,write]) of
        {ok, File} ->
            file:write(File, Content),
            file:close(File);
        {error, Error} ->
            ?ERROR(?FMT("Couldn't open ~p for writing, error: ~p~n", [FilePath, Error]))
    end.
