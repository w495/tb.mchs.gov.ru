-module(pdf_converter).
-compile(export_all).

-include("../include/common.hrl").

convert(Id) ->    
    spawn(fun() ->
        case Id of
            null -> ok;
            _ ->
                Exec = filename:absname("/usr/bin/wkhtmltopdf"),
                Dir = utils:sformat("./static/data/docs/~p/gen/", [Id]),
                filelib:ensure_dir(Dir),
                String = utils:sformat("~s http://~s:~p/Doc/~p/?print=1 ~s/~p.pdf", [Exec, ?LSTHOST, ?LSTPORT, Id, Dir, Id]),
                io:format("String = ~s", [String]),
                Result = os:cmd(String),
                case  Result of
                    "0" -> ok;
                    _ -> error
                end
        end
    end).