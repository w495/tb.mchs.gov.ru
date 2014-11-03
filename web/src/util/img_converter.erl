-module(img_converter).
-compile(export_all).

convert(Img_url) ->
    spawn(fun() ->
        case Img_url of
            null -> ok;
            _ ->
                Exec = filename:absname("./deps/imgconverter/nconvert-x86_64"),
                Bash = filename:absname("./deps/imgconverter/imgconverter.bash"),
                String = utils:sformat("~s ~s \"~s\"", [Bash, Exec, filename:absname(Img_url)]),
                Result = os:cmd(String),
                case  Result of
                    "0" -> ok;
                    _ -> error
                end
        end
    end).