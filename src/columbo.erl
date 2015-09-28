-module(columbo).


-export([main/1]).

columbo_dir() ->
    ".columbo".

main(_Args) ->
    ok = ensure_columbo_dir(),
    DepsSpec1 = read_rebar_deps("rebar.config"),
    DepsSpec2 = [ {Dep, Url, author_from_url(Url), Treeish}
                || {Dep, {Url, Treeish}} <- DepsSpec1],
    Authors = [ Author || {_,_,Author,_} <- DepsSpec2],
    ensure_author_dirs(Authors),
    lists:foreach( fun clone_dep/1, DepsSpec2),
    io:format("~p~n", [Authors]).
    %% {ok, Dirs} = file:list_dir("deps"),
    %% Deps = [ erlang:list_to_atom(Dir) || Dir <- Dirs],
    %% DepVersions = [ {Dep, current_version(Dep)}
    %%                 || Dep <- Deps ],
    %% io:format("~p~n", [DepVersions]),
    %% DepDeps = [{Dep, dep_deps(Dep)} || Dep <- Deps],
    %% io:format("~p~n", [DepDeps]).

    %% [tail_tags(Dep) || Dep <- Dirs].


clone_dep({Dep, Url, Author, _Treeish}) ->
    Cmd = io_lib:format("git clone ~p ~p/~p/~p",
                        [Url, columbo_dir(), Author, Dep]),
    execute_cmd(Cmd).

ensure_columbo_dir() ->
    ensure_dir(columbo_dir()).

ensure_author_dirs(Authors) ->
    lists:foreach(fun(Author) -> io_lib:format("~s/~p", [columbo_dir(), Author]) end,
                  Authors).

ensure_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            Error
    end.

current_version(Dep) ->
%%    Cmd = "git describe --always --tags",
    CurrentDir = current_dir(),
    cd_deps_dir(CurrentDir, Dep),
    {Tag,_Description} = current_tag_and_branch(),
%%    Res = trim_version_string(execute_cmd(Cmd)),
    cd_dir(CurrentDir),
    Tag.

current_tag_and_branch() ->
    Cmd1 = "git describe --always --tags",
    Tag  = trim_version_string(execute_cmd(Cmd1)),
    Cmd2 = "git branch",
    Branch = trim_version_string(execute_cmd(Cmd2)),
    {Tag, Branch}.


tail_tags(Dep) ->
    Cmd = "git tag -l \"[0-9]*\" -l \"v[0-9]*\"",
    CurrentDir = current_dir(),
    cd_deps_dir(CurrentDir, Dep),
    Res = string:tokens(execute_cmd(Cmd), "\n"),
    Relevant = tail(lists:sort([strip_v(R) || R <- Res]), 5),
    Cmd2 = "git tag -l \"[0-9]*.*p[0-9]*\" -l \"v[0-9]*.*p[0-9]*\" \"[0-9]*.*basho[0-9]*\" -l \"v[0-9]*.*basho[0-9]*\"",
    Res2 = string:tokens(execute_cmd(Cmd2), "\n"),
    Relevant2 = tail(lists:sort([strip_v(R) || R <- Res2]), 5),
    cd_dir(CurrentDir),
    io:format("~p tags: ~p~n", [Dep, Relevant]),
    io:format("~p Basho tags: ~p~n", [Dep, Relevant2]).

tail(Ls, N) when length(Ls) < N ->
    Ls;
tail(Ls, N) ->
    lists:nthtail(length(Ls) - N, Ls).

strip_v([$v|Vsn]) -> Vsn;
strip_v(Vsn) -> Vsn.


print_deps(Dep, Deps) ->
    io:format("~p deps:~n", [Dep]),
    [ io:format("    ~p~n", [D]) || D <-Deps].

cd_dir(Dir) ->
    ok = file:set_cwd(Dir).

cd_deps_dir(CurrentDir, Dep) ->
    Dir = lists:flatten(io_lib:format("~s/deps/~p", [CurrentDir, Dep])),
    cd_dir(Dir).

execute_cmd(Cmd) ->
    os:cmd(Cmd).

current_dir() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

trim_version_string(S) ->
    string:strip(S, both, hd("\n")).

dep_deps(Dep) ->
    FileName = lists:flatten(io_lib:format("./deps/~p/rebar.config", [Dep])),
    case file:consult(FileName) of
         {ok, Terms} ->
            RawDeps = proplists:get_value(deps, Terms, []),
            pretty_deps(RawDeps);
        {error, enoent} ->
            []
    end.

read_rebar_deps(Filename) ->
    case file:consult(Filename) of
         {ok, Terms} ->
            RawDeps = proplists:get_value(deps, Terms, []),
            pretty_deps(RawDeps);
        {error, enoent} ->
            []
    end.

pretty_deps(Deps) ->
    [ pretty_dep(Dep)
      || Dep <- Deps ].

pretty_dep({Dep, _Req, Git}) ->
    {Dep, extract_git_info(Git)}.

extract_git_info({git, Url, Info}) ->
    {Url, Info}.

author_from_url(Url) ->
    {match, [Tmp]} =  re:run(Url, "github.\com.([^/]*)", [{capture, first, list}]),
    [_, Author] = string:tokens(Tmp, "/"),
    Author.
