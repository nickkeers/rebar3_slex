-module(rebar3_slex_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, rebar3_slex).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                 {name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 rebar3_slex compile"},
                                 {opts, [
                                    {srcdir, $s, "srcdir", "src", "the source directory to copy .dtl.erl files to"}
                                 ]},
                                 {short_desc, "rebar3 plugin to compile slex files"},
                                 {desc, ""} 
    ]),
    State2 = rebar_api:add_deps_to_path(State),
    {ok, rebar_state:add_provider(State2, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    SrcOut = proplists:get_value(srcdir, Args),
    State2 = do_slex_compile(SrcOut),
    case State2 of 
        {error, _} ->
            State2;
        _ ->
            % Restore our code path if we didn't get an error
            _ = rebar_api:restore_code_path(State2),
            {ok, State2}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_slex_compile(SrcOut) ->
    rebar_base_compiler:run([], [], "src", ".slex", SrcOut, ".erl", fun compile_slex/3).

compile_slex(Source, Target, _Config) ->
    try slex_compiler:compile(Source, [{target, erl}, {out_dir, src}]) of
        {ok, Target} -> ok;
        {error, Error} ->
            rebar_api:debug("compile ~p -> ~p ~n  fail: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Source, [{Source, [Error]}], [], [])
    catch
        throw:Error ->
            rebar_api:debug("compile ~p -> ~p ~n  throw: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Source, [{Source, [Error]}], [], [])
    end.
