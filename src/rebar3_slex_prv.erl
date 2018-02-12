-module(rebar3_slex_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, slex).
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
                                 {example, "rebar3 slex"},
                                 {opts, []},
                                 {short_desc, "rebar3 plugin to compile slex files"},
                                 {desc, ""} 
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    do_slex_compile().

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_slex_compile() ->
    rebar_base_compiler:run([], [], "src", ".slex", "src", ".erl", fun compile_slex/3).

compile_slex(Source, Target, _Config) ->
    try slex_compiler:compile(Source, [{target, erl}, {out_dir, src}]) of
        {ok, _Mod, _Out} ->
            ok;
        ok -> ok;
        {ok, Target} -> ok;
        {error, Error} ->
            rebar_api:debug("compile ~p -> ~p ~n  fail: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Source, [{Source, [Error]}], [], [])
    catch
        throw:Error ->
            rebar_api:debug("compile ~p -> ~p ~n  throw: ~P~n", [Source, Target, Error, 10]),
            rebar_base_compiler:error_tuple(Source, [{Source, [Error]}], [], [])
    end.
