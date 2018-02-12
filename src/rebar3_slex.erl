-module(rebar3_slex).

-export([init/1]).

-define(PROVIDER, rebar3_slex).
-define(DEPS, [app_discovery]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State2} = rebar3_slex_prv:init(State),
    {ok, State2}.
