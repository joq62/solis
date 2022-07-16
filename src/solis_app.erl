%%%-------------------------------------------------------------------
%% @doc solis public API
%% @end
%%%-------------------------------------------------------------------

-module(solis_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    solis_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
