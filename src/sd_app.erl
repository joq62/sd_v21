%%%-------------------------------------------------------------------
%% @doc sd public API
%% @end
%%%-------------------------------------------------------------------

-module(sd_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sd_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
