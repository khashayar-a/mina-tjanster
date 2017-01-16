-module(bank_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    bank_sup:start_link().

stop(_State) ->
    ok.


%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).

simple_test() ->
    ok = application:start(bank),
    ?assertNot(undefined == whereis(bank_sup)).

-endif.
