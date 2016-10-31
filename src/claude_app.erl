%%%-------------------------------------------------------------------
%% @doc claude public API
%% @end
%%%-------------------------------------------------------------------

-module(claude_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Definitions = filename:join([code:priv_dir(claude), "aws_api"]),
    ok = claude_generator:load_from_files(Definitions),
    claude_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
