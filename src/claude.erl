-module(claude).

-export([api/1, client/1, session/1, definitions_dirpath/0]).

session(#{} = Options) ->
    Options.

client(Session) ->
    client(Session, #{}).

client(Session, Options) ->
    #{session => Session}.

api(ServiceName) ->
    claude_generator:load_latest(atom_to_list(ServiceName),
                                 definitions_dirpath()).

api(ServiceName, Version) ->
    claude_generator:load_latest(atom_to_list(ServiceName),
                                 Version).

definitions_dirpath() ->
    filename:join([code:priv_dir(claude), "aws_api"]).
