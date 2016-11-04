-module(claude_client).

-export([request/6, get_url/2]).

get_url(_ApiMetadata, #{endpoint_url := Url} = _Client) ->
    Url;
get_url(#{<<"globalEndpoint">> := Url}, _Client) ->
    https_url(Url);
get_url(#{<<"endpointPrefix">> := Prefix}, #{region := Region} = _Client) ->
    https_url(<<Prefix/binary, ".", Region/binary, ".amazonaws.com">>);
get_url(#{<<"endpointPrefix">> := _Prefix}, _Client) ->
    throw({missed_arg, region}).

https_url(Host) ->
    <<"https://", Host/binary>>.

request(_Protocol, Method, Url, Uri, Client, Parameters) ->
    QP = parameters_to_query_params(Parameters),
    hackney_url:make_url(Url, Uri, QP).

parameters_to_query_params(Parameters) when is_list(Parameters);
					    is_map(Parameters) ->
    lists:flatmap(fun parameters_join/1, parameters_to_query_params_r(Parameters)).

parameters_to_query_params_r(#{} = Parameters) ->
    [{Key, parameters_to_query_params_r(Value)} || 
	{Key, Value} <- maps:to_list(Parameters)];
parameters_to_query_params_r(ParameterList) when is_list(ParameterList) ->
    Enum = lists:zip(lists:seq(1, length(ParameterList)), ParameterList),
    [{integer_to_list(Pos), parameters_to_query_params_r(Value)} ||
	{Pos, Value} <- Enum];
parameters_to_query_params_r({value, String} = Value) ->
    Value.

parameters_join({Key, {value, Value}}) ->
    [{Key, Value}];
parameters_join({Key, KVs}) when is_list(KVs) ->
    [{string:join([Key, SubKey], "."), Value} ||
	KV <- KVs, {SubKey, Value} <- parameters_join(KV)].
