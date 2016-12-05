-module(claude_client).

-export([request/6, get_url/2]).

-include_lib("hackney/include/hackney_lib.hrl").

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

request(_Protocol, Scope, Method, StaticUrl, Client, Parameters) ->
    ParsedStaticUrl = hackney_url:parse_url(StaticUrl),
    QP = parameters_to_query_params(Parameters),
    Headers = [{<<"Host">>,
                list_to_binary(ParsedStaticUrl#hackney_url.host)}],
    Url = ParsedStaticUrl#hackney_url{qs = hackney_url:qs(QP)},
    Body = <<"">>,
    SignedHeaders = sign_headers(Scope, Method, Url,
                                 Headers, Body, Client),
    hackney:request(Method, Url, SignedHeaders).

sign_headers(_Scope, _Method, _Url, Headers, _Body, #{no_sign := true}) ->
    Headers;
sign_headers(Scope, MethodAtom, Url, Headers, Body, Client) ->
    #{access_key_id := AccessKeyId,
      secret_access_key := SecretAccessKey,
      region := Region} = Client,
    #hackney_url{path = Uri, qs = QueryString} = Url,
    Method = list_to_binary(string:to_upper(atom_to_list(MethodAtom))),
    aws_signature:v4(AccessKeyId, SecretAccessKey, Scope, Region,
                     Method, Uri, QueryString, Headers, Body).

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
parameters_to_query_params_r({value, _String} = Value) ->
    Value.

parameters_join({Key, {value, Value}}) ->
    [{Key, Value}];
parameters_join({Key, KVs}) when is_list(KVs) ->
    [{string:join([Key, SubKey], "."), Value} ||
        KV <- KVs, {SubKey, Value} <- parameters_join(KV)].
