-module(aws_signature).

-export([v4/9]).

v4(AccessKeyId, SecretAccessKey, Scope, Region,
   Method, Uri, QueryString, Headers, Body) ->
    HashFunction = "SHA256",
    Now = calendar:universal_time(),
    LongDate = list_to_binary(ec_date:format("YmdTHisZ", Now)),
    ShortDate = list_to_binary(ec_date:format("Ymd", Now)),
    StampedHeaders = stamp_headers(LongDate, Headers),
    {CanonicalHeaders, SignedHeaders} = canonical_headers(StampedHeaders),
    Request = canonical_request(Method, Uri, QueryString,
                                CanonicalHeaders, SignedHeaders,
                                Body, HashFunction),
    RequestDigest = digest(HashFunction, Request),
    RequestType = <<"aws4_request">>,
    CredScope = credential_scope(ShortDate, Region, Scope, RequestType),
    StringToSign = string_to_sign(HashFunction, LongDate, CredScope,
                                  RequestDigest),
    SigningKey = signing_key(HashFunction, SecretAccessKey, ShortDate,
                             Region, Scope, RequestType),
    Signature = sign(HashFunction, SigningKey, StringToSign),
    Authorization = authorization(HashFunction, AccessKeyId, CredScope,
                                  SignedHeaders, Signature),
    add_authorization_header(Authorization, StampedHeaders).

stamp_headers(Date, Headers) ->
    [{<<"x-amz-date">>, Date} | Headers].

add_authorization_header(Authorization, Headers) ->
    [{<<"authorization">>, Authorization} | Headers].

canonical_request(Method, Uri, QueryString, CanonicalHeaders,
                  SignedHeaders, Body, HashFunction) ->
    CanonicalURI = Uri,
    CanonicalQueryString = canonical_query_string(QueryString),
    PayloadDigest = digest(HashFunction, Body),
    Parts = [Method, CanonicalURI, CanonicalQueryString,
             CanonicalHeaders, SignedHeaders, PayloadDigest],
    iolist_to_binary(join(Parts, <<"\n">>)).

canonical_query_string(QueryString) ->
    Parsed = hackney_url:parse_qs(QueryString),
    hackney_url:qs(lists:sort(Parsed)).

canonical_headers(Headers) ->
    HeadersMap = lists:foldl(fun add_header/2, #{}, Headers),
    SortedHeaders = lists:sort(maps:to_list(HeadersMap)),
    Canonical = lists:map(fun header_to_iolist/1, SortedHeaders),
    {Names, _Values} = lists:unzip(SortedHeaders),
    {Canonical, signed_headers(Names)}.

add_header({Name, Value}, Headers) ->
    CanonicalName = canonical_header_name(Name),
    PreviousValues = maps:get(CanonicalName, Headers, []),
    CanonicalValue = canonical_header_value(Value),
    Headers#{CanonicalName => [CanonicalValue | PreviousValues]}.

canonical_header_name(Name) ->
    list_to_binary(string:strip(string:to_lower(binary_to_list(Name)))).

canonical_header_value(Value) ->
    list_to_binary(string:strip(internal_trim(binary_to_list(Value)))).

internal_trim(String) ->
    lists:reverse(internal_trim(String, "")).

internal_trim("", Acc) ->
    Acc;
internal_trim([$\ | [ $\ | _ ] = Rest], Acc) ->
    internal_trim(Rest, Acc);
internal_trim([C | Rest], Acc) ->
    internal_trim(Rest, [C | Acc]).

header_to_iolist({Name, Values}) ->
    [Name, <<":">>, join(Values, <<",">>), <<"\n">>].

signed_headers(Names) ->
    iolist_to_binary(join(Names, <<";">>)).

join([] = Empty, _Separator) ->
    Empty;
join(Elements, Separator) ->
    tl([Joined || Element <- Elements, Joined <- [Separator, Element]]).

digest("SHA256", Data) ->
    base16:encode(crypto:hash(sha256, Data)).

string_to_sign(HashFunction, LongDate, CredScope, Request) ->
    Parts = [HashFunction, LongDate, CredScope, Request],
    iolist_to_binary(join(Parts, <<"\n">>)).

credential_scope(ShortDate, Region, Scope, RequestType) ->
    Parts = [ShortDate, Region, Scope, RequestType],
    iolist_to_binary(join(Parts, <<"/">>)).

signing_key(HashFunction, SecretAccessKey, ShortDate,
            Region, Scope, RequestType) ->
    Hash = hmac_fun(HashFunction),
    Key = <<"AWS4", SecretAccessKey/binary>>,
    lists:foldl(Hash, Key, [ShortDate, Region, Scope, RequestType]).

hash_function("SHA256") ->
    sha256.

hmac_fun(HashFunction) ->
    HashName = hash_function(HashFunction),
    fun(Data, Key) ->
            crypto:hmac(HashName, Key, Data)
    end.

sign(HashFunction, SigningKey, StringToSign) ->
    HMAC = hmac_fun(HashFunction),
    base16:encode(HMAC(StringToSign, SigningKey)).

authorization(HashFunction, AccessKeyId, CredScope,
              SignedHeaders, Signature) ->
    <<"AWS4-HMAC-", (list_to_binary(HashFunction))/binary,
      "Credential=", AccessKeyId/binary, "/", CredScope/binary, ", ",
      "SignedHeaders=", SignedHeaders/binary, ", ",
      "Signature=", Signature/binary>>.
