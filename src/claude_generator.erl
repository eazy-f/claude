-module(claude_generator).

-export([load_from_files/1, load_ec2/1]).

load_from_files(DefinitionsDirpath) ->
    load_ec2(DefinitionsDirpath),
    ok.

load_ec2(DefinitionsDirpath) ->
    Filename = filename:join([DefinitionsDirpath, "ec2", "2016-09-15", "service-2.json"]),
    {ok, Content} = file:read_file(Filename),
    Ec2Definition = jiffy:decode(Content, [return_maps]),
    {Module, Code} = generate_code(Ec2Definition),
    code:load_binary(Module, Filename, Code).

generate_code(ApiDefinitions) ->
    AST = generate_module(ApiDefinitions),
    {ok, Module, Binary} = compile:forms(erl_syntax:revert_forms(AST)),
    {Module, Binary}.

generate_module(ApiDefinitions) ->
    ApiOperations = operation_functions(ApiDefinitions),
    Exports = exports(ApiOperations),
    Functions = ApiOperations,
    erl_syntax:form_list([module_name(ApiDefinitions), Exports | Functions]).

module_name(ApiDefinitions) ->
    #{
       <<"metadata">> := #{
         <<"endpointPrefix">> := EndpointName
        }
     } = ApiDefinitions,
    NamePrefix = "claude",
    NameSuffix = "client_generated",
    ModuleName = list_to_atom(string:join([NamePrefix, binary_to_list(EndpointName), NameSuffix], "_")),
    erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModuleName)]).

operation_functions(#{<<"operations">> := Operations} = ApiDefinitions) ->
    OperationDefinitions = maps:values(Operations),
    #{<<"shapes">> := Shapes} = ApiDefinitions,
    [operation_function(Op, Shapes) || Op <- OperationDefinitions].

claude_name(CamelCaseBin) ->
    Words = camel_parts(binary_to_list(CamelCaseBin)),
    string:join(Words, "_").

camel_parts(CamelString) ->
    camel_parts(CamelString, []).

-define(is_capital(C), ((C) >= $A andalso (C) =< $Z)).

camel_parts([], Acc) ->
    lists:reverse(Acc);
camel_parts([Capital | Rest], Acc) when ?is_capital(Capital) ->
    {WordTail, Tail} = lists:splitwith(fun(C) -> not ?is_capital(C) end, Rest),
    camel_parts(Tail, [[string:to_lower(Capital) | WordTail] | Acc]).

operation_function(Operation, Shapes) ->
    #{<<"name">> := Name} = Operation,
    Output = maps:get(<<"output">>, Operation, undefined),
    FunctionBody = [erl_syntax:abstract(Output)],
    FunctionName = erl_syntax:atom(list_to_atom(claude_name(Name))),
    Args = [erl_syntax:variable('Client'), erl_syntax:variable('Parameters')],
    erl_syntax:function(FunctionName, [erl_syntax:clause(Args, none, FunctionBody)]).
    
exports(ExportedFunctions) ->
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:function_name(Fun),
                                             erl_syntax:integer(erl_syntax:function_arity(Fun)))
                  || Fun <- ExportedFunctions],
    erl_syntax:attribute(erl_syntax:atom(export),
                         [erl_syntax:list(ExportList)]).
