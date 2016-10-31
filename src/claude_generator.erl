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

http_method(Method) when Method == <<"POST">>;
                         Method == <<"GET">>;
                         Method == <<"DELETE">>;
                         Method == <<"PUT">> ->
    binary_to_atom(Method, latin1).

operation_function(Operation, Shapes) ->
    Output = maps:get(<<"output">>, Operation, undefined),
    #{<<"name">> := Name,
      <<"input">> := InputShape,
      <<"http">> := #{
        <<"method">> := Method,
        <<"requestUri">> := Uri
    }} = Operation,
    FunctionName = erl_syntax:atom(list_to_atom(claude_name(Name))),
    Args = [erl_syntax:variable('Client'), erl_syntax:variable('Parameters')],
    CallClient = erl_syntax:application(erl_syntax:atom(claude_client),
                                        erl_syntax:atom(request),
                                        [erl_syntax:abstract(http_method(Method)),
                                         erl_syntax:abstract(Uri),
                                         erl_syntax:variable('Client'),
                                         erl_syntax:variable('ParsedParameters')]),
    ParseParameters = parameters_parser(InputShape, Shapes),
    FunctionBody = [erl_syntax:match_expr(
                      erl_syntax:variable('Parser'),
                      ParseParameters),
                    erl_syntax:match_expr(
                      erl_syntax:variable('ParsedParameters'),
                      erl_syntax:application(
                       erl_syntax:variable('Parser'),
                       [erl_syntax:variable('Parameters')])),
                    CallClient],
    erl_syntax:function(FunctionName, [erl_syntax:clause(Args, none, FunctionBody)]).
    
exports(ExportedFunctions) ->
    ExportList = [erl_syntax:arity_qualifier(erl_syntax:function_name(Fun),
                                             erl_syntax:integer(erl_syntax:function_arity(Fun)))
                  || Fun <- ExportedFunctions],
    erl_syntax:attribute(erl_syntax:atom(export),
                         [erl_syntax:list(ExportList)]).

parameters_parser(Definition, Shapes) ->
    #{<<"shape">> := ShapeName} = Definition,
    {ok, Shape} = maps:find(ShapeName, Shapes),
    #{<<"type">> := ShapeType} = Shape,
    EntryFail = fail_clause({unexpected_type, ShapeType}),
    case ShapeType of
        <<"structure">> ->
            #{<<"members">> := Members} = Shape,
            Clauses = [parameter_parser_clause(Name, MemberDefinition, Shapes)
                        || {Name, MemberDefinition} <- maps:to_list(Members)],
            VarStruct = prefix_var("Struct", binary_to_list(ShapeName)),
            EntryClause = erl_syntax:clause(
                            [erl_syntax:variable(VarStruct)],
                            erl_syntax:application(
                             erl_syntax:atom(is_map),
                             [erl_syntax:variable(VarStruct)]),
                            [erl_syntax:match_expr(
                               erl_syntax:variable('Parser'),
                               erl_syntax:fun_expr(Clauses)),
                             erl_syntax:application(
                              erl_syntax:atom(maps),
                              erl_syntax:atom(fold),
                              [erl_syntax:variable('Parser'),
                               erl_syntax:abstract(#{}),
                               erl_syntax:variable(VarStruct)]
                              )]),
            Entry = erl_syntax:fun_expr([EntryClause, EntryFail]),
            Entry;
        <<"list">> ->
            #{<<"member">> := MemberDefinition} = Shape,
            VarList = prefix_var("List", binary_to_list(ShapeName)),
            EntryClause = erl_syntax:clause(
                            [erl_syntax:variable(VarList)],
                            erl_syntax:application(
                             erl_syntax:atom(is_list),
                             [erl_syntax:variable(VarList)]),
                            [erl_syntax:application(
                              erl_syntax:atom(lists),
                              erl_syntax:atom(map),
                              [parameters_parser(MemberDefinition, Shapes),
                               erl_syntax:variable(VarList)])]),
            erl_syntax:fun_expr([EntryClause, EntryFail]);
        <<"string">> ->
            VarString = prefix_var("String", binary_to_list(ShapeName)),
            EntryClause = erl_syntax:clause(
                            [erl_syntax:variable(VarString)],
                            erl_syntax:application(
                             erl_syntax:atom(is_list),
                             [erl_syntax:variable(VarString)]),
                            [return_var(VarString)]),
            erl_syntax:fun_expr([EntryClause, EntryFail]);
        _ ->
            erl_syntax:fun_expr(
              [erl_syntax:clause(
                [erl_syntax:variable('_')],
                none,
                [erl_syntax:abstract({value, "undefined"})]),
              fail_clause({unexpected_type, ShapeType})])
    end.

return_var(VarName) ->
    erl_syntax:tuple([erl_syntax:atom(value), erl_syntax:variable(VarName)]).

prefix_var(Name, Prefix) ->
    string:concat(Name, Prefix).

fail_clause(Reason) ->
    erl_syntax:clause(
      [erl_syntax:variable('_')],
      none,
      [erl_syntax:application(
         erl_syntax:atom(throw),
         [erl_syntax:abstract(Reason)])]).

parameter_parser_clause(Name, Definition, Shapes) ->
    Parser = parameters_parser(Definition, Shapes),
    [Clause, _FailClause] = erl_syntax:fun_expr_clauses(Parser),
    [ValuePattern] = erl_syntax:clause_patterns(Clause),
    Key = struct_member_key(Name, Definition),
    erl_syntax:clause(
      [erl_syntax:abstract(list_to_atom(claude_name(Name))),
       ValuePattern,
       erl_syntax:variable('Acc')],
      erl_syntax:clause_guard(Clause),
      [erl_syntax:match_expr(
        erl_syntax:variable('Res'),
        erl_syntax:block_expr(erl_syntax:clause_body(Clause))),
       erl_syntax:map_expr(
         erl_syntax:variable('Acc'),
         [erl_syntax:map_field_assoc(
           erl_syntax:abstract(Key),
           erl_syntax:variable('Res'))])]).

struct_member_key(_OriginalName, #{<<"locationName">> := LocationName}) ->
    binary_to_list(LocationName);
struct_member_key(OriginalName, _Definition) ->
    binary_to_list(OriginalName).