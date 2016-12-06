-module(claude_generator).

-export([load_from_files/1, load_latest/2, load_service_from_files/3]).

-include_lib("hackney/include/hackney_lib.hrl").

load_latest(ServiceName, DefinitionsDirpath) ->
    [Latest | _] = list_api_verions(ServiceName, DefinitionsDirpath),
    load_service_from_files(ServiceName, Latest, DefinitionsDirpath).

list_api_verions(ServiceName, DefinitionsDirpath) ->
    ServiceHome = filename:join([DefinitionsDirpath, ServiceName]),
    {ok, ApiList} = file:list_dir(ServiceHome),
    lists:reverse(lists:sort(ApiList)).

load_from_files(DefinitionsDirpath) ->
    load_service_from_files("ec2", "2016-09-15", DefinitionsDirpath),
    ok.

load_service_from_files(ServiceName, ApiVersion, DefinitionsDirpath) ->
    Filename = filename:join([DefinitionsDirpath, ServiceName,
                              ApiVersion, "service-2.json"]),
    {ok, Content} = file:read_file(Filename),
    Ec2Definition = jiffy:decode(Content, [return_maps]),
    ModuleName = make_module_name(ServiceName, ApiVersion),
    {Module, Code} = generate_code(ModuleName, Ec2Definition),
    {module, _} = code:load_binary(Module, Filename, Code),
    Module.

generate_code(ModuleName, ApiDefinitions) ->
    AST = generate_module(ModuleName, ApiDefinitions),
    {ok, Module, Binary} = compile:forms(erl_syntax:revert_forms(AST)),
    {Module, Binary}.

generate_module(ModuleName, ApiDefinitions) ->
    ApiOperations = operation_functions(ApiDefinitions),
    Exports = exports(ApiOperations),
    Functions = ApiOperations,
    erl_syntax:form_list([module_name(ModuleName), Exports | Functions]).

make_module_name(ServiceName, ApiVersion) ->
    NamePrefix = "claude",
    NameSuffix = "client_generated",
    NameParts = [NamePrefix, ServiceName, ApiVersion, NameSuffix],
    list_to_atom(string:join(NameParts, "_")).

module_name(ModuleName) ->
    erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(ModuleName)]).

operation_functions(#{<<"operations">> := Operations} = ApiDefinitions) ->
    OperationDefinitions = maps:values(Operations),
    #{<<"shapes">> := Shapes, <<"metadata">> := Metadata} = ApiDefinitions,
    [operation_function(Op, Metadata, Shapes) || Op <- OperationDefinitions].

claude_name(CamelCaseBin) ->
    Words = camel_parts(binary_to_list(CamelCaseBin)),
    list_to_atom(string:join(Words, "_")).

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
    list_to_atom(string:to_lower(binary_to_list(Method))).

operation_function(Operation, Metadata, Shapes) ->
    Output = maps:get(<<"output">>, Operation, undefined),
    #{<<"name">> := Name,
      <<"input">> := InputShape,
      <<"http">> := #{
        <<"method">> := Method,
        <<"requestUri">> := Uri
    }} = Operation,
    #{<<"protocol">> := Protocol,
      <<"endpointPrefix">> := EndpointPrefix,
      <<"apiVersion">> := Version} = Metadata,
    FunctionName = erl_syntax:atom(claude_name(Name)),
    ParsedUri = hackney_url:parse_url(<<"http://example.com", Uri/binary>>),
    UriParameters = maps:from_list(hackney_url:parse_qs(ParsedUri#hackney_url.qs)),
    UriPath = ParsedUri#hackney_url.path,
    StaticParameters = UriParameters#{<<"Action">> => {value, Name},
                                      <<"Version">> => {value, Version}},
    Scope = maps:get(<<"signingName">>, Metadata, EndpointPrefix),
    StartingLocation = <<Name/binary, "Response">>,
    Args = [erl_syntax:variable('Client'), erl_syntax:variable('Parameters')],
    GetUrl = erl_syntax:application(
              erl_syntax:atom(claude_client),
              erl_syntax:atom(get_url),
              [erl_syntax:abstract(Metadata),
               erl_syntax:variable('Client')]),
    CallClient = erl_syntax:application(
                   erl_syntax:atom(claude_client),
                   erl_syntax:atom(request),
                   [erl_syntax:atom(binary_to_atom(Protocol, latin1)),
                    erl_syntax:abstract(Scope),
                    erl_syntax:abstract(http_method(Method)),
                    binary_concat([erl_syntax:variable('Url'),
                                   erl_syntax:abstract(UriPath)]),
                    erl_syntax:variable('Client'),
                    erl_syntax:variable('MergedParameters')]),
    ParseParameters = parameters_parser(InputShape, Shapes),
    FunctionBody = [erl_syntax:match_expr(
                      erl_syntax:variable('Url'),
                      GetUrl),
                    erl_syntax:match_expr(
                      erl_syntax:variable('Parser'),
                      ParseParameters),
                    erl_syntax:match_expr(
                      erl_syntax:variable('ParsedParameters'),
                      erl_syntax:application(
                       erl_syntax:variable('Parser'),
                       [erl_syntax:variable('Parameters')])),
                    erl_syntax:match_expr(
                      erl_syntax:variable('MergedParameters'),
                      erl_syntax:application(
                       erl_syntax:atom(maps),
                       erl_syntax:atom(merge),
                       [erl_syntax:variable('ParsedParameters'),
                        erl_syntax:abstract(StaticParameters)])),
                    erl_syntax:match_expr(
                      erl_syntax:variable('Response'),
                      CallClient),
                    erl_syntax:match_expr(
                      erl_syntax:variable('OutputParser'),
                      output_parser_function(StartingLocation, Output,
                                             Shapes)),
                    erl_syntax:application(
                      erl_syntax:variable('OutputParser'),
                      [erl_syntax:variable('Response')])],
    erl_syntax:function(FunctionName, [erl_syntax:clause(Args, none, FunctionBody)]).

output_parser_function(_StartingLocation, undefined, _Shapes) ->
    erl_syntax:fun_expr(
      [erl_syntax:clause(
         [erl_syntax:variable('_')],
         none,
         [erl_syntax:atom(ok)])]);
output_parser_function(StartingLocation, OutputShape, Shapes) ->
    EventFun = erl_syntax:fun_expr(
                 output_parsing_event_fun_clauses(StartingLocation,
                                                  OutputShape,
                                                  Shapes)),
    erl_syntax:fun_expr(
      [erl_syntax:clause(
         [erl_syntax:variable('Input')],
         none,

         [erl_syntax:application(
           erl_syntax:atom(erlsom),
           erl_syntax:atom(parse_sax),
           [erl_syntax:variable('Input'),
            erl_syntax:abstract([]),
            EventFun])])]).

output_parsing_event_fun_clauses(Location, OutputShape, Shapes) ->
    PassClause = erl_syntax:clause(
                   [erl_syntax:variable('_'),
                    erl_syntax:variable('Acc')],
                   none,
                   [erl_syntax:variable('Acc')]),
    CharactersClause = erl_syntax:clause(
                         [erl_syntax:tuple(
                            [erl_syntax:atom(characters),
                             erl_syntax:variable('Chars')]),
                          erl_syntax:list(
                            [erl_syntax:variable('Converter')],
                            erl_syntax:variable('Acc'))],
                         [erl_syntax:application(
                            erl_syntax:atom(is_function),
                            [erl_syntax:variable('Converter')])],
                         [erl_syntax:list(
                            [erl_syntax:application(
                               erl_syntax:variable('Converter'),
                               [erl_syntax:variable('Chars')])],
                            erl_syntax:variable('Acc'))]),
    Clauses = [CharactersClause, PassClause],
    output_parsing_event_fun_clauses(Location, OutputShape, Shapes, Clauses).

output_parsing_event_fun_clauses(Location, OutputShape, Shapes, Acc) ->
    #{<<"shape">> := ShapeName} = OutputShape,
    {ok, Shape} = maps:find(ShapeName, Shapes),
    #{<<"type">> := ShapeType} = Shape,
    case ShapeType of
        <<"structure">> ->
            #{<<"members">> := Members} = Shape,
            Start = erl_syntax:clause(
                      [erlsom_start_element(Location),
                       erl_syntax:variable('Acc')],
                      none,
                      [erl_syntax:list(
                         [erl_syntax:abstract(#{})],
                         erl_syntax:variable('Acc'))]),
            Folder = output_parsing_event_fun_structure_folder(Shapes),
            maps:fold(Folder, [Start | Acc], Members);
        <<"list">> ->
            #{<<"member">> := Member} = Shape,
            MemberLocation = maps:get(<<"locationName">>, Member, <<"item">>),
            Start = erl_syntax:clause(
                      [erlsom_start_element(Location),
                       erl_syntax:variable('Acc')],
                      none,
                      [erl_syntax:list(
                         [erl_syntax:abstract([])],
                         erl_syntax:variable('Acc'))]),
            Element = erl_syntax:clause(
                        [erlsom_end_element(MemberLocation),
                         erl_syntax:list(
                           [erl_syntax:variable('Child'),
                            erl_syntax:variable('Parent')],
                           erl_syntax:variable('Acc'))],
                        none,
                        [erl_syntax:list(
                           [erl_syntax:list(
                              [erl_syntax:variable('Child')],
                              erl_syntax:variable('Parent'))],
                           erl_syntax:variable('Acc'))]),
            output_parsing_event_fun_clauses(MemberLocation, Member,
                                             Shapes, [Start, Element | Acc]);
        _ ->
            Start = erl_syntax:clause(
                      [erlsom_start_element(Location),
                       erl_syntax:variable('Acc')],
                      none,
                      [erl_syntax:list(
                         [id_fun()],
                         erl_syntax:variable('Acc'))]),
            [Start | Acc]
    end.

output_parsing_event_fun_structure_folder(Shapes) ->
    fun(Name, Type, Clauses) ->
            Location = maps:get(<<"locationName">>, Type, Name),
            End = erl_syntax:clause(
                    [erlsom_end_element(Location),
                     erl_syntax:list(
                       [erl_syntax:variable('Child'),
                        erl_syntax:variable('Parent')],
                       erl_syntax:variable('Acc'))],
                    none,
                    [erl_syntax:list(
                       [erl_syntax:map_expr(
                          erl_syntax:variable('Parent'),
                          [erl_syntax:map_field_assoc(
                             erl_syntax:abstract(claude_name(Name)),
                             erl_syntax:variable('Child'))])],
                       erl_syntax:variable('Acc'))]),
            output_parsing_event_fun_clauses(Location, Type,
                                             Shapes, [End | Clauses])
    end.

erlsom_start_element(Location) ->
    erl_syntax:tuple(
     [erl_syntax:atom(startElement),
      erl_syntax:variable('_'),
      erl_syntax:abstract(binary_to_list(Location)),
      erl_syntax:variable('_'),
      erl_syntax:variable('_')]).

erlsom_end_element(Location) ->
    erl_syntax:tuple(
     [erl_syntax:atom(endElement),
      erl_syntax:variable('_'),
      erl_syntax:abstract(binary_to_list(Location)),
      erl_syntax:variable('_')]).

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
        <<"boolean">> ->
            value_transformer(is_boolean, atom_to_list, ShapeName);
        Integer when Integer == <<"integer">>; Integer == <<"long">> ->
            value_transformer(is_integer, integer_to_list, ShapeName);
        Float when Float == <<"float">>; Float == <<"double">> ->
            value_transformer(is_float, float_to_list, ShapeName);
        NotImplemented ->
            io:format("not implemented type: ~p~n", [NotImplemented]),
            erl_syntax:fun_expr(
              [erl_syntax:clause(
                [erl_syntax:variable('_')],
                none,
                [erl_syntax:abstract({value, "undefined"})]),
              fail_clause({unexpected_type, ShapeType})])
    end.

value_transformer(Guard, Transformer, ShapeName) ->
    Var = prefix_var("Var", binary_to_list(ShapeName)),
    EntryClause = erl_syntax:clause(
                    [erl_syntax:variable(Var)],
                    erl_syntax:application(
                      erl_syntax:atom(Guard),
                      [erl_syntax:variable(Var)]),
                    [erl_syntax:tuple(
                       [erl_syntax:atom(value),
                        erl_syntax:application(
                          erl_syntax:atom(Transformer),
                          [erl_syntax:variable(Var)])])]),
    FailClause = fail_clause({unexpected_type, ShapeName}),
    erl_syntax:fun_expr([EntryClause, FailClause]).

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
      [erl_syntax:abstract(claude_name(Name)),
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

binary_concat(BinaryList) ->
    Forms = [erl_syntax:binary_field(Body, [erl_syntax:atom(binary)]) 
             || Body <- BinaryList],
    erl_syntax:binary(Forms).

id_fun() ->
    erl_syntax:fun_expr(
      [erl_syntax:clause(
         [erl_syntax:variable('Val')],
         none,
         [erl_syntax:variable('Val')])]).
