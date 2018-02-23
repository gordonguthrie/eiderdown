-record(ast, {
          type              :: atom(),
          content    = []   :: list(),
          classnames = []   :: string(),
          scope      = html :: html | review,
          hash       = ""   :: string()
         }).

-record(optional_tag, {
          tag        = [] :: string(),
          classnames = [] :: string(),
          scope      = html :: html | review
         }).

ast_to_map(#ast{} = Rec) ->
    maps:from_list(lists:zip(record_info(fields, ast), tl(tuple_to_list(Rec)))).

optional_tag_to_map(#optional_tag{} = Rec) ->
    maps:from_list(lists:zip(record_info(fields, optional_tag),
                             tl(tuple_to_list(Rec)))).

map_to_ast(Map) ->
    #ast{type       = maps:get(type, Map),
         content    = maps:get(content, Map),
         classnames = maps:get(classnames, Map),
         scope      = maps:get(classnames, Map),
         hash       = maps:get(hash, Map)
        }.

map_to_optional_tag(Map) ->
    #optional_tag{tag        = maps:get(tag, Map),
                  classnames = maps:get(classnames, Map),
                  scope      = maps:get(scope, Map)
                  }.
