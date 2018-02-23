-module(eiderdown_reconcile).

-export([
         make_reviewable/2
        ]).

make_reviewable(String, OptionalTags) ->
    Tags = [eiderdown:map_to_optional_tag(X) || X <- OptionalTags],
    AST = eiderdown:to_reviewable_from_utf8(String, Tags),
    [eiderdown:ast_to_map(X) || X <- AST].
