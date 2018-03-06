-module(eiderdown_utils).

-export([
         ast_to_map/1,
         optional_tag_to_map/1,
         map_to_ast/1,
         map_to_optional_tag/1
        ]).

-include("eiderdown.hrl").
