%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Gordon Guthrie
%%% @doc,
%%%
%%% @end
%%% Created : 10 Sep 2009 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------

-module(eiderdown).

-export([conv/1,
         conv_utf8/1,
         conv_file/2]).

-import(lists, [flatten/1, reverse/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).
-define(TAB,    9).
-define(LF,    10).
-define(CR,    13).
-define(NBSP, 160).
-define(AMP, $&, $a, $m, $p, $;).
-define(COPY, $&, $c, $o, $p, $y, $;).

-record(ast, {
          type         :: atom(),
          content = [] :: list()
         }).

%%% the lexer first lexes the input
%%% make_lines does 2 passes:
%%% * it chops the lexed strings into lines which it represents as a
%%%   list of lists
%%% * it then types the lines into the following:
%%% * normal lines
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - ATX header lines
%%%   - unordered lists
%%%   - ordered lists
%%%   - code blocks
%%% the parser then does its magic interpolating the references as appropriate
conv(String) -> Lex = lex(String),
                %% io:format("Lex is ~p~n", [Lex]),
                UntypedLines = make_lines(Lex),
                %% io:format("UntypedLines are ~p~n", [UntypedLines]),
                TypedLines = type_lines(UntypedLines),
                %% io:format("TypedLines are ~p~n", [TypedLines]),
                AST = parse(TypedLines),
                %% io:format("in parse AST is ~p~n", [AST]),
                HTML = make_html(AST),
                string:strip(HTML, both, $\n).

-spec conv_utf8(list()) -> list().
conv_utf8(Utf8) ->
    Str = xmerl_ucs:from_utf8(Utf8),
    Res = conv(Str),
    xmerl_ucs:to_utf8(Res).

conv_file(FileIn, FileOut) ->
    case file:open(FileIn, [read]) of
        {ok, Device} -> Input = get_all_lines(Device,[]),
                        Output = conv(Input),
                        write(FileOut, Output);
        _            -> error
    end.

get_all_lines(Device, Accum) ->
    case io:get_line(Device,"") of
        eof  -> file:close(Device),
                Accum;
        Line ->
            get_all_lines(Device,Accum ++ Line)
    end.

write(File, Text) ->
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Text]),
            file:close(Id);
        _ ->
            error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% make_html
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_html(AST) -> make_html(AST, []).

make_html([], Acc) ->
    flatten(reverse(Acc));
make_html([#ast{type    = html,
                content = HTML} | T], Acc) ->
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = para,
                content = Text} | T], Acc) ->
    HTML = "<p>" ++ Text ++ "</p>\n",
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = {heading, N},
                content = Text} | T], Acc) ->
    HTML = "<h" ++ integer_to_list(N) ++ ">"
        ++ Text
        ++ "</h" ++ integer_to_list(N) ++ ">\n",
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = ol,
                content = Lis} | T], Acc) ->
    HTML = "<ol>\n" ++ make_list_html(Lis, []) ++ "</ol>\n",
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = ul,
                content = Lis} | T], Acc) ->
    HTML = "<ul>\n" ++ make_list_html(Lis, []) ++ "</ul>\n",
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = {code, none},
                content = Text} | T], Acc) ->
    HTML = "<pre><code>" ++ Text ++ "</code></pre>",
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = {code, Class},
                content = Text} | T], Acc) ->
    HTML = "<pre><code class='" ++ Class ++ "'>" ++ Text ++ "</code></pre>",
    make_html(T, [HTML | Acc]);
make_html([#ast{type    = tag,
                content = Text} | T], Acc) ->
    make_html(T, [Text | Acc]);
make_html([Text | T], Acc) ->
    make_html(T, [Text | Acc]).

make_list_html([], Acc) -> lists:reverse(Acc);
make_list_html([#ast{type    = li,
                     content = C} | T], Acc) ->
    make_list_html(T, [make_lis(C) | Acc]).

make_lis(Text) -> "<li>" ++ Text ++ "</li>\n".


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Parse the lines
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse(TypedLines) -> p1(TypedLines, []).


%% Terminal clause
p1([], Acc)    -> reverse(Acc);

%% Tags have the highest precedence...
p1([{tag, Tag} | T], Acc) ->
    case T of
        []                -> p1([], [#ast{type    = tag,
                                          content = make_tag_str(Tag)} | Acc]);
        [{blank, _} | T2] -> p1(T2, [#ast{type    = tag,
                                          content = make_tag_str(Tag)} | Acc]);
        _Other            -> p1(T, [#ast{type    = tag,
                                         content = make_tag_str(Tag)} | Acc])
    end;

p1([{blocktag, [{{{tag, open}, Type}, Tg}] = _Tag} | T], Acc) ->
    {Block, Rest} = grab_for_blockhtml(T, Type, []),
    Str = lists:flatten([Tg, "\n" | Block]),
    p1(Rest, [Str | Acc]);

%% blank lines/linefeeds are gobbled down and discarded
p1([{Type, _} | T], Acc)
  when Type == blank orelse Type == linefeed ->
    Rest = grab_empties(T),
    p1(Rest, Acc);

%% one normal is just normal...
p1([{normal, P} | T], Acc) ->
    P2 = string:strip(make_str(snip(P)), both, ?SPACE),
        p1(T, [#ast{type    = para,
                content = P2} | Acc]);

%% atx headings
p1([{{h1, P}, _} | T], Acc) ->
    NewP = string:strip(make_str(snip(P)), right),
    p1(T,  [#ast{type    = {heading, 1},
                 content = NewP} | Acc]);
p1([{{h2, P}, _} | T], Acc) ->
    NewP = string:strip(make_str(snip(P)), right),
    p1(T,  [#ast{type    = {heading, 2},
                 content = NewP} | Acc]);
p1([{{h3, P}, _} | T], Acc) ->
    NewP = string:strip(make_str(snip(P)), right),
    p1(T,  [#ast{type    = {heading, 3},
                 content = NewP} | Acc]);
p1([{{h4, P}, _} | T], Acc) ->
    NewP = string:strip(make_str(snip(P)), right),
    p1(T,  [#ast{type    = {heading, 4},
                 content = NewP} | Acc]);
p1([{{h5, P}, _} | T], Acc) ->
    NewP = string:strip(make_str(snip(P)), right),
    p1(T,  [#ast{type    = {heading, 5},
                 content = NewP} | Acc]);
p1([{{h6, P}, _} | T], Acc) ->
    NewP = string:strip(make_str(snip(P)), right),
    p1(T,  [#ast{type    = {heading, 6},
                 content = NewP} | Acc]);

%% grab all the unordered lists
p1([{Type, _} | _T] = List, Acc) when Type == ul orelse
                                         Type == ol ->
    {ULs, NewT} = grab_list_items(List, Type, []),
    NewC = [#ast{type    = li,
                 content = make_str(snip(make_list_str(X)))}
            || {_Type, X} <- ULs],
    p1(NewT,  [#ast{type    = Type,
                    content = NewC} | Acc]);

%% codeblock consumes any following empty lines
%% and other codeblocks
p1([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], Acc) ->
    p1([{{codeblock, merge(P1, P2)}, S1 ++ S2} | T], Acc);
p1([{{codeblock, P}, _} | T], Acc) ->
    Type = case P of
               [{string, ""}]    -> {code, none};
               [{string, Class}] -> {code, Class}
           end,
    {Content, Rest} = grab_for_codeblock(T, []),
    AST = #ast{type    = Type,
               content = Content},
    p1(Rest, [AST | Acc]).

grab_list_items([], _Type, Acc) ->
    {lists:reverse(Acc), []};
grab_list_items([{Type, _} = H | T], Type, Acc) ->
    grab_list_items(T, Type, [H | Acc]);
grab_list_items(List, _, Acc) ->
    {lists:reverse(Acc), List}.

%% two terminal clauses - if there is a muck up and no terminal
%% block quote everything gets gobbled
%% if there ia terminal blockquote normal control returns after that
grab_for_codeblock([], Acc) ->
    {lists:flatten(lists:reverse(Acc)), []};
grab_for_codeblock([{{codeblock, _}, _} | T], Acc) ->
    {lists:reverse(Acc), T};
grab_for_codeblock([H | T], Acc) ->
    {_Type, Content} = H,
    Str = make_plain_str(Content),
    grab_for_codeblock(T, [Str | Acc]).

grab_for_blockhtml([], Type, Acc) ->
    {lists:reverse(["</" ++ Type ++ ">" | Acc]), []};
grab_for_blockhtml([{blocktag, [{{{tag, close}, Type}, Tg}]}
                    | T], Type,  Acc) ->
    {lists:reverse([Tg | Acc]), T};
grab_for_blockhtml([{blocktag, [{{{tag, _}, GrabType}, Tg}]}
                    | T], Type,  Acc) when GrabType =/= Type ->
    %% blocktags grabbed in a blocktag need a line ending pushed
    grab_for_blockhtml(T, Type, ["\n", Tg | Acc]);
grab_for_blockhtml([{tag, {{{tag, self_closing}, _Ty}, Tg}}
                    | T], Type, Acc) ->
    grab_for_blockhtml(T, Type, [Tg | Acc]);
grab_for_blockhtml([H | T], Type, Acc) ->
    {_Type, Content} = H,
    Str = make_plain_str(Content),
    grab_for_blockhtml(T, Type, [Str | Acc]).

grab_empties([{linefeed, _} | T]) -> grab_empties(T);
grab_empties([{blank, _} | T])    -> grab_empties(T);
grab_empties(List)                -> List.

merge(P1, P2) ->
    flatten([P1 | P2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Make the lines from the raw tokens
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_lines(Tokens) -> ml1(Tokens, [], []).

ml1([], [], A2)                     -> reverse(A2);
ml1([], A1, A2)                     -> ml1([], [], [reverse(A1) | A2]);
ml1([{{lf, _}, _} = H | T], A1, A2) -> ml1(T, [], [ml2(H, A1) | A2]);
ml1([H | T], A1, A2)                -> ml1(T, [H | A1], A2).

ml2(H, List) -> reverse([H | List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Process the lines and give each line a type. The valid types are:
%%% * normal line
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - ATX header lines
%%%   - unordered lists (including code blocks)
%%%   - ordered lists (including code blocks)
%%%   - code blocks
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_lines(Lines) ->
    TypedLines = t_l1(Lines, []),
    %% io:format("TypedLines before stripping ~p~n", [TypedLines]),
    strip_lines(TypedLines).

t_l1([], A2) -> reverse(A2);

%% types atx lines
t_l1([[{{md, atx}, _} | _T] = H | T], A2) ->
    t_l1(T, [type_atx(H) | A2]);

%% types unordered lists lines
t_l1([[{{ws, _, _}, _}, {{md, star}, _} = ST1,
       {{ws, _, _}, _} = WS1 | T1] = H | T], A2) ->
    t_l1(T, [{type_star([ST1, WS1 | T1]), H} | A2]);
t_l1([[{{md, star}, _}, {{ws, _, _}, _} | _T1] = H | T], A2) ->
    t_l1(T, [{type_star(H), H} | A2]);

%% types ordered lists...
t_l1([[{{ws, _, _}, _}, {num, _} = N1| T1] | T], A2) ->
    t_l1(T, [type_ol([N1 | T1]) | A2]);
t_l1([[{num, _} | _T] = H | T], A2) ->
    t_l1(T, [type_ol(H) | A2]);

%% Block level tags - these are look ahead they must be
%% on a single line (ie directly followed by a lf and nothing else
t_l1([[{{{tag, _Type}, Tag}, _Contents} = H | T1] = List | T], A2) ->
    case is_blank(T1) of
        false -> t_l1(T, [{normal , List} | A2]);
        true  -> case is_block_tag(Tag) of
                     true  -> t_l1(T, [{blocktag , [H]} | A2]);
                     false ->
                         case is_inline_tag(Tag) of
                             true  -> t_l1(T, [{tag,    [H | T1]} | A2]);
                             false -> t_l1(T, [{normal, [H | T1]} | A2])
                         end
                 end
    end;

%% types a blank line or a code block
t_l1([[{{lf, _}, _}| []]  = H | T], A2) ->
    t_l1(T, [{linefeed, H} | A2]);
t_l1([[{{ws, _, _}, _} | _T1] = H | T], A2) ->
    t_l1(T, [type_ws(H) | A2]);

%% type codeblocks
t_l1([[{{punc, backtick}, _},
       {{punc, backtick}, _},
       {{punc, backtick}, _},
       {string, Type},
       {{lf, lf}, _}] = H | T], Acc) ->
    t_l1(T, [{{codeblock, [{string, Type}]}, H} | Acc]);
t_l1([[{{punc, backtick}, _},
       {{punc, backtick}, _},
       {{punc, backtick}, _},
       {{lf, lf}, _}] = H | T], Acc) ->
    t_l1(T, [{{codeblock, [{string, ""}]}, H} | Acc]);
t_l1([[{{punc, backtick}, _},
       {{punc, backtick}, _},
       {{punc, backtick}, _}] = H | T], Acc) ->
    t_l1(T, [{{codeblock, [{string, ""}]}, H} | Acc]);

%% Final clause...
t_l1([H | T], A2) ->
    t_l1(T, [{normal , H} | A2]).

%% strips blanks from the beginning and end
strip_lines(List) -> reverse(strip_l1(reverse(strip_l1(List)))).

strip_l1([{linefeed, _} | T]) -> strip_l1(T);
strip_l1([{blank, _} | T])    -> strip_l1(T);
strip_l1(List)                -> List.

%%
%% Loads of type rules...
%%

is_blank([])                     -> true;
is_blank([{{lf, _}, _}    | []]) -> true;
is_blank([{{ws, _, _}, _} | T])  -> is_blank(T);
is_blank(_List)                  -> false.

is_block_tag("div")   -> true;
is_block_tag(_)       -> false.

is_inline_tag("span") -> true;
is_inline_tag("img")  -> true;
is_inline_tag("a")    -> true;
is_inline_tag(_)      -> false.

type_star(List) ->
    case List of
        [{{md, star}, _},
         {{ws, _, _}, _} | _T] -> ul;
        _Other2                    -> normal
    end.

type_ol(List) ->
    case type_ol1(List, []) of
        normal            -> {normal, List};
        ol                -> {ol, List};
        {esc_normal, Str} -> {normal, Str}
    end.
%% this line terminates on an escaped fullstop after a number
%% (but you need to drop the bslash...)
type_ol1([{num, _} = N,
          {{punc, bslash}, _},
          {{punc, fullstop}, _} = P | T], Acc) ->
    {esc_normal, flatten([reverse(Acc), N, P | T])};
%% we accumulate the digits in case we need to escape a full stop in a normal line
type_ol1([{num, _} = H | T], Acc)      ->
    type_ol1(T, [H | Acc]);
type_ol1([{{punc, fullstop}, _},
          {{ws, _, _}, _} | _T], _Acc) ->
    ol;
type_ol1(_List, _Acc)                  ->
    normal.

%% You need to understand what this function is trying to d...
%% '### blah' is fine
%% '### blah ###' is reduced to '### blah' because trailing #'s are
%% just for show but...
%% '##' is like appling '#' to '#' <-- applying 1 less styling to a single #
%% and '###' is like appling '##' to '#' etc, etc
%% but after you hit 6#'s you just get this for a single hash
%% ie '#############' is like applying '######' to a single '#'
%% but/and '######## blah' is like apply '######' to '## blah'
%% strip trailing #'s as they are decorative only...
type_atx(List) ->
    {Sz, R} = get_atx_size(List),
    A = [{{md, atx}, "#"}],
    Type =
        case is_all_hashes(R) of
            true  ->
                if
                    Sz == 1 ->
                        normal;
                    ((Sz > 1) andalso (Sz < 6)) ->
                        Ns = integer_to_list(Sz - 1),
                        Hn = list_to_atom("h" ++ Ns),
                        {Hn, A};
                    ((Sz == 6) andalso (R == [])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, lf}, "\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, crlf}, "\r\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R =/= [])) ->
                        {h6, A}
                end;
            false ->
                Ns = integer_to_list(Sz),
                Hn = list_to_atom("h" ++ Ns),
                {Hn, strip_atx(R)}
        end,
    {Type, List}.

is_all_hashes([])                   -> true;
is_all_hashes([{{md, atx}, _} | T]) -> is_all_hashes(T);
is_all_hashes([{{lf, _}, _} | []])  -> true;
is_all_hashes(_List)                -> false.

get_atx_size(List) -> g_atx_size1(List, 0).

%% this function also strips whitespace to the left...
g_atx_size1([{{md, atx}, _} = A | T], N) when N == 6 -> {6, [A | T]};
g_atx_size1([{{md, atx}, _} | T], N)                 -> g_atx_size1(T, N + 1);
g_atx_size1([{{ws, _, _}, _} | T], N)                -> g_atx_size1(T, N);
g_atx_size1(List, N)                                 -> {N, List}.

strip_atx(List) -> reverse(s_atx1(reverse(List))).

s_atx1([{{lf, _}, _}, {{md, atx}, _} | T]) -> s_atx1(T);
s_atx1([{{md, atx}, _} | T])               -> s_atx1(T);
s_atx1(List)                               -> List.

type_ws(List) ->
    case type_ws1(List) of
        blank  -> {blank, List};
        normal -> {normal, List}
    end.

type_ws1([])                     -> blank;
type_ws1([{{lf, _}, _} | []])    -> blank;
type_ws1([[] | T])               -> type_ws1(T);
type_ws1([{{ws, _, _}, _} | T])  -> type_ws1(T);
type_ws1(_L)                     -> normal.

%% make a tag into a string
make_tag_str(L) -> make_tag1(L, []).

make_tag1([], Acc) -> lists:reverse(Acc);
make_tag1([{{{tag, _Type}, _Tag}, B} | T], Acc) ->
    make_tag1(T, [B | Acc]);
make_tag1([H | T], Acc) ->
    make_tag1(T, [make_str([H]) | Acc]).

esc_tag(String) -> esc_t1(String, []).

esc_t1([], Acc)          -> lists:reverse(Acc);
esc_t1([?NBSP | T], Acc) -> esc_t1(T, [?SPACE | Acc]); % non-breaking space to space
esc_t1([H | T], Acc)     -> esc_t1(T, [H | Acc]).

%% if it is a list we need to discard the initial white space...
%% and the '
make_list_str([{{ws, _, _}, _}       | T]) -> make_list_str(T);
make_list_str([{{md, star}, _}       | T]) -> make_list_str(T);
make_list_str([{num, _}              | T]) -> make_list_str(T);
make_list_str([{{punc, fullstop}, _} | T]) -> make_list_str(T);
make_list_str(List)                        -> List.

snip(List) -> List2 = reverse(List),
              case List2 of
                  [{{lf, _}, _} | T] -> lists:reverse(T);
                  _                  -> List
              end.

%% end of ref processing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Build the Lexed Token List
%%% This is a two part lexer, first it chunks the input and then on the second
%%% pass it gathers it into lines and types the lines
%%%
%%% NOTE that there are two different styles of processing lines:
%%% * markdown transformed
%%% * block
%%% inside block processing the whole text is dumped and just url encoded
%%% and the original text is always maintained during the lexing/parsing
%%% so that it can be recreated if the context requires it...
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(String) ->
    RawTokens = l1(String, [], []),
    %% io:format("RawTokens is ~p~n", [RawTokens]),
    merge_ws(RawTokens).

merge_ws(List) -> m_ws1(List, []).

m_ws1([], Acc) -> reverse(Acc);
m_ws1([{{ws, _, N1}, W1}, {{ws, _, N2}, W2} | T], Acc) ->
    m_ws1([{{ws, comp, N1 + N2}, W1 ++ W2} | T], Acc);
m_ws1([H | T], Acc) -> m_ws1(T, [H | Acc]).

%% this is the terminal head which ends the parsing...
l1([], [], A2)             -> flatten(reverse(A2));
l1([], A1, A2)             -> l1([], [], [l2(A1) | A2]);
%% these two heads capture opening and closing tags
l1([$<, $/|T], A1, A2)     -> {Tag, NewT} = closingdiv(T, []),
                              l1(NewT, [], [Tag, l2(A1) | A2]);
l1([$< | T], A1, A2)       -> {Tag, NewT} = openingdiv(T),
                              l1(NewT, [], [Tag , l2(A1) | A2]);
%% these clauses are the normal lexer clauses
l1([$# | T], A1, A2)       -> l1(T, [], [{{md, atx}, "#"},  l2(A1) | A2]);
l1([$* | T], A1, A2)       -> l1(T, [], [{{md, star}, "*"}, l2(A1) | A2]);
l1([$_ | T], A1, A2)       -> l1(T, [], [{{md, underscore}, "_"}, l2(A1) | A2]);
l1([$1 | T], A1, A2)       -> l1(T, [], [{num, "1"}, l2(A1) | A2]);
l1([$2 | T], A1, A2)       -> l1(T, [], [{num, "2"}, l2(A1) | A2]);
l1([$3 | T], A1, A2)       -> l1(T, [], [{num, "3"}, l2(A1) | A2]);
l1([$4 | T], A1, A2)       -> l1(T, [], [{num, "4"}, l2(A1) | A2]);
l1([$5 | T], A1, A2)       -> l1(T, [], [{num, "5"}, l2(A1) | A2]);
l1([$6 | T], A1, A2)       -> l1(T, [], [{num, "6"}, l2(A1) | A2]);
l1([$7 | T], A1, A2)       -> l1(T, [], [{num, "7"}, l2(A1) | A2]);
l1([$8 | T], A1, A2)       -> l1(T, [], [{num, "8"}, l2(A1) | A2]);
l1([$9 | T], A1, A2)       -> l1(T, [], [{num, "9"}, l2(A1) | A2]);
l1([$0 | T], A1, A2)       -> l1(T, [], [{num, "0"}, l2(A1) | A2]);
l1([$. | T], A1, A2)       -> l1(T, [], [{{punc, fullstop}, "."}, l2(A1) | A2]); %"
l1([$` | T], A1, A2)       -> l1(T, [], [{{punc, backtick}, "`"}, l2(A1) | A2]); %"
%% note there is a special 'whitespace' {{ws, none, 1}, ""} which is used to generate non-space
%% filling whitespace for cases like '*bob* is great' which needs a non-space filling
%% whitespace prepended to trigger emphasis so it renders as "<em>bob</em> is great...
%% that 'character' doesn't exist so isn't in the lexer but appears in the parser
l1([?SPACE | T], A1, A2)   -> l1(T, [], [{{ws, sp,  1}, " "}, l2(A1) | A2]);
l1([?TAB | T], A1, A2)     -> l1(T, [], [{{ws, tab, 2}, "\t"}, l2(A1) | A2]);
l1([?NBSP | T], A1, A2)    -> l1(T, [], [{{ws, sp,  1}, "&nbsp"}, l2(A1) | A2]);
l1([?CR, ?LF | T], A1, A2) -> l1(T, [], [{{lf, crlf}, [?CR , ?LF]}, l2(A1) | A2]);
l1([?LF | T], A1, A2)      -> l1(T, [], [{{lf, lf}, [?LF]}, l2(A1) | A2]);
%% l1([?CR | T], A1, A2)      -> l1(T, [], [{{lf, cr}, [?CR]}, l2(A1) | A2]);
%% this final clause accumulates line fragments
l1([H|T], A1, A2)          -> l1(T, [H |A1] , A2).

l2([])   -> [];
l2(List) -> {string, flatten(reverse(List))}.

%% need to put in regexes for urls and e-mail addies
openingdiv(String) ->
    case get_url(String) of
        {{url, URL}, R1} -> {{url, URL}, R1};
        not_url          ->
            case get_email_addie(String) of
                {{email, EM}, R2} -> {{email, EM}, R2};
                not_email         -> openingdiv1(String, [])
            end
    end.

%% dumps out a list if it is not an opening div
openingdiv1([], Acc)         -> {flatten([{{punc, bra}, "<"}
                                          | lex(reverse(Acc))]), []};
openingdiv1([$/,$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                                Acc3 = string:to_lower(Acc2),
                                [Tag | _T] = string:tokens(Acc3, " "),
                                {{{{tag, self_closing}, Tag}, "<"
                                  ++ Acc2 ++ "/>"}, T};
%% special for non-tags
openingdiv1([$>| T], [])     -> {[{{punc, bra}, "<"},
                                  {{punc, ket}, ">"}], T};
openingdiv1([$>| T], Acc)    -> Acc2 = flatten(reverse(Acc)),
                                Acc3 = string:to_lower(Acc2),
                                [Tag | _T] = string:tokens(Acc3, " "),
                                {{{{tag, open}, Tag}, "<"
                                  ++ Acc2 ++ ">"}, T};
openingdiv1([H|T], Acc)      -> openingdiv1(T, [H | Acc]).

%% dumps out a list if it is not an closing div
closingdiv([], Acc)     -> {flatten([{{punc, bra}, "<"},
                                     {{punc, fslash}, "/"}
                                     | lex(reverse(Acc))]), []};
closingdiv([$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                            Acc3 = string:to_lower(Acc2),
                            [Tag | _T] = string:tokens(Acc3, " "),
                            {{{{tag, close}, Tag}, "</"
                              ++ Acc2 ++ ">"}, T};
closingdiv([H|T], Acc)   -> closingdiv(T, [H | Acc]).

get_url(String) -> HTTP_regex = "^(H|h)(T|t)(T|t)(P|p)(S|s)*://",
                   case re:run(String, HTTP_regex) of
                       nomatch    -> not_url;
                       {match, _} -> get_url1(String, [])
                   end.

get_url1([], Acc)            -> URL = flatten(reverse(Acc)),
                                {{url, URL}, []};
%% allow escaped kets
get_url1([$\\, $> | T], Acc) -> get_url1(T, [$>, $\\ | Acc]);
get_url1([$> | T], Acc)      -> URL = flatten(reverse(Acc)),
                                {{url, URL}, T};
get_url1([H | T], Acc)       -> get_url1(T, [H | Acc]).

get_email_addie(String) ->
    Snip_regex = ">",
    case re:run(String, Snip_regex) of
        nomatch                -> not_email;
        {match, [{N, _} | _T]} ->
            {Possible, [$> | T]} = lists:split(N, String),
            EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
                ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
                ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
                ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
                ++ "|biz|info|mobi|name|aero|jobs|museum)",
            case re:run(Possible, EMail_regex) of
                nomatch    -> not_email;
                {match, _} -> {{email, Possible}, T}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_plain_str(List) -> m_plain(List, []).

m_plain([], Acc)                           -> flatten(reverse(Acc));
m_plain([{{ws, none, 1}, none} | T], Acc)     -> m_plain(T, [" " | Acc]);
m_plain([{_, Str} | T], Acc)               -> m_plain(T, [Str | Acc]).

make_str(List) -> m_str1(List, []).

m_str1([], A) ->
    Flat = flatten(reverse(A)),
    htmlchars(Flat);
m_str1([{tags, _} = Tag | T], A) ->
    m_str1(T, [Tag | A]);
m_str1([{{{tag, Type}, Tag}, Contents} | T], A) ->
    C = strip_div(Tag, Type, Contents),
    Tag2 = esc_tag(Tag),
    TagStr = case is_inline_tag(Tag) of
                 true  -> case Type of
                              open         -> {tags, "<"  ++ Tag2 ++ C ++ ">"};
                              close        -> {tags, "</" ++ Tag2 ++      ">"};
                              self_closing -> {tags, "<"  ++ Tag2 ++ C ++ " />;"}
                          end;
                 false -> case Type of
                              open         -> {tags, "&lt;"  ++ Tag2 ++ C ++ "&gt;"};
                              close        -> {tags, "&lt;/" ++ Tag2 ++ C ++ "&gt;"};
                              self_closing -> {tags, "&lt;"  ++ Tag2 ++ C ++ " /&gt;;"}
                         end
             end,
    m_str1(T, [TagStr | A]);
m_str1([{_, Orig} | T], A)  ->
    m_str1(T, [Orig | A]).

strip_div(Tag, open, Contents) ->
    Len    = length(Tag),
    Left   = string:left(Contents, length(Contents) - 1),
    _Right = string:right(Left,    length(Left) - (Len + 1));
strip_div(Tag, self_closing, Contents) ->
    Len   = length(Tag),
    Left  = string:left(Contents, length(Contents) - 2),
    _Right = string:right(Left,   length(Left) - (Len + 1));
%% closing tags can't contain content
strip_div(_Tag, close, _Contents) ->
    "".

%% convert ascii into html characters
%% htmlencode(List) ->
%%     htmlencode(List, []).

%% htmlencode([], Acc) ->
%%     lists:flatten(lists:reverse(Acc));

%% htmlencode([$&   | Rest], Acc) -> htmlencode(Rest, ["&amp;" | Acc]);
%% htmlencode([$<   | Rest], Acc) -> htmlencode(Rest, ["&lt;" | Acc]);
%% htmlencode([$>   | Rest], Acc) -> htmlencode(Rest, ["&gt;" | Acc]);
%% htmlencode([160  | Rest], Acc) -> htmlencode(Rest, ["&nbsp;" | Acc]);
%% htmlencode([Else | Rest], Acc) -> htmlencode(Rest, [Else | Acc]).

htmlchars(List) -> htmlchars1(List, []).

htmlchars1([], Acc) -> flatten(reverse(Acc));
%% tags are just wheeched out unescaped
htmlchars1([{tags, Tag} | T], Acc)   -> htmlchars1(T, [Tag | Acc]);
%% line ends are pushed to a space..
htmlchars1([?CR, ?LF | T], Acc)      -> htmlchars1(T, ["\n" | Acc]);
htmlchars1([?LF | T], Acc)           -> htmlchars1(T, ["\n" | Acc]);
htmlchars1([?CR | T], Acc)           -> htmlchars1(T, ["\r" | Acc]);
%% emphasis is a bit strange - must be preceeded by or followed by
%% white space to work and can also be escaped
%% there is a non-space filling white space represented by the atom 'none'
%% which is created in the parser (NOT IN THE LEXER!) and which triggers
%% emphasis or strong tags being turned on...
htmlchars1([$\\, $*, $*, $* | T], A) -> htmlchars1(T, [$*, $*, $* | A]);
htmlchars1([$*, $*, $* | T], A)      -> {T2, NewA} = superstrong(T, $*),
                                        htmlchars1(T2, [NewA | A]);
%% repeat for strong
htmlchars1([$\\, $*, $* | T], A)     -> htmlchars1(T, [$*, $* | A]);
htmlchars1([$*, $* | T], A)          -> {T2, NewA} = strong(T, $*),
                                        htmlchars1(T2, [NewA | A]);
%% likewise for strong
htmlchars1([$\\, $* | T], A)         -> htmlchars1(T, [$* | A]);
htmlchars1([$* | T], A)              -> {T2, NewA} = emphasis(T, $*),
                                        htmlchars1(T2, [NewA | A]);
%% and again for underscores
htmlchars1([$\\, $_, $_, $_ | T], A) -> htmlchars1(T, [$_, $_, $_ | A]);
%% the none atom is the non-space filling whitespace
htmlchars1([$_, $_, $_ | T], A)      -> {T2, NewA} = superstrong(T, $_),
                                        htmlchars1(T2, [NewA | A]);
%% and strong
%% and again for underscores
htmlchars1([$\\, $_, $_ | T], A)     -> htmlchars1(T, [$_, $_ | A]);
htmlchars1([$_, $_ | T], A)          -> {T2, NewA} = strong(T, $_),
                                        htmlchars1(T2, [NewA | A]);
%% likewise for strong
htmlchars1([$\\, $_ | T], A)         -> htmlchars1(T, [$_ | A]);
htmlchars1([$_ | T], A)              -> {T2, NewA} = emphasis(T, $_),
                                        htmlchars1(T2, [NewA | A]);
%% handle backtick escaping
htmlchars1([$\\, $` | T], A)         -> htmlchars1(T, [$` | A]);
htmlchars1([$`, $` | T], A)          -> {T2, NewA} = dblcode(T),
                                        htmlchars1(T2, [NewA | A]);
htmlchars1([$` | T], A)              -> {T2, NewA} = code(T),
                                        htmlchars1(T2, [NewA | A]);
htmlchars1([?COPY | T], A)           -> htmlchars1(T, ["&copy;" | A]);
htmlchars1([?AMP | T], A)            -> htmlchars1(T, ["&amp;" | A]);
htmlchars1([$& | T], A)              -> htmlchars1(T, ["&amp;" | A]);
htmlchars1([$< | T], A)              -> htmlchars1(T, ["&lt;" | A]);
htmlchars1([?NBSP | T], A)           -> htmlchars1(T, ["&nbsp;" | A]);
htmlchars1([?TAB | T], A)            -> htmlchars1(T, ["    " | A]);
htmlchars1([none | T], A)            -> htmlchars1(T, A);
htmlchars1([H | T], A)               -> htmlchars1(T, [H | A]).

emphasis(List, Delim)    -> interpolate(List, Delim, "em", "" ,[]).
strong(List, Delim)      -> interpolate2(List, Delim, "strong", "", []).
superstrong(List, Delim) -> interpolate3(List, Delim, "strong", "em", "", []).
dblcode(List)            -> {T, Tag} = interpolate2(List, $`, "code", "" ,[]),
                            {T, "<pre>" ++ Tag ++ "</pre>"}.
code(List)               -> interpolateX(List, $`, "code", "", []).

%% pain in the arse - sometimes the closing tag should be preceded by
%% a "\n" and sometimes not in showdown.js
%% interpolate is for single delimiters...
interpolateX([], Delim, _Tag, _X, Acc) ->
    {[], [Delim] ++ htmlchars(reverse(Acc))};
interpolateX([Delim | T], Delim, Tag, X, Acc) ->
    {T,  "<" ++ Tag ++ ">" ++ htmlchars(reverse(Acc)) ++ X ++
         "</" ++ Tag ++ ">"};
interpolateX([H | T], Delim, Tag, X, Acc) ->
    interpolateX(T, Delim, Tag, X, [H | Acc]).

interpolate([], Delim, _Tag, _X, Acc) ->
    {[], [Delim] ++ htmlchars(reverse(Acc))};
interpolate([Delim | T], Delim, Tag, X, Acc) ->
    {T,  "<" ++ Tag ++ ">" ++ htmlchars(reverse(Acc)) ++ X ++
         "</" ++ Tag ++ ">"};
interpolate([H | T], Delim, Tag, X, Acc) ->
    interpolate(T, Delim, Tag, X, [H | Acc]).

%% interpolate two is for double delimiters...
interpolate2([], Delim, _Tag,  _X, Acc) ->
    {[], [Delim] ++ [Delim] ++ htmlchars(reverse(Acc))};
interpolate2([Delim, Delim | T], Delim, Tag, X, Acc) ->
    {T,  "<" ++ Tag ++ ">" ++ htmlchars(reverse(Acc)) ++ X ++
         "</" ++ Tag ++ ">"};
interpolate2([H | T], Delim, Tag, X, Acc) ->
    interpolate2(T, Delim, Tag, X, [H | Acc]).

%% interpolate three is for double delimiters...
interpolate3([], D, _Tag1, Tag2, _X, Acc)           ->
    {[], "<" ++ Tag2 ++ ">" ++ [D] ++ "</" ++ Tag2 ++ ">"
     ++ htmlchars(reverse(Acc))};
interpolate3([D, D, D | T], D, Tag1, Tag2, _X, Acc) ->
    {T,  "<" ++ Tag1 ++ ">" ++  "<" ++ Tag2 ++ ">"
     ++ htmlchars(reverse(Acc)) ++ "</" ++ Tag2 ++ ">"
     ++ "</" ++ Tag1 ++ ">"};
interpolate3([H | T], D, Tag1, Tag2, X, Acc) ->
    interpolate3(T, D, Tag1, Tag2, X, [H | Acc]).

%%%-------------------------------------------------------------------
%%%
%%% Unit Tests
%%%
%%%-------------------------------------------------------------------

-include("eiderdown_tests.hrl").
