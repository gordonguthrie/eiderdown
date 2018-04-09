%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Gordon Guthrie
%%% @doc,
%%%
%%% @end
%%% Created : 10 Sep 2009 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------

-module(eiderdown).

-export([
         to_html/1,
         to_html/2,
         to_html/3,
         to_html_from_utf8/1,
         to_html_from_utf8/2,
         to_html_from_utf8/3,
         to_html_from_file/2,
         to_html_from_file/3,
         to_html_from_file/4,
         to_summary_from_utf8/1,
         to_summary_from_utf8/2,
         to_reviewable/1,
         to_reviewable/2,
         to_reviewable_from_utf8/1,
         to_reviewable_from_utf8/2
        ]).

%% utilities exported for use
-export([
         ast_to_map/1,
         optional_tag_to_map/1,
         map_to_ast/1,
         map_to_optional_tag/1
        ]).

-import(lists, [flatten/1, reverse/1]).

-include_lib("eunit/include/eunit.hrl").

-define(SPACE, 32).
-define(TAB,    9).
-define(LF,    10).
-define(CR,    13).
-define(NBSP, 160).
-define(AMP, $&, $a, $m, $p, $;).
-define(COPY, $&, $c, $o, $p, $y, $;).

%% Shared records
-include("eiderdown.hrl").

to_reviewable(String) ->
    to_reviewable(String, []).

to_reviewable(String, OptionalTags) ->
    _AST = get_ast(String, OptionalTags).

to_reviewable_from_utf8(Utf8) ->
    to_reviewable_from_utf8(Utf8, []).

to_reviewable_from_utf8(Utf8, OptionalTags) ->
    String = xmerl_ucs:from_utf8(Utf8),
    _AST = get_ast(String, OptionalTags).

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
to_html(String) ->
    to_html(String, []).

to_html(String, []) ->
    to_html(String, [], html).

to_html(String, OptionalTags, Scope)  when Scope == html orelse
                                           Scope == review ->
    AST = get_ast(String, OptionalTags),
    HTML = make_html(AST, Scope),
    string:strip(HTML, both, $\n).

get_ast(String, OptionalTags) ->
    %% io:format("String is ~p~n", [String]),
    Lex = lex(String),
    %% io:format("Lex is ~p~n", [Lex]),
    UntypedLines = make_lines(Lex),
    %% io:format("UntypedLines are ~p~n", [UntypedLines]),
    TypedLines = type_lines(UntypedLines, OptionalTags),
    %% io:format("TypedLines are ~p~n", [TypedLines]),
    AST = parse(TypedLines),
    %% io:format("in parse AST is ~p~n", [AST]),
    AST.

-spec to_html_from_utf8(list()) -> list().
to_html_from_utf8(Utf8) ->
    to_html_from_utf8(Utf8, []).

to_html_from_utf8(Utf8, []) ->
    to_html_from_utf8(Utf8, [], html).

to_html_from_utf8(Utf8, OptionalTags, Scope) ->
    Str = xmerl_ucs:from_utf8(Utf8),
    Res = to_html(Str, OptionalTags, Scope),
    xmerl_ucs:to_utf8(Res).

to_html_from_file(FileIn, FileOut) ->
    to_html_from_file(FileIn, FileOut, []).

to_html_from_file(FileIn, FileOut, [])->
    to_html_from_file(FileIn, FileOut, [], html).

to_html_from_file(FileIn, FileOut, OptionalTags, Scope) ->
    case file:open(FileIn, [read]) of
        {ok, Device} -> Input = get_all_lines(Device,[]),
                        Output = to_html(Input, OptionalTags, Scope),
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
%%% make_summary
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

to_summary_from_utf8(String) ->
    to_summary_from_utf8(String, []).

to_summary_from_utf8(String, OptionalTags) ->
    Lex = lex(String),
    %% io:format("Lex is ~p~n", [Lex]),
    UntypedLines = make_lines(Lex),
    %% io:format("UntypedLines are ~p~n", [UntypedLines]),
    TypedLines = type_lines(UntypedLines, OptionalTags),
    %% io:format("TypedLines are ~p~n", [TypedLines]),
    Summary = make_summary(TypedLines),
    %% io:format("Summary is ~p~n", [Summary]),
    AST = parse(Summary),
    %% io:format("in parse AST is ~p~n", [AST]),
    HTML = make_html(AST, review),
    string:strip(HTML, both, $\n).

make_summary(AST) ->
    make_s1(AST, []).

make_s1([], Acc)                  -> lists:flatten(lists:reverse(Acc));
make_s1([{linefeed, _} | T], Acc) -> make_s1(T, Acc);
make_s1([{blocktag, P} | T], Acc) -> NewAcc = summarise_blocktag(P),
                                     make_s1(T, [NewAcc | Acc]);
make_s1([{normal, P} | T],   Acc) -> NewAcc = summarise_para(P),
                                     make_s1(T, [NewAcc | Acc]);
make_s1([H | T], Acc)             -> make_s1(T, [H | Acc]).

summarise_blocktag(Para) ->
    Strings = [{string, "`" ++ Contents ++ "`"}
               || {{{_, _}, _}, Contents} <- Para],
    {normal, Strings}.

summarise_para(Para) ->
    Words = count_words(Para, 0),
    Tags = [X || {{{tag, _}, _}, _} = X <- Para],
    {GoodTags, BadTags} = sort_tags(Tags, [], []),

    GTags = case GoodTags of
                [] -> "";
                _  -> "good tags :\n" ++make_list(GoodTags)
            end,
    BTags = case BadTags of
                [] -> "";
                _  -> "bad tags  :\n" ++ make_list(BadTags)
            end,
    Text = "para      : " ++ integer_to_list(Words) ++ " words\n",
    String = string:strip(Text ++ GTags ++ BTags, right, $\n),
    {normal, [{string, String}]}.

make_list(List) ->
    Lines = [Contents || {{{tag, _Type}, _Tag}, Contents} <- List],
    %% we wrap the text in a pair of backtigs so it will render as
    %% code later down the line
    Stripped = ["`" ++ Text ++ "`\n" || Text <- Lines],
    lists:flatten(Stripped).

sort_tags([], Acc1, Acc2) ->
    {lists:reverse(Acc1), lists:reverse(Acc2)};
sort_tags([{{{tag, open},  Tag}, _} = H1,
           {{{tag, close}, Tag}, _} | T], Acc1, Acc2) ->
    sort_tags(T, [H1 | Acc1], Acc2);
sort_tags([{{{tag, self_closing}, _}, _} = H | T], Acc1, Acc2) ->
    sort_tags(T, [H | Acc1], Acc2);
sort_tags([H | T], Acc1, Acc2) ->
    sort_tags(T, Acc1, [H | Acc2]).

count_words([], N)                -> N;
count_words([{string, S} | T], N) -> Words = get_words(S),
                                     count_words(T, N + Words);
count_words([_H | T], N)          -> count_words(T, N).

get_words(String) -> length(string:tokens(String, " ")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% make_html
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_html(AST, Type) ->
    NewAST = filter_ast(AST, Type),
    %% io:format("NewAST is ~p~n", [NewAST]),
    make_html2(NewAST, []).

%% everything stays in if we are in review
filter_ast(AST, review) -> AST;
filter_ast(AST, html)   -> [X || X <- AST, X == X#ast{scope = html}].

make_html2([], Acc) ->
    flatten(reverse(Acc));
make_html2([#ast{type    = html,
                 content = HTML} | T], Acc) ->
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type       = para,
                 content    = Text,
                 classnames = CNames} | T], Acc) ->
    HTML = case CNames of
               [] -> "<p>" ++ Text ++ "</p>\n";
               _  -> "<p class='" ++ CNames ++ "'>" ++ Text ++ "</p>\n"
           end,
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type    = {heading, N},
                 content = Text} | T], Acc) ->
    HTML = "<h" ++ integer_to_list(N) ++ ">"
        ++ Text
        ++ "</h" ++ integer_to_list(N) ++ ">\n",
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type    = ol,
                 content = Lis} | T], Acc) ->
    HTML = "<ol>\n" ++ make_list_html(Lis, []) ++ "</ol>\n",
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type    = ul,
                 content = Lis} | T], Acc) ->
    HTML = "<ul>\n" ++ make_list_html(Lis, []) ++ "</ul>\n",
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type    = {code, none},
                 content = Text} | T], Acc) ->
    HTML = "<pre><code>" ++ Text ++ "</code></pre>",
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type    = {code, Class},
                 content = Text} | T], Acc) ->
    HTML = "<pre><code class='" ++ Class ++ "'>" ++ Text ++ "</code></pre>",
    make_html2(T, [HTML | Acc]);
make_html2([#ast{type    = tag,
                 content = Text} | T], Acc) ->
    make_html2(T, [Text | Acc]);
make_html2([#ast{type    = divv,
                 content = Text} | T], Acc) ->
    make_html2(T, [Text | Acc]);
make_html2([Text | T], Acc) ->
    io:format("Text is ~p~n", [Text]),
    make_html2(T, [Text | Acc]).

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

parse(TypedLines) -> RawAST = p1(TypedLines, []),
                     _AST = [add_hash(X) || X <- RawAST].

add_hash(#ast{content = C} = AST) -> AST#ast{hash = make_hash(C)}.

make_hash(Term) -> crypto:hash(md5, term_to_binary(Term)).

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

p1([{blocktag, [{{{tag, _}, _Type}, Tg}]} | T], Acc) ->
    p1(T, [#ast{type    = divv,
                content = Tg ++ "\n"}| Acc]);

%% blank lines/linefeeds are gobbled down and discarded
p1([{Type, _} | T], Acc)
  when Type == blank orelse Type == linefeed ->
    Rest = grab_empties(T),
    p1(Rest, Acc);

%% special optional tags present here
p1([{{normal, {Classnames, Scope}}, P} | T], Acc) ->
    P2 = string:strip(make_str(snip(P)), both, ?SPACE),
    p1(T, [#ast{type       = para,
                content    = P2,
                classnames = Classnames,
                scope      = Scope} | Acc]);
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
    {Type, Classes}
        = case P of
              [{string, ""}]    -> {{code, none}, ""};
              [{string, Class}] -> {{code, Class}, "eiderdown-code " ++ Class}

           end,
    {Content, Rest} = grab_for_codeblock(T, []),
    AST = #ast{type       = Type,
               content    = Content,
               classnames = Classes},
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
type_lines(Lines, OptionalTags) ->
    TypedLines = t_l1(Lines, OptionalTags, []),
    %% io:format("TypedLines before stripping ~p~n", [TypedLines]),
    strip_lines(TypedLines).

t_l1([], _OptionalTags, Acc) -> reverse(Acc);

%% types optional tags
t_l1([[{string, _S}, {colon, _} | _T] = H | T], OptionalTags, Acc) ->
    t_l1(T, OptionalTags, [type_options(H, OptionalTags) | Acc]);

%% types atx lines
t_l1([[{{md, atx}, _} | _T] = H | T], O, Acc) ->
    t_l1(T, O, [type_atx(H) | Acc]);

%% types unordered lists lines
t_l1([[{{ws, _, _}, _}, {{md, star}, _} = ST1,
       {{ws, _, _}, _} = WS1 | T1] = H | T], O, Acc) ->
    t_l1(T, O, [{type_star([ST1, WS1 | T1]), H} | Acc]);
t_l1([[{{md, star}, _}, {{ws, _, _}, _} | _T1] = H | T], O, Acc) ->
    t_l1(T, O, [{type_star(H), H} | Acc]);

%% types ordered lists...
t_l1([[{{ws, _, _}, _}, {num, _} = N1| T1] | T], O, Acc) ->
    t_l1(T, O, [type_ol([N1 | T1]) | Acc]);
t_l1([[{num, _} | _T] = H | T], O, Acc) ->
    t_l1(T, O, [type_ol(H) | Acc]);

%% Block level tags - these are look ahead they must be
%% on a single line (ie directly followed by a lf and nothing else
t_l1([[{{{tag, _Type}, Tag}, _Contents} = H | T1] = List | T], O, Acc) ->
    case is_blank(T1) of
        false -> t_l1(T, O, [{normal , List} | Acc]);
        true  -> case is_block_tag(Tag) of
                     true  -> t_l1(T, O, [{blocktag , [H]} | Acc]);
                     false ->
                         case is_inline_tag(Tag) of
                             true  -> t_l1(T, O, [{tag,    [H | T1]} | Acc]);
                             false -> t_l1(T, O, [{normal, [H | T1]} | Acc])
                         end
                 end
    end;

%% types a blank line or a code block
t_l1([[{{lf, _}, _}| []]  = H | T], O, Acc) ->
    t_l1(T, O, [{linefeed, H} | Acc]);
t_l1([[{{ws, _, _}, _} | _T1] = H | T], O, Acc) ->
    t_l1(T, O, [type_ws(H) | Acc]);

%% type codeblocks
t_l1([[{{punc, backtick}, _},
       {{punc, backtick}, _},
       {{punc, backtick}, _},
       {string, Type},
       {{lf, lf}, _}] = H | T], O, Acc) ->
    t_l1(T, O, [{{codeblock, [{string, Type}]}, H} | Acc]);
t_l1([[{{punc, backtick}, _},
       {{punc, backtick}, _},
       {{punc, backtick}, _},
       {{lf, lf}, _}] = H | T], O, Acc) ->
    t_l1(T, O, [{{codeblock, [{string, ""}]}, H} | Acc]);
t_l1([[{{punc, backtick}, _},
       {{punc, backtick}, _},
       {{punc, backtick}, _}] = H | T], O, Acc) ->
    t_l1(T, O, [{{codeblock, [{string, ""}]}, H} | Acc]);

%% Final clause...
t_l1([H | T], O, Acc) ->
    t_l1(T, O, [{normal , H} | Acc]).

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

%% we can pass in optional tags and this typer matches them
%% the format is:
%% {{"string", "classnames"}, $scope}
%% where $scope is one of [html | review]
%%
%% The idea is that if someone starts a paragraph as `:todo`
%% we will create a paragraph with the classnames "my classes"
%% and this will get a special display in the output
%%
%% if the $scope is `review` the paragraphs will be filtered out before
%% production and if it is `html` they will be retained
type_options([{string, TagType}, {colon, _} | T] = List, OptionalTags) ->
    case is_optional_tag(string:to_lower(TagType), OptionalTags) of
        false ->
            {normal, List};
        Tag ->
            {{normal, {Tag#optional_tag.classnames,
                       Tag#optional_tag.scope}}, T}
    end.

is_optional_tag(Tag, OptionalTags) ->
    lists:keyfind(Tag, #optional_tag.tag, OptionalTags).

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
l1([$<, $/ | T], A1, A2)   -> {Tag, NewT} = closingdiv(T, []),
                              l1(NewT, [], [Tag, l2(A1) | A2]);
l1([$< | T], A1, A2)       -> {Tag, NewT} = openingdiv(T),
                              l1(NewT, [], [Tag , l2(A1) | A2]);
%% these clauses are the normal lexer clauses
l1([$: | T], A1, A2)       -> l1(T, [], [{colon, ":"},      l2(A1) | A2]);
l1([$# | T], A1, A2)       -> l1(T, [], [{{md, atx}, "#"},  l2(A1) | A2]);
l1([$* | T], A1, A2)       -> l1(T, [], [{{md, star}, "*"}, l2(A1) | A2]);
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
l1([?SPACE   | T], A1, A2)   -> l1(T, [], [{{ws, sp,  1}, " "}, l2(A1) | A2]);
l1([?TAB     | T], A1, A2)   -> l1(T, [], [{{ws, tab, 2}, "\t"}, l2(A1) | A2]);
l1([?NBSP    | T], A1, A2)   -> l1(T, [], [{{ws, sp,  1}, "&nbsp"}, l2(A1) | A2]);
l1([?CR, ?LF | T], A1, A2)   -> l1(T, [], [{{lf, crlf}, [?CR , ?LF]}, l2(A1) | A2]);
l1([?LF      | T], A1, A2)   -> l1(T, [], [{{lf, lf}, [?LF]}, l2(A1) | A2]);
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
                 true  -> make_tag(Type, Tag2, C);
                 false -> make_esc_tag(Type, Tag2, C)
             end,
    m_str1(T, [TagStr | A]);
m_str1([{_, Orig} | T], A)  ->
    m_str1(T, [Orig | A]).

make_tag(open, Tag, Content) ->
    {tags, "<" ++ Tag ++ Content ++ ">"};
make_tag(close, Tag, _Content) ->
    {tags, "</" ++ Tag ++ ">"};
make_tag(self_closing, Tag, Content) ->
    {tags, "<" ++ Tag ++ Content ++ " />;"}.

make_esc_tag(open, Tag, Content) ->
    {tags, "&lt;" ++ Tag ++ Content ++ "&gt;"};
make_esc_tag(close, Tag, _Content) ->
    {tags, "&lt;/" ++ Tag ++ "&gt;"};
make_esc_tag(self_closgin, Tag, Content) ->
    {tags, "&lt;" ++ Tag ++ Content ++ " /&gt;"}.

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
