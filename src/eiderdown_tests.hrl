-define(OPTS, [
                 #optional_tag{tag        = "quote",
                               classnames = "my classes",
                               scope      = html},
                 #optional_tag{tag        = "todo",
                               classnames = "more classes",
                               scope      = review}
                ]).

unit_test_() ->
    [
     %% simple tests

     ?_assertEqual("<p>=></p>", to_html("=>")),
     ?_assertEqual("<p>=&lt;</p>", to_html("=<")),
     ?_assertEqual("<p>&lt;=</p>", to_html("<=")),
     ?_assertEqual("<p>&lt;></p>", to_html("<>")),
     ?_assertEqual("<p>&copy;</p>", to_html("&copy;")),
     ?_assertEqual("<div>\n<p>erk</p>\n</div>\n<p>gah</p>",
                   to_html("<div>\nerk\n</div>\ngah")),
     ?_assertEqual("<div class='danjo'>\n<p>erk</p>\n</div>\n<p>gah</p>",
                   to_html("<div class='danjo'>\nerk\n</div>\ngah")),
     ?_assertEqual("<p>blah &lt;div&gt;erk&lt;/div&gt; blah</p>",
                   to_html("blah <div>erk</div> blah")),
     ?_assertEqual("<p>blah</p>\n<p>bleh</p>", to_html("blah\n\n\nbleh")),
     ?_assertEqual("<p>---blah</p>\n<p>a</p>", to_html("---blah\na")),
     ?_assertEqual("<p>555.blah</p>\n<p>a</p>", to_html("555.blah\na")),
     ?_assertEqual("<p>555 @blah</p>\n<p>a</p>", to_html("555 @blah\na")),
     ?_assertEqual("<p>blah</p>\n<p>blah</p>", to_html("blah\r\nblah\n")),
     ?_assertEqual("<p>blah</p>\n<p>blah</p>", to_html("blah\r\nblah")),
     ?_assertEqual("<p>blah</p>\n<p>blah</p>", to_html("blah\nblah")),
     ?_assertEqual("<p>[</p>\n<p>a</p>", to_html("\n[\na")),
     ?_assertEqual("<p>></p>\n<p>a</p>", to_html("\n>\na")),
     ?_assertEqual("<p>-</p>\n<p>a</p>", to_html("\n-\na")),
     ?_assertEqual("<p>[</p>\n<p>a</p>", to_html("[\na")),
     ?_assertEqual("<p>></p>\n<p>a</p>", to_html(">\na")),
     ?_assertEqual("<p>-</p>\n<p>a</p>", to_html("-\na")),
     ?_assertEqual("<p>abc`def</p>\n<p>a</p>", to_html("abc\\`def\na")),
     ?_assertEqual("<p>xyz</p>\n<p>ab:c</p>\n<p>a</p>", to_html("xyz\r\nab:c\na")),
     ?_assertEqual("<p>xyz</p>\n<p>ab:c</p>\n<p>a</p>", to_html("xyz\nab:c\na")),
     ?_assertEqual("<p>xyz    ab:c</p>\n<p>a</p>", to_html("xyz\tab:c\na")),
     ?_assertEqual("<p>xyz ab:c</p>\n<p>a</p>", to_html("xyz ab:c\na")),
     ?_assertEqual("<p>xyz(ab:c</p>\n<p>a</p>", to_html("xyz(ab:c\na")),
     ?_assertEqual("<p>xyz]ab:c</p>\n<p>a</p>", to_html("xyz]ab:c\na")),
     ?_assertEqual("<p>xyz[ab:c</p>\n<p>a</p>", to_html("xyz[ab:c\na")),
     ?_assertEqual("<p>xyz)ab:c</p>\n<p>a</p>", to_html("xyz)ab:c\na")),
     ?_assertEqual("<p>xyz(ab:c</p>\n<p>a</p>", to_html("xyz(ab:c\na")),
     ?_assertEqual("<p>xyz/ab:c</p>\n<p>a</p>", to_html("xyz/ab:c\na")),
     ?_assertEqual("<p>xyz\\ab:c</p>\n<p>a</p>", to_html("xyz\\ab:c\na")),
     ?_assertEqual("<p>xyz!ab:c</p>\n<p>a</p>", to_html("xyz!ab:c\na")),
     ?_assertEqual("<p>xyz`ab:c</p>\n<p>a</p>", to_html("xyz`ab:c\na")),
     ?_assertEqual("<p>xyz\"ab:c</p>\n<p>a</p>", to_html("xyz\"ab:c\na")),
     ?_assertEqual("<p>xyz'ab:c</p>\n<p>a</p>", to_html("xyz'ab:c\na")),
     ?_assertEqual("<p>xyz:ab:c</p>\n<p>a</p>", to_html("xyz:ab:c\na")),
     ?_assertEqual("<p>xyz.ab:c</p>\n<p>a</p>", to_html("xyz.ab:c\na")),
     ?_assertEqual("<p>xyz0ab:c</p>\n<p>a</p>", to_html("xyz0ab:c\na")),
     ?_assertEqual("<p>xyz9ab:c</p>\n<p>a</p>", to_html("xyz9ab:c\na")),
     ?_assertEqual("<p>xyz8ab:c</p>\n<p>a</p>", to_html("xyz8ab:c\na")),
     ?_assertEqual("<p>xyz7ab:c</p>\n<p>a</p>", to_html("xyz7ab:c\na")),
     ?_assertEqual("<p>xyz6ab:c</p>\n<p>a</p>", to_html("xyz6ab:c\na")),
     ?_assertEqual("<p>xyz5ab:c</p>\n<p>a</p>", to_html("xyz5ab:c\na")),
     ?_assertEqual("<p>xyz4ab:c</p>\n<p>a</p>", to_html("xyz4ab:c\na")),
     ?_assertEqual("<p>xyz3ab:c</p>\n<p>a</p>", to_html("xyz3ab:c\na")),
     ?_assertEqual("<p>xyz2ab:c</p>\n<p>a</p>", to_html("xyz2ab:c\na")),
     ?_assertEqual("<p>xyz1ab:c</p>\n<p>a</p>", to_html("xyz1ab:c\na")),
     ?_assertEqual("<p>xyz_ab:c</p>\n<p>a</p>", to_html("xyz_ab:c\na")),
     ?_assertEqual("<p>xyz*ab:c</p>\n<p>a</p>", to_html("xyz*ab:c\na")),
     ?_assertEqual("<p>xyz+ab:c</p>\n<p>a</p>", to_html("xyz+ab:c\na")),
     ?_assertEqual("<p>xyz>ab:c</p>\n<p>a</p>", to_html("xyz>ab:c\na")),
     ?_assertEqual("<p>xyz#ab:c</p>\n<p>a</p>", to_html("xyz#ab:c\na")),
     ?_assertEqual("<p>xyz-ab:c</p>\n<p>a</p>", to_html("xyz-ab:c\na")),
     ?_assertEqual("<p>xyz=ab:c</p>\n<p>a</p>", to_html("xyz=ab:c\na")),
     ?_assertEqual("<p>xyz/ab:c</p>\n<p>a</p>", to_html("xyz/ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", to_html("\r\n ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", to_html("\n ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", to_html("  ab:c\na")),
     ?_assertEqual("<p>( ab:c</p>\n<p>a</p>", to_html("( ab:c\na")),
     ?_assertEqual("<p>] ab:c</p>\n<p>a</p>", to_html("] ab:c\na")),
     ?_assertEqual("<p>[ ab:c</p>\n<p>a</p>", to_html("[ ab:c\na")),
     ?_assertEqual("<p>) ab:c</p>\n<p>a</p>", to_html(") ab:c\na")),
     ?_assertEqual("<p>( ab:c</p>\n<p>a</p>", to_html("( ab:c\na")),
     ?_assertEqual("<p>/ ab:c</p>\n<p>a</p>", to_html("/ ab:c\na")),
     ?_assertEqual("<p>\\ ab:c</p>\n<p>a</p>", to_html("\\ ab:c\na")),
     ?_assertEqual("<p>! ab:c</p>\n<p>a</p>", to_html("! ab:c\na")),
     ?_assertEqual("<p>` ab:c</p>\n<p>a</p>", to_html("` ab:c\na")),
     ?_assertEqual("<p>\" ab:c</p>\n<p>a</p>", to_html("\" ab:c\na")),
     ?_assertEqual("<p>' ab:c</p>\n<p>a</p>", to_html("' ab:c\na")),
     ?_assertEqual("<p>: ab:c</p>\n<p>a</p>", to_html(": ab:c\na")),
     ?_assertEqual("<p>. ab:c</p>\n<p>a</p>", to_html(". ab:c\na")),
     ?_assertEqual("<p>0 ab:c</p>\n<p>a</p>", to_html("0 ab:c\na")),
     ?_assertEqual("<p>9 ab:c</p>\n<p>a</p>", to_html("9 ab:c\na")),
     ?_assertEqual("<p>8 ab:c</p>\n<p>a</p>", to_html("8 ab:c\na")),
     ?_assertEqual("<p>7 ab:c</p>\n<p>a</p>", to_html("7 ab:c\na")),
     ?_assertEqual("<p>6 ab:c</p>\n<p>a</p>", to_html("6 ab:c\na")),
     ?_assertEqual("<p>5 ab:c</p>\n<p>a</p>", to_html("5 ab:c\na")),
     ?_assertEqual("<p>4 ab:c</p>\n<p>a</p>", to_html("4 ab:c\na")),
     ?_assertEqual("<p>3 ab:c</p>\n<p>a</p>", to_html("3 ab:c\na")),
     ?_assertEqual("<p>2 ab:c</p>\n<p>a</p>", to_html("2 ab:c\na")),
     ?_assertEqual("<p>1 ab:c</p>\n<p>a</p>", to_html("1 ab:c\na")),
     ?_assertEqual("<p>_ ab:c</p>\n<p>a</p>", to_html("_ ab:c\na")),
     ?_assertEqual("<p>= ab:c</p>\n<p>a</p>", to_html("= ab:c\na")),
     ?_assertEqual("<p>/ ab:c</p>\n<p>a</p>", to_html("/ ab:c\na")),
     ?_assertEqual("<p>&lt; /ab:c</p>\n<p>a</p>", to_html("< /ab:c\na")),
     ?_assertEqual("<p>&lt; ab:c</p>\n<p>a</p>", to_html("< ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", to_html("\r\nab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", to_html("\nab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", to_html(" ab:c\na")),
     ?_assertEqual("<p>(ab:c</p>\n<p>a</p>", to_html("(ab:c\na")),
     ?_assertEqual("<p>]ab:c</p>\n<p>a</p>", to_html("]ab:c\na")),
     ?_assertEqual("<p>[ab:c</p>\n<p>a</p>", to_html("[ab:c\na")),
     ?_assertEqual("<p>)ab:c</p>\n<p>a</p>", to_html(")ab:c\na")),
     ?_assertEqual("<p>(ab:c</p>\n<p>a</p>", to_html("(ab:c\na")),
     ?_assertEqual("<p>/ab:c</p>\n<p>a</p>", to_html("/ab:c\na")),
     ?_assertEqual("<p>\\ab:c</p>\n<p>a</p>", to_html("\\ab:c\na")),
     ?_assertEqual("<p>!ab:c</p>\n<p>a</p>", to_html("!ab:c\na")),
     ?_assertEqual("<p>`ab:c</p>\n<p>a</p>", to_html("`ab:c\na")),
     ?_assertEqual("<p>\"ab:c</p>\n<p>a</p>", to_html("\"ab:c\na")),
     ?_assertEqual("<p>'ab:c</p>\n<p>a</p>", to_html("'ab:c\na")),
     ?_assertEqual("<p>:ab:c</p>\n<p>a</p>", to_html(":ab:c\na")),
     ?_assertEqual("<p>.ab:c</p>\n<p>a</p>", to_html(".ab:c\na")),
     ?_assertEqual("<p>0ab:c</p>\n<p>a</p>", to_html("0ab:c\na")),
     ?_assertEqual("<p>9ab:c</p>\n<p>a</p>", to_html("9ab:c\na")),
     ?_assertEqual("<p>8ab:c</p>\n<p>a</p>", to_html("8ab:c\na")),
     ?_assertEqual("<p>7ab:c</p>\n<p>a</p>", to_html("7ab:c\na")),
     ?_assertEqual("<p>6ab:c</p>\n<p>a</p>", to_html("6ab:c\na")),
     ?_assertEqual("<p>5ab:c</p>\n<p>a</p>", to_html("5ab:c\na")),
     ?_assertEqual("<p>4ab:c</p>\n<p>a</p>", to_html("4ab:c\na")),
     ?_assertEqual("<p>3ab:c</p>\n<p>a</p>", to_html("3ab:c\na")),
     ?_assertEqual("<p>2ab:c</p>\n<p>a</p>", to_html("2ab:c\na")),
     ?_assertEqual("<p>1ab:c</p>\n<p>a</p>", to_html("1ab:c\na")),
     ?_assertEqual("<p>_ab:c</p>\n<p>a</p>", to_html("_ab:c\na")),
     ?_assertEqual("<p>*ab:c</p>\n<p>a</p>", to_html("*ab:c\na")),
     ?_assertEqual("<p>+ab:c</p>\n<p>a</p>", to_html("+ab:c\na")),
     ?_assertEqual("<h1>ab:c</h1>\n<p>a</p>", to_html("#ab:c\na")),
     ?_assertEqual("<p>-ab:c</p>\n<p>a</p>", to_html("-ab:c\na")),
     ?_assertEqual("<p>=ab:c</p>\n<p>a</p>", to_html("=ab:c\na")),
     ?_assertEqual("<p>/ab:c</p>\n<p>a</p>", to_html("/ab:c\na")),
     ?_assertEqual("<p>Hey</p>\n<p>Ho!</p>\n<p>Lets Go</p>", to_html("    \nHey\nHo!  \nLets Go")),
     ?_assertEqual("<p>Hey Ho</p>\n<p>Lets Go</p>", to_html("Hey Ho\t\nLets Go")),
     ?_assertEqual("<p>Hey Ho</p>\n<p>Lets Go</p>", to_html("Hey Ho  \nLets Go")),
     ?_assertEqual("<p>Hey</p>\n<p>Ho!</p>\n<p>Hardy</p>", to_html("Hey\nHo!\nHardy\n\n")),
     ?_assertEqual("<p>Hey Ho!</p>\n<p>a</p>", to_html("Hey Ho!\na")),
     ?_assertEqual("<p>3 &lt;4</p>\n<p>a</p>", to_html(" 3 <4\na")),
     ?_assertEqual("<p>3 &lt; 4</p>\n<p>a</p>", to_html(" 3 < 4\na")),
     ?_assertEqual("<p>3 > 4</p>\n<p>a</p>", to_html("3 > 4\na")),
     ?_assertEqual("<p>a</p>\n<p>b</p>\n<p>c</p>", to_html("a\nb\nc\n \n\t\n     ")),
     ?_assertEqual("<p>a</p>\n<p>b</p>\n<p>c</p>", to_html("a\nb\nc\n\n\n")),
     ?_assertEqual("", to_html("  \n")),
     ?_assertEqual("", to_html("\t\n")),
     ?_assertEqual("", to_html("\n\n")),
     ?_assertEqual("", to_html("\n")),

     %% HTML element tests
     %% well behaved inline tags

     ?_assertEqual("<p>blah <span>erk</span> blah</p>",
                   to_html("blah <span>erk</span> blah")),
     ?_assertEqual("<p>blah <span attr='true'>erk</span> blah</p>",
                   to_html("blah <span attr='true'>erk</span> blah")),
     ?_assertEqual("<p>blah</p>\n<p><img src=\"image.png\"> blah</p>",
                   to_html("blah \n<img src=\"image.png\"> blah")),
     ?_assertEqual("<p>blah</p>\n<img src=\"image.png\">\n<p>blah</p>",
                   to_html("blah \n<img src=\"image.png\">\n blah")),
     ?_assertEqual("<p>blah <a href=\"link\">link</a> blah</p>",
                   to_html("blah <a href=\"link\">link</a> blah")),
     ?_assertEqual("<p>blah</p>\n<p><a href=\"link\">link</a> blah</p>",
                   to_html("blah \n<a href=\"link\">link</a> blah")),
     ?_assertEqual("<p>blah</p>\n<a href=\"link\">\n<p>link</p>\n</a>\n<p>blah</p>",
                   to_html("blah \n<a href=\"link\">\nlink\n</a>\n blah")),
     ?_assertEqual("<p>&lt;random&gt;</p>",
                   to_html("<random>")),
     ?_assertEqual("<p>erk&lt;random&gt;</p>",
                   to_html("erk<random>")),
     ?_assertEqual("<p>erk&lt;random&gt;bnajo&lt;/random&gt;</p>",
                   to_html("erk<random>bnajo</random>")),

     %% naughty elements

     ?_assertEqual("<p>blah</p>\n<p><span>erk</p>\n<p></span> blah</p>",
                   to_html("blah \n"
                        "<span>erk\n"
                        "</span> blah")),
     ?_assertEqual("<p>blah</p>\n<span>\n<p>erk</p>\n<p></span> blah</p>",
                   to_html("blah \n<span>\nerk\n</span> blah")),
     ?_assertEqual("<p>blah <a \nhref=\"link\">link</a> blah</p>",
                   to_html("blah <a \nhref=\"link\">link</a> blah")),
     ?_assertEqual("<p>blah</p>\n<p><img src=\"image.png\"> blah</p>",
                   to_html("blah \n<img src=\"image.png\"> blah")),
     ?_assertEqual("<p>blah</p>\n<p><img \nsrc=\"image.png\"> blah</p>",
                   to_html("blah \n<img \nsrc=\"image.png\"> blah")),
     ?_assertEqual("<p>blah</p>\n<img \nsrc=\"image.png\">\n<p>blah</p>",
                   to_html("blah \n<img \nsrc=\"image.png\">\n blah")),
     ?_assertEqual("<p>blah</p>\n</a>",
                   to_html("blah\n</a>")),
     ?_assertEqual("<p>blah</img></p>",
                   to_html("blah</img>")),
     ?_assertEqual("<p>blah</a>asfds</p>",
                   to_html("blah</a fandiid>asfds\n")),
     ?_assertEqual("<img src=\"deffo\" squid>",
                   to_html("<img src=\"deffo\" squid>")),


     %% bold/italic/code tests

     ?_assertEqual("<p>should be <em>italic</em> should be <strong>bold</strong> "
                   "should be <strong><em>bold italic</em></strong></p>",
                   to_html("should be *italic* should be **bold** should be ***bold italic***")),
     ?_assertEqual("<p>some stuff <code>yaycode</code> more stuff <code>more code!</code></p>",
                   to_html("some stuff `yaycode` more stuff `more code!`")),
     ?_assertEqual("<p>Now is the winter of <code>our discontent</code> "
                   "made glorious summer by this Son of York</p>",
                   to_html("Now is the winter of `our discontent` made glorious summer "
                        "by this Son of York")),
     ?_assertEqual("<p><code>&lt;div&gt;blah&lt;/div&gt;</code></p>", to_html("`<div>blah</div>`")),
     ?_assertEqual("<p><em>_</em>blah</p>\n<p>a</p>", to_html("___blah\na")),
     ?_assertEqual("<p><em>*</em>blah</p>\n<p>a</p>", to_html("***blah\na")),
     ?_assertEqual("<p><strong><em>you</em></strong> sad bastard</p>\n<p>a</p>",
                   to_html("___you___ sad bastard\na")),
     ?_assertEqual("<p><strong>you</strong> sad bastard</p>\n<p>a</p>",
                   to_html("__you__ sad bastard\na")),
     ?_assertEqual("<p><em>you</em> sad bastard</p>\n<p>a</p>", to_html("_you_ sad bastard\na")),
     ?_assertEqual("<p><strong><em>you</em></strong> sad bastard</p>\n<p>a</p>",
                   to_html("***you*** sad bastard\na")),
     ?_assertEqual("<p><strong>you</strong> sad bastard</p>\n<p>a</p>",
                   to_html("**you** sad bastard\na")),
     ?_assertEqual("<p><em>you</em> sad bastard</p>\n<p>a</p>", to_html("*you* sad bastard\na")),
     ?_assertEqual("<p>you _sad_ bastard</p>\n<p>a</p>", to_html("you \\_sad\\_ bastard\na")),
     ?_assertEqual("<p>you *sad* bastard</p>\n<p>a</p>", to_html("you \\*sad\\* bastard\na")),
     ?_assertEqual("<p>you<em>sad</em>bastard</p>\n<p>a</p>", to_html("you_sad_bastard\na")),
     ?_assertEqual("<p>you<em>sad</em>bastard</p>\n<p>a</p>", to_html("you*sad*bastard\na")),
     ?_assertEqual("<p>you <strong><em>sad</em></strong> bastard</p>\n<p>a</p>",
                   to_html("you ___sad___ bastard\na")),
     ?_assertEqual("<p>you <strong>sad</strong> bastard</p>\n<p>a</p>",
                   to_html("you __sad__ bastard\na")),
     ?_assertEqual("<p>you <em>sad</em> bastard</p>\n<p>a</p>", to_html("you _sad_ bastard\na")),
     ?_assertEqual("<p>you <strong><em>sad</em></strong> bastard</p>\n<p>a</p>",
                   to_html("you ***sad*** bastard\na")),
     ?_assertEqual("<p>you <strong>sad</strong> bastard</p>\n<p>a</p>",
                   to_html("you **sad** bastard\na")),
     ?_assertEqual("<p>you <em>sad</em> bastard</p>\n<p>a</p>", to_html("you *sad* bastard\na")),
     ?_assertEqual("<pre><code>erk\n</code></pre>", to_html("```\nerk\n```")),
     ?_assertEqual("<pre><code>erk</code></pre>", to_html("```\nerk")),
     ?_assertEqual("<pre><code>first\n    second\n</code></pre>",
                   to_html("```\nfirst\n    second\n```")),

     %% basic lists

     ?_assertEqual("<ol>\n<li>should be <em>italic</em></li>\n"
                   "<li>should be <strong>bold</strong></li>\n"
                   "<li>should be <strong><em>bold italic</em></strong></li>\n</ol>",
                   to_html("\n 1.   should be *italic*\n"
                        "2.  should be **bold**\n"
                        " 3.  should be ***bold italic***")),
     ?_assertEqual("<ol>\n<li>a</li>\n<li>b</li>\n</ol>", to_html(" 1. a\n 2. b\n")),
     ?_assertEqual("<ol>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n</ol>", to_html("4. a\n5. b\n6. c")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>\n<p>blah</p>", to_html("4. blah\nblah")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>", to_html("555. blah")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>", to_html("4. blah")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>", to_html("1. blah")),
     ?_assertEqual("<p>*blah</p>\n<p>a</p>", to_html("*blah\na")),
     ?_assertEqual("<ul>\n<li>blah</li>\n</ul>", to_html("* blah")),
     ?_assertEqual("<ul>\n<li>ab:c</li>\n</ul>\n<p>a</p>", to_html("* ab:c\na")),
     %% single indents
     ?_assertEqual("<ul>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ul>",
                   to_html("* one\n"
                        " * two\n"
                        "  * three\n"
                        "  * four\n"
                        " * five")),
     %% double indents
     ?_assertEqual("<ul>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ul>",
                   to_html("* one\n"
                        "  * two\n"
                        "    * three\n"
                        "    * four\n"
                        "  * five")),
     %% irregular indents
     ?_assertEqual("<ul>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ul>",
                   to_html("* one\n"
                        "  * two\n"
                        "    * three\n"
                        "    * four\n"
                        " * five")),
     ?_assertEqual("<ul>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ul>",
                   to_html("* one\n"
                        "  * two\n"
                        "    * three\n"
                        "    * four\n"
                        "* five")),

     %% single indents
     ?_assertEqual("<ol>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ol>",
                   to_html(" 1. one\n"
                        "2. two\n"
                        "   44. three\n"
                        "   44. four\n"
                        " 55. five")),
     %% double indents
     ?_assertEqual("<ol>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ol>",
                   to_html(" 44. one\n"
                        "   44. two\n"
                        "     1. three\n"
                        "     2. four\n"
                        "   5. five")),
     %% irregular indents
     ?_assertEqual("<ol>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ol>",
                   to_html(" 1. one\n"
                        "  2. two\n"
                        "    33. three\n"
                        "       44.    four\n"
                        " 5. five")),
     ?_assertEqual("<ol>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ol>",
                   to_html("9. one\n"
                        "  44. two\n"
                        "    0. three\n"
                        "    4. four\n"
                        "5. five")),

     %% header tests

     ?_assertEqual("<h1>blahblah</h1>\n<p>bleh</p>", to_html("# blahblah ###\nbleh")),
     ?_assertEqual("<h6># blahblah</h6>\n<p>bleh</p>", to_html("####### blahblah\nbleh")),
     ?_assertEqual("<h6>blahblah</h6>\n<p>bleh</p>", to_html("###### blahblah\nbleh")),
     ?_assertEqual("<h5>blahblah</h5>\n<p>bleh</p>", to_html("##### blahblah\nbleh")),
     ?_assertEqual("<h4>blahblah</h4>\n<p>bleh</p>", to_html("#### blahblah\nbleh")),
     ?_assertEqual("<h3>blahblah</h3>\n<p>bleh</p>", to_html("### blahblah\nbleh")),
     ?_assertEqual("<h2>blahblah</h2>\n<p>bleh</p>", to_html("## blahblah\nbleh")),
     ?_assertEqual("<h1>blahblah</h1>\n<p>bleh</p>", to_html("# blahblah\nbleh")),
     ?_assertEqual("<h1>blahblah</h1>", to_html("# blahblah ###")),
     ?_assertEqual("<h6># blahblah</h6>", to_html("####### blahblah")),
     ?_assertEqual("<h6>blahblah</h6>", to_html("###### blahblah")),
     ?_assertEqual("<h5>blahblah</h5>", to_html("##### blahblah")),
     ?_assertEqual("<h4>blahblah</h4>", to_html("#### blahblah")),
     ?_assertEqual("<h3>blahblah</h3>", to_html("### blahblah")),
     ?_assertEqual("<h2>blahblah</h2>", to_html("## blahblah")),
     ?_assertEqual("<h1>blahblah</h1>", to_html("# blahblah")),
     ?_assertEqual("<h6>#</h6>\n<p>a</p>", to_html("########\na")),
     ?_assertEqual("<h6>#</h6>\n<p>a</p>", to_html("#######\na")),
     ?_assertEqual("<h5>#</h5>\n<p>a</p>", to_html("######\na")),
     ?_assertEqual("<h4>#</h4>\n<p>a</p>", to_html("#####\na")),
     ?_assertEqual("<h3>#</h3>\n<p>a</p>", to_html("####\na")),
     ?_assertEqual("<h2>#</h2>\n<p>a</p>", to_html("###\na")),
     ?_assertEqual("<h1>#</h1>\n<p>a</p>", to_html("##\na")),
     ?_assertEqual("<p>#</p>\n<p>a</p>", to_html("#\na")),
     ?_assertEqual("<h1>ab:c</h1>\n<p>a</p>", to_html("# ab:c\na")),

     %%
     %% Tests of summary functions
     %%

     ?_assertEqual("<h1>Header</h1>\n"
                   "<p>para      : 3 words</p>",
                   to_summary_from_utf8("# Header\n"
                                        " \n"
                                        "Some body text\n")),
     ?_assertEqual("<h1>Header</h1>\n"
                   "<p>para      : 3 words</p>\n"
                   "<p>para      : 2 words</p>",
                   to_summary_from_utf8("# Header\n"
                                        " \n"
                                        "Some body text\n"
                                        "another para")),
     ?_assertEqual("<p>para      : 6 words</p>",
                   to_summary_from_utf8("one **strong** three "
                                        "__four__ *five* six")),
     ?_assertEqual("<p>para      : 2 words\n"
                  "bad tags  :\n<code>&lt;span></code></p>",
                   to_summary_from_utf8("one <span> two")),
     ?_assertEqual("<p>para      : 2 words\n"
                  "good tags :\n<code>&lt;span></code></p>",
                   to_summary_from_utf8("one <span> two</span>")),
     ?_assertEqual("<p>para      : 2 words\n"
                  "good tags :\n<code>&lt;img src=\"a\"/></code></p>",
                   to_summary_from_utf8("one <img src=\"a\"/> two")),
     ?_assertEqual("<p>para      : 2 words\n"
                  "bad tags  :\n<code>&lt;img src=\"a\"></code></p>",
                   to_summary_from_utf8("one <img src=\"a\"> two")),
     ?_assertEqual("<p>para      : 1 words</p>\n"
                   "<p><code>&lt;div></code></p>\n"
                   "<p>para      : 1 words</p>\n"
                   "<p><code>&lt;/div></code></p>\n"
                   "<p>para      : 1 words</p>",
                   to_summary_from_utf8("a\n<div>\nb\n</div>\nc")),

     %%
     %% Start testing optional tags
     %%

      ?_assertEqual("<p>:comment fish</p>\n"
                   "<p>rando</p>",
                   to_html(":comment fish\nrando", [])),

     ?_assertEqual("<p class='my classes'>fish</p>\n"
                   "<p>rando</p>",
                   to_html(":quote fish\nrando", ?OPTS, html)),

     ?_assertEqual("<p>:belch fish</p>\n<p>rando</p>",
                   to_html(":belch fish\nrando", ?OPTS, html)),

     ?_assertEqual("<p>rando</p>",
                   to_html(":todo fish\nrando", ?OPTS, html)),

     ?_assertEqual("<p class='more classes'>fish</p>\n"
                   "<p>rando</p>",
                   to_html(":todo fish\nrando", ?OPTS, review))

    ].
