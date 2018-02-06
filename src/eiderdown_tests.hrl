unit_test_() ->
    [
     %% simple tests

     ?_assertEqual("<p>=></p>", conv("=>")),
     ?_assertEqual("<p>=&lt;</p>", conv("=<")),
     ?_assertEqual("<p>&lt;=</p>", conv("<=")),
     ?_assertEqual("<p>&lt;></p>", conv("<>")),
     ?_assertEqual("<p>&copy;</p>", conv("&copy;")),
     ?_assertEqual("<div>\nerk\n</div>", conv("<div>\nerk\n</div>")),
     ?_assertEqual("<p>blah &lt;div&gt;erk&lt;/div&gt; blah</p>",
                   conv("blah <div>erk</div> blah")),
     ?_assertEqual("<p>blah</p>\n<p>bleh</p>", conv("blah\n\n\nbleh")),
     ?_assertEqual("<p>---blah</p>\n<p>a</p>", conv("---blah\na")),
     ?_assertEqual("<p>555.blah</p>\n<p>a</p>", conv("555.blah\na")),
     ?_assertEqual("<p>555 @blah</p>\n<p>a</p>", conv("555 @blah\na")),
     ?_assertEqual("<p>blah</p>\n<p>blah</p>", conv("blah\r\nblah\n")),
     ?_assertEqual("<p>blah</p>\n<p>blah</p>", conv("blah\r\nblah")),
     ?_assertEqual("<p>blah</p>\n<p>blah</p>", conv("blah\nblah")),
     ?_assertEqual("<p>[</p>\n<p>a</p>", conv("\n[\na")),
     ?_assertEqual("<p>></p>\n<p>a</p>", conv("\n>\na")),
     ?_assertEqual("<p>-</p>\n<p>a</p>", conv("\n-\na")),
     ?_assertEqual("<p>[</p>\n<p>a</p>", conv("[\na")),
     ?_assertEqual("<p>></p>\n<p>a</p>", conv(">\na")),
     ?_assertEqual("<p>-</p>\n<p>a</p>", conv("-\na")),
     ?_assertEqual("<p>abc`def</p>\n<p>a</p>", conv("abc\\`def\na")),
     ?_assertEqual("<p>xyz</p>\n<p>ab:c</p>\n<p>a</p>", conv("xyz\r\nab:c\na")),
     ?_assertEqual("<p>xyz</p>\n<p>ab:c</p>\n<p>a</p>", conv("xyz\nab:c\na")),
     ?_assertEqual("<p>xyz    ab:c</p>\n<p>a</p>", conv("xyz\tab:c\na")),
     ?_assertEqual("<p>xyz ab:c</p>\n<p>a</p>", conv("xyz ab:c\na")),
     ?_assertEqual("<p>xyz(ab:c</p>\n<p>a</p>", conv("xyz(ab:c\na")),
     ?_assertEqual("<p>xyz]ab:c</p>\n<p>a</p>", conv("xyz]ab:c\na")),
     ?_assertEqual("<p>xyz[ab:c</p>\n<p>a</p>", conv("xyz[ab:c\na")),
     ?_assertEqual("<p>xyz)ab:c</p>\n<p>a</p>", conv("xyz)ab:c\na")),
     ?_assertEqual("<p>xyz(ab:c</p>\n<p>a</p>", conv("xyz(ab:c\na")),
     ?_assertEqual("<p>xyz/ab:c</p>\n<p>a</p>", conv("xyz/ab:c\na")),
     ?_assertEqual("<p>xyz\\ab:c</p>\n<p>a</p>", conv("xyz\\ab:c\na")),
     ?_assertEqual("<p>xyz!ab:c</p>\n<p>a</p>", conv("xyz!ab:c\na")),
     ?_assertEqual("<p>xyz`ab:c</p>\n<p>a</p>", conv("xyz`ab:c\na")),
     ?_assertEqual("<p>xyz\"ab:c</p>\n<p>a</p>", conv("xyz\"ab:c\na")),
     ?_assertEqual("<p>xyz'ab:c</p>\n<p>a</p>", conv("xyz'ab:c\na")),
     ?_assertEqual("<p>xyz:ab:c</p>\n<p>a</p>", conv("xyz:ab:c\na")),
     ?_assertEqual("<p>xyz.ab:c</p>\n<p>a</p>", conv("xyz.ab:c\na")),
     ?_assertEqual("<p>xyz0ab:c</p>\n<p>a</p>", conv("xyz0ab:c\na")),
     ?_assertEqual("<p>xyz9ab:c</p>\n<p>a</p>", conv("xyz9ab:c\na")),
     ?_assertEqual("<p>xyz8ab:c</p>\n<p>a</p>", conv("xyz8ab:c\na")),
     ?_assertEqual("<p>xyz7ab:c</p>\n<p>a</p>", conv("xyz7ab:c\na")),
     ?_assertEqual("<p>xyz6ab:c</p>\n<p>a</p>", conv("xyz6ab:c\na")),
     ?_assertEqual("<p>xyz5ab:c</p>\n<p>a</p>", conv("xyz5ab:c\na")),
     ?_assertEqual("<p>xyz4ab:c</p>\n<p>a</p>", conv("xyz4ab:c\na")),
     ?_assertEqual("<p>xyz3ab:c</p>\n<p>a</p>", conv("xyz3ab:c\na")),
     ?_assertEqual("<p>xyz2ab:c</p>\n<p>a</p>", conv("xyz2ab:c\na")),
     ?_assertEqual("<p>xyz1ab:c</p>\n<p>a</p>", conv("xyz1ab:c\na")),
     ?_assertEqual("<p>xyz_ab:c</p>\n<p>a</p>", conv("xyz_ab:c\na")),
     ?_assertEqual("<p>xyz*ab:c</p>\n<p>a</p>", conv("xyz*ab:c\na")),
     ?_assertEqual("<p>xyz+ab:c</p>\n<p>a</p>", conv("xyz+ab:c\na")),
     ?_assertEqual("<p>xyz>ab:c</p>\n<p>a</p>", conv("xyz>ab:c\na")),
     ?_assertEqual("<p>xyz#ab:c</p>\n<p>a</p>", conv("xyz#ab:c\na")),
     ?_assertEqual("<p>xyz-ab:c</p>\n<p>a</p>", conv("xyz-ab:c\na")),
     ?_assertEqual("<p>xyz=ab:c</p>\n<p>a</p>", conv("xyz=ab:c\na")),
     ?_assertEqual("<p>xyz/ab:c</p>\n<p>a</p>", conv("xyz/ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", conv("\r\n ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", conv("\n ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", conv("  ab:c\na")),
     ?_assertEqual("<p>( ab:c</p>\n<p>a</p>", conv("( ab:c\na")),
     ?_assertEqual("<p>] ab:c</p>\n<p>a</p>", conv("] ab:c\na")),
     ?_assertEqual("<p>[ ab:c</p>\n<p>a</p>", conv("[ ab:c\na")),
     ?_assertEqual("<p>) ab:c</p>\n<p>a</p>", conv(") ab:c\na")),
     ?_assertEqual("<p>( ab:c</p>\n<p>a</p>", conv("( ab:c\na")),
     ?_assertEqual("<p>/ ab:c</p>\n<p>a</p>", conv("/ ab:c\na")),
     ?_assertEqual("<p>\\ ab:c</p>\n<p>a</p>", conv("\\ ab:c\na")),
     ?_assertEqual("<p>! ab:c</p>\n<p>a</p>", conv("! ab:c\na")),
     ?_assertEqual("<p>` ab:c</p>\n<p>a</p>", conv("` ab:c\na")),
     ?_assertEqual("<p>\" ab:c</p>\n<p>a</p>", conv("\" ab:c\na")),
     ?_assertEqual("<p>' ab:c</p>\n<p>a</p>", conv("' ab:c\na")),
     ?_assertEqual("<p>: ab:c</p>\n<p>a</p>", conv(": ab:c\na")),
     ?_assertEqual("<p>. ab:c</p>\n<p>a</p>", conv(". ab:c\na")),
     ?_assertEqual("<p>0 ab:c</p>\n<p>a</p>", conv("0 ab:c\na")),
     ?_assertEqual("<p>9 ab:c</p>\n<p>a</p>", conv("9 ab:c\na")),
     ?_assertEqual("<p>8 ab:c</p>\n<p>a</p>", conv("8 ab:c\na")),
     ?_assertEqual("<p>7 ab:c</p>\n<p>a</p>", conv("7 ab:c\na")),
     ?_assertEqual("<p>6 ab:c</p>\n<p>a</p>", conv("6 ab:c\na")),
     ?_assertEqual("<p>5 ab:c</p>\n<p>a</p>", conv("5 ab:c\na")),
     ?_assertEqual("<p>4 ab:c</p>\n<p>a</p>", conv("4 ab:c\na")),
     ?_assertEqual("<p>3 ab:c</p>\n<p>a</p>", conv("3 ab:c\na")),
     ?_assertEqual("<p>2 ab:c</p>\n<p>a</p>", conv("2 ab:c\na")),
     ?_assertEqual("<p>1 ab:c</p>\n<p>a</p>", conv("1 ab:c\na")),
     ?_assertEqual("<p>_ ab:c</p>\n<p>a</p>", conv("_ ab:c\na")),
     ?_assertEqual("<p>= ab:c</p>\n<p>a</p>", conv("= ab:c\na")),
     ?_assertEqual("<p>/ ab:c</p>\n<p>a</p>", conv("/ ab:c\na")),
     ?_assertEqual("<p>&lt; /ab:c</p>\n<p>a</p>", conv("< /ab:c\na")),
     ?_assertEqual("<p>&lt; ab:c</p>\n<p>a</p>", conv("< ab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", conv("\r\nab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", conv("\nab:c\na")),
     ?_assertEqual("<p>ab:c</p>\n<p>a</p>", conv(" ab:c\na")),
     ?_assertEqual("<p>(ab:c</p>\n<p>a</p>", conv("(ab:c\na")),
     ?_assertEqual("<p>]ab:c</p>\n<p>a</p>", conv("]ab:c\na")),
     ?_assertEqual("<p>[ab:c</p>\n<p>a</p>", conv("[ab:c\na")),
     ?_assertEqual("<p>)ab:c</p>\n<p>a</p>", conv(")ab:c\na")),
     ?_assertEqual("<p>(ab:c</p>\n<p>a</p>", conv("(ab:c\na")),
     ?_assertEqual("<p>/ab:c</p>\n<p>a</p>", conv("/ab:c\na")),
     ?_assertEqual("<p>\\ab:c</p>\n<p>a</p>", conv("\\ab:c\na")),
     ?_assertEqual("<p>!ab:c</p>\n<p>a</p>", conv("!ab:c\na")),
     ?_assertEqual("<p>`ab:c</p>\n<p>a</p>", conv("`ab:c\na")),
     ?_assertEqual("<p>\"ab:c</p>\n<p>a</p>", conv("\"ab:c\na")),
     ?_assertEqual("<p>'ab:c</p>\n<p>a</p>", conv("'ab:c\na")),
     ?_assertEqual("<p>:ab:c</p>\n<p>a</p>", conv(":ab:c\na")),
     ?_assertEqual("<p>.ab:c</p>\n<p>a</p>", conv(".ab:c\na")),
     ?_assertEqual("<p>0ab:c</p>\n<p>a</p>", conv("0ab:c\na")),
     ?_assertEqual("<p>9ab:c</p>\n<p>a</p>", conv("9ab:c\na")),
     ?_assertEqual("<p>8ab:c</p>\n<p>a</p>", conv("8ab:c\na")),
     ?_assertEqual("<p>7ab:c</p>\n<p>a</p>", conv("7ab:c\na")),
     ?_assertEqual("<p>6ab:c</p>\n<p>a</p>", conv("6ab:c\na")),
     ?_assertEqual("<p>5ab:c</p>\n<p>a</p>", conv("5ab:c\na")),
     ?_assertEqual("<p>4ab:c</p>\n<p>a</p>", conv("4ab:c\na")),
     ?_assertEqual("<p>3ab:c</p>\n<p>a</p>", conv("3ab:c\na")),
     ?_assertEqual("<p>2ab:c</p>\n<p>a</p>", conv("2ab:c\na")),
     ?_assertEqual("<p>1ab:c</p>\n<p>a</p>", conv("1ab:c\na")),
     ?_assertEqual("<p>_ab:c</p>\n<p>a</p>", conv("_ab:c\na")),
     ?_assertEqual("<p>*ab:c</p>\n<p>a</p>", conv("*ab:c\na")),
     ?_assertEqual("<p>+ab:c</p>\n<p>a</p>", conv("+ab:c\na")),
     ?_assertEqual("<h1>ab:c</h1>\n<p>a</p>", conv("#ab:c\na")),
     ?_assertEqual("<p>-ab:c</p>\n<p>a</p>", conv("-ab:c\na")),
     ?_assertEqual("<p>=ab:c</p>\n<p>a</p>", conv("=ab:c\na")),
     ?_assertEqual("<p>/ab:c</p>\n<p>a</p>", conv("/ab:c\na")),
     ?_assertEqual("<p>Hey</p>\n<p>Ho!</p>\n<p>Lets Go</p>", conv("    \nHey\nHo!  \nLets Go")),
     ?_assertEqual("<p>Hey Ho</p>\n<p>Lets Go</p>", conv("Hey Ho\t\nLets Go")),
     ?_assertEqual("<p>Hey Ho</p>\n<p>Lets Go</p>", conv("Hey Ho  \nLets Go")),
     ?_assertEqual("<p>Hey</p>\n<p>Ho!</p>\n<p>Hardy</p>", conv("Hey\nHo!\nHardy\n\n")),
     ?_assertEqual("<p>Hey Ho!</p>\n<p>a</p>", conv("Hey Ho!\na")),
     ?_assertEqual("<p>3 &lt;4</p>\n<p>a</p>", conv(" 3 <4\na")),
     ?_assertEqual("<p>3 &lt; 4</p>\n<p>a</p>", conv(" 3 < 4\na")),
     ?_assertEqual("<p>3 > 4</p>\n<p>a</p>", conv("3 > 4\na")),
     ?_assertEqual("<p>a</p>\n<p>b</p>\n<p>c</p>", conv("a\nb\nc\n \n\t\n     ")),
     ?_assertEqual("<p>a</p>\n<p>b</p>\n<p>c</p>", conv("a\nb\nc\n\n\n")),
     ?_assertEqual("", conv("  \n")),
     ?_assertEqual("", conv("\t\n")),
     ?_assertEqual("", conv("\n\n")),
     ?_assertEqual("", conv("\n")),

     %% HTML element tests
     %% well behaved inline tags

     ?_assertEqual("<p>blah <span>erk</span> blah</p>",
                   conv("blah <span>erk</span> blah")),
     ?_assertEqual("<p>blah <span attr='true'>erk</span> blah</p>",
                   conv("blah <span attr='true'>erk</span> blah")),
     ?_assertEqual("<p>blah</p>\n<p><img src=\"image.png\"> blah</p>",
                   conv("blah \n<img src=\"image.png\"> blah")),
     ?_assertEqual("<p>blah</p>\n<img src=\"image.png\">\n<p>blah</p>",
                   conv("blah \n<img src=\"image.png\">\n blah")),
     ?_assertEqual("<p>blah <a href=\"link\">link</a> blah</p>",
                   conv("blah <a href=\"link\">link</a> blah")),
     ?_assertEqual("<p>blah</p>\n<p><a href=\"link\">link</a> blah</p>",
                   conv("blah \n<a href=\"link\">link</a> blah")),
     ?_assertEqual("<p>blah</p>\n<a href=\"link\">\n<p>link</p>\n</a>\n<p>blah</p>",
                   conv("blah \n<a href=\"link\">\nlink\n</a>\n blah")),
     ?_assertEqual("<p>&lt;random&gt;</p>",
                   conv("<random>")),
     ?_assertEqual("<p>erk&lt;random&gt;</p>",
                   conv("erk<random>")),
     ?_assertEqual("<p>erk&lt;random&gt;bnajo&lt;/random&gt;</p>",
                   conv("erk<random>bnajo</random>")),

     %% naughty elements

     ?_assertEqual("<p>blah</p>\n<p><span>erk</p>\n<p></span> blah</p>",
                   conv("blah \n"
                        "<span>erk\n"
                        "</span> blah")),
     ?_assertEqual("<p>blah</p>\n<span>\n<p>erk</p>\n<p></span> blah</p>",
                   conv("blah \n<span>\nerk\n</span> blah")),
     ?_assertEqual("<p>blah <a \nhref=\"link\">link</a> blah</p>",
                   conv("blah <a \nhref=\"link\">link</a> blah")),
     ?_assertEqual("<p>blah</p>\n<p><img src=\"image.png\"> blah</p>",
                   conv("blah \n<img src=\"image.png\"> blah")),
     ?_assertEqual("<p>blah</p>\n<p><img \nsrc=\"image.png\"> blah</p>",
                   conv("blah \n<img \nsrc=\"image.png\"> blah")),
     ?_assertEqual("<p>blah</p>\n<img \nsrc=\"image.png\">\n<p>blah</p>",
                   conv("blah \n<img \nsrc=\"image.png\">\n blah")),
     ?_assertEqual("<p>blah</p>\n</a>",
                   conv("blah\n</a>")),
     ?_assertEqual("<p>blah</img></p>",
                   conv("blah</img>")),
     ?_assertEqual("<p>blah</a>asfds</p>",
                   conv("blah</a fandiid>asfds\n")),
     ?_assertEqual("<img src=\"deffo\" squid>",
                   conv("<img src=\"deffo\" squid>")),


     %% bold/italic/code tests

     ?_assertEqual("<p>should be <em>italic</em> should be <strong>bold</strong> "
                   "should be <strong><em>bold italic</em></strong></p>",
                   conv("should be *italic* should be **bold** should be ***bold italic***")),
     ?_assertEqual("<p>some stuff <code>yaycode</code> more stuff <code>more code!</code></p>",
                   conv("some stuff `yaycode` more stuff `more code!`")),
     ?_assertEqual("<p>Now is the winter of <code>our discontent</code> "
                   "made glorious summer by this Son of York</p>",
                   conv("Now is the winter of `our discontent` made glorious summer "
                        "by this Son of York")),
     ?_assertEqual("<p><code>&lt;div&gt;blah&lt;/div&gt;</code></p>", conv("`<div>blah</div>`")),
     ?_assertEqual("<p><em>_</em>blah</p>\n<p>a</p>", conv("___blah\na")),
     ?_assertEqual("<p><em>*</em>blah</p>\n<p>a</p>", conv("***blah\na")),
     ?_assertEqual("<p><strong><em>you</em></strong> sad bastard</p>\n<p>a</p>",
                   conv("___you___ sad bastard\na")),
     ?_assertEqual("<p><strong>you</strong> sad bastard</p>\n<p>a</p>",
                   conv("__you__ sad bastard\na")),
     ?_assertEqual("<p><em>you</em> sad bastard</p>\n<p>a</p>", conv("_you_ sad bastard\na")),
     ?_assertEqual("<p><strong><em>you</em></strong> sad bastard</p>\n<p>a</p>",
                   conv("***you*** sad bastard\na")),
     ?_assertEqual("<p><strong>you</strong> sad bastard</p>\n<p>a</p>",
                   conv("**you** sad bastard\na")),
     ?_assertEqual("<p><em>you</em> sad bastard</p>\n<p>a</p>", conv("*you* sad bastard\na")),
     ?_assertEqual("<p>you _sad_ bastard</p>\n<p>a</p>", conv("you \\_sad\\_ bastard\na")),
     ?_assertEqual("<p>you *sad* bastard</p>\n<p>a</p>", conv("you \\*sad\\* bastard\na")),
     ?_assertEqual("<p>you<em>sad</em>bastard</p>\n<p>a</p>", conv("you_sad_bastard\na")),
     ?_assertEqual("<p>you<em>sad</em>bastard</p>\n<p>a</p>", conv("you*sad*bastard\na")),
     ?_assertEqual("<p>you <strong><em>sad</em></strong> bastard</p>\n<p>a</p>",
                   conv("you ___sad___ bastard\na")),
     ?_assertEqual("<p>you <strong>sad</strong> bastard</p>\n<p>a</p>",
                   conv("you __sad__ bastard\na")),
     ?_assertEqual("<p>you <em>sad</em> bastard</p>\n<p>a</p>", conv("you _sad_ bastard\na")),
     ?_assertEqual("<p>you <strong><em>sad</em></strong> bastard</p>\n<p>a</p>",
                   conv("you ***sad*** bastard\na")),
     ?_assertEqual("<p>you <strong>sad</strong> bastard</p>\n<p>a</p>",
                   conv("you **sad** bastard\na")),
     ?_assertEqual("<p>you <em>sad</em> bastard</p>\n<p>a</p>", conv("you *sad* bastard\na")),
     ?_assertEqual("<pre><code>erk\n</code></pre>", conv("```\nerk\n```")),
     ?_assertEqual("<pre><code>erk</code></pre>", conv("```\nerk")),
     ?_assertEqual("<pre><code>first\n    second\n</code></pre>",
                   conv("```\nfirst\n    second\n```")),

     %% basic lists

     ?_assertEqual("<ol>\n<li>should be <em>italic</em></li>\n"
                   "<li>should be <strong>bold</strong></li>\n"
                   "<li>should be <strong><em>bold italic</em></strong></li>\n</ol>",
                   conv("\n 1.   should be *italic*\n"
                        "2.  should be **bold**\n"
                        " 3.  should be ***bold italic***")),
     ?_assertEqual("<ol>\n<li>a</li>\n<li>b</li>\n</ol>", conv(" 1. a\n 2. b\n")),
     ?_assertEqual("<ol>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n</ol>", conv("4. a\n5. b\n6. c")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>\n<p>blah</p>", conv("4. blah\nblah")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>", conv("555. blah")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>", conv("4. blah")),
     ?_assertEqual("<ol>\n<li>blah</li>\n</ol>", conv("1. blah")),
     ?_assertEqual("<p>*blah</p>\n<p>a</p>", conv("*blah\na")),
     ?_assertEqual("<ul>\n<li>blah</li>\n</ul>", conv("* blah")),
     ?_assertEqual("<ul>\n<li>ab:c</li>\n</ul>\n<p>a</p>", conv("* ab:c\na")),
     %% single indents
     ?_assertEqual("<ul>\n"
                   "<li>one</li>\n"
                   "<li>two</li>\n"
                   "<li>three</li>\n"
                   "<li>four</li>\n"
                   "<li>five</li>\n"
                   "</ul>",
                   conv("* one\n"
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
                   conv("* one\n"
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
                   conv("* one\n"
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
                   conv("* one\n"
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
                   conv(" 1. one\n"
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
                   conv(" 44. one\n"
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
                   conv(" 1. one\n"
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
                   conv("9. one\n"
                        "  44. two\n"
                        "    0. three\n"
                        "    4. four\n"
                        "5. five")),

     %% header tests

     ?_assertEqual("<h1>blahblah</h1>\n<p>bleh</p>", conv("# blahblah ###\nbleh")),
     ?_assertEqual("<h6># blahblah</h6>\n<p>bleh</p>", conv("####### blahblah\nbleh")),
     ?_assertEqual("<h6>blahblah</h6>\n<p>bleh</p>", conv("###### blahblah\nbleh")),
     ?_assertEqual("<h5>blahblah</h5>\n<p>bleh</p>", conv("##### blahblah\nbleh")),
     ?_assertEqual("<h4>blahblah</h4>\n<p>bleh</p>", conv("#### blahblah\nbleh")),
     ?_assertEqual("<h3>blahblah</h3>\n<p>bleh</p>", conv("### blahblah\nbleh")),
     ?_assertEqual("<h2>blahblah</h2>\n<p>bleh</p>", conv("## blahblah\nbleh")),
     ?_assertEqual("<h1>blahblah</h1>\n<p>bleh</p>", conv("# blahblah\nbleh")),
     ?_assertEqual("<h1>blahblah</h1>", conv("# blahblah ###")),
     ?_assertEqual("<h6># blahblah</h6>", conv("####### blahblah")),
     ?_assertEqual("<h6>blahblah</h6>", conv("###### blahblah")),
     ?_assertEqual("<h5>blahblah</h5>", conv("##### blahblah")),
     ?_assertEqual("<h4>blahblah</h4>", conv("#### blahblah")),
     ?_assertEqual("<h3>blahblah</h3>", conv("### blahblah")),
     ?_assertEqual("<h2>blahblah</h2>", conv("## blahblah")),
     ?_assertEqual("<h1>blahblah</h1>", conv("# blahblah")),
     ?_assertEqual("<h6>#</h6>\n<p>a</p>", conv("########\na")),
     ?_assertEqual("<h6>#</h6>\n<p>a</p>", conv("#######\na")),
     ?_assertEqual("<h5>#</h5>\n<p>a</p>", conv("######\na")),
     ?_assertEqual("<h4>#</h4>\n<p>a</p>", conv("#####\na")),
     ?_assertEqual("<h3>#</h3>\n<p>a</p>", conv("####\na")),
     ?_assertEqual("<h2>#</h2>\n<p>a</p>", conv("###\na")),
     ?_assertEqual("<h1>#</h1>\n<p>a</p>", conv("##\na")),
     ?_assertEqual("<p>#</p>\n<p>a</p>", conv("#\na")),
     ?_assertEqual("<h1>ab:c</h1>\n<p>a</p>", conv("# ab:c\na"))

    ].
