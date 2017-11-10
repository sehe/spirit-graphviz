# spirit-graphviz
related to https://stackoverflow.com/questions/46334463/how-to-use-boost-spirit-list-operator-with-mandatory-minimum-amount-of-elements/46365596#46365596

> The problem is the above code will succeed for just `node1;`, which is incorrect.

You're swimming against the stream. Just `node1;` is fine in DOT. So it's probably easier to arrange your grammar to reflect it too.

## Considerations

Graphviz's grammar has quite a few idiosyncrasies that make it hard to directly translate the syntax tree into a useful graph representation. 

I think this is reflective of the fact that their own parsing functions build a graph on the fly, not attempting to represent the source syntax tree at all.

This is apparent because the semantics are stateful, with a subtle mix between global state, lexical scope and subgraph namespaces. Order of appearance in the graph matters. The fact that nodes are always share a "global" namespace _and_ can be implicitly declared is a factor that doesn't really simplify things.

## How To Solve It

Though I'm not *usually* a fan of semantic actions¹, it seems like semantic actions are the thing to use here. You could mimick the behaviour of Graphviz's parser by responding to each parsed rule with an "event" that can be handled by a stateful "builder" which cause the appropriate changes to the domain model.

However, I tried doing this, and it got really complicated, mainly because the rules' synthesized types are not convenient for building.

**Separation Of Concerns** is the key to removing such bottle-necks².

If you parse into a pure AST first, and build your Model from that, both the Parser and the semantic logic get hugely simplified.

## The Model

I invent the following `Model` representation that I think captures the semantics of GraphViz domain model nicely:

TODO

## The AST

Let's create a _separate_ set of classes to represent the source document. Note:

 - this closely follows http://www.graphviz.org/doc/info/lang.html
 - it is intentionally 1:1 with our Spirit rules later on
 - it shares the basic types with the Model (Id, NodeRef, CompassPoint) because they are literally represented in the input

<!-- -->

    namespace Model {
        using Id = std::string;

        using Attributes = std::map<Id, std::string>;
        enum class GraphKind { directed, undirected };

        enum class CompassPoint { n, ne, e, se, s, sw, w, nw, c, _ };

        struct NodeRef {
            Id id;
            Id port;
            CompassPoint compass_pt = CompassPoint::_;
        };
    }

    namespace Ast {
        using Model::CompassPoint;
        using Model::Id;
        using Model::NodeRef;
        using Model::GraphKind;
        using OptionalId = boost::optional<Id>;

        using AList    = Model::Attributes;
        using AttrList = std::vector<AList>;

        struct AttrStmt {
            enum Group { graph, node, edge } group;
            AttrList attributes;
        };

        struct Attr {
            Id key, value;

            operator std::pair<Id, Id>() const { return {key, value}; }
        };

        struct NodeStmt {
            NodeRef node_id;
            AttrList attributes;
        };

        struct EdgeStmt;
        using Stmt = boost::variant<
                AttrStmt,
                Attr,
                NodeStmt,
                boost::recursive_wrapper<EdgeStmt> // includes sub graphs
            >;

        using StmtList = std::vector<Stmt>;
        
        struct Graph {
            OptionalId id;
            StmtList stmt_list;
        };

        struct EdgeStmt {
            std::vector<boost::variant<NodeRef, Graph> > hops;
            AttrList attributes;
        };

        struct GraphViz {
            bool strict;
            GraphKind kind;
            Graph graph;
        };
    }

## The Grammar

The grammar simply closely follows the spec, and maps 1:1 to the Ast so we don't have to do any magic(¹ again).

    namespace Parser {

        namespace qi = boost::spirit::qi;
        namespace px = boost::phoenix;

        template <typename It>
        struct GraphViz : qi::grammar<It, Ast::GraphViz()> {
            GraphViz() : GraphViz::base_type(start) {
                using namespace qi;
                using boost::spirit::repository::qi::distinct;
                auto kw = distinct(char_("a-zA-Z0-9_"));

                start   = skip(space) [matches[kw["strict"]] >> kind_ >> graph_];

                kind_  %= kw["digraph"] >> attr(GraphKind::directed)   [ set_arrow_(px::val("->")) ]
                        | kw["graph"]   >> attr(GraphKind::undirected) [ set_arrow_(px::val("--")) ]
                        ;

                graph_    = -ID_ >> stmt_list;
                subgraph_ = -(kw["subgraph"] >> -ID_) >> stmt_list;

                string_   = '"' >> *('\\' >> char_ | ~char_('"')) >> '"';
                ID_       = string_ | +char_("a-zA-Z0-9_");

                stmt_list = '{' >> *(stmt >> -lit(';')) >> '}';

                stmt      = attr_stmt
                          | attribute
                          | node_stmt
                          | edge_stmt
                          ;

                attr_stmt = kw[attr_group] >> attr_list;

                attribute = ID_ >> '=' >> ID_;

                node_stmt = node_id >> -attr_list >> !arrow_;

                edge_stmt
                    = (node_id | subgraph_) % arrow_ >> -attr_list
                    ;

                a_list    = '[' >> *(attribute >> -omit[char_(",;")]) >> ']';
                attr_list = +a_list;

                node_id 
                    = ID_ >> (
                            (attr(Ast::Id{}))              >> (':' >> kw[compass_pt]) >> !lit(':')
                          | (':' >> ID_ | attr(Ast::Id{})) >> (':' >> kw[compass_pt] | attr(Ast::CompassPoint::_))
                    )
                    ;

                BOOST_SPIRIT_DEBUG_NODES(
                     (graph_) (subgraph_)
                     (a_list) (attr_list)
                     (stmt) (attr_stmt) (attribute) (node_stmt) (edge_stmt) (stmt_list)
                     (node_id)
                     (start)(kind_)(ID_)(string_)
                )
            }

          private:
            ////////////////////////
            using Skipper = qi::space_type;

            //////////////////////////////
            // Arrows depend on GraphKind
            qi::symbols<const char> arrow_;

            struct set_arrow_t { // allow dynamic setting
                qi::symbols<const char>& _ref;
                void operator()(const char* op) const { _ref.clear(); _ref.add(op); }
            };
            px::function<set_arrow_t> set_arrow_ { {arrow_} };

            ////////////////////////
            // enums using symbols<>
            struct AttrGroup : qi::symbols<const char, Ast::AttrStmt::Group> {
                AttrGroup() { add
                    ("graph", Ast::AttrStmt::Group::graph)
                    ("node", Ast::AttrStmt::Group::node)
                    ("edge", Ast::AttrStmt::Group::edge);
                }
            } attr_group;

            struct CompassPoint : qi::symbols<const char, Ast::CompassPoint> {
                CompassPoint() { add
                    ("n",  Ast::CompassPoint::n)
                    ("ne", Ast::CompassPoint::ne)
                    ("e",  Ast::CompassPoint::e)
                    ("se", Ast::CompassPoint::se)
                    ("s",  Ast::CompassPoint::s)
                    ("sw", Ast::CompassPoint::sw)
                    ("w",  Ast::CompassPoint::w)
                    ("nw", Ast::CompassPoint::nw)
                    ("c",  Ast::CompassPoint::c)
                    ("_",  Ast::CompassPoint::_);
                }
            } compass_pt;

            ////////////////////////
            // productions
            qi::rule<It, Ast::Graph(),     Skipper> graph_, subgraph_;
            qi::rule<It, Ast::AList(),     Skipper> a_list;
            qi::rule<It, Ast::AttrList(),  Skipper> attr_list;
            qi::rule<It, Ast::NodeRef(),   Skipper> node_id; // misnomer

            qi::rule<It, Ast::Stmt(),      Skipper> stmt;
            qi::rule<It, Ast::AttrStmt(),  Skipper> attr_stmt;
            qi::rule<It, Ast::Attr(),      Skipper> attribute;
            qi::rule<It, Ast::NodeStmt(),  Skipper> node_stmt;
            qi::rule<It, Ast::EdgeStmt(),  Skipper> edge_stmt;

            qi::rule<It, Ast::StmtList(),  Skipper> stmt_list;

            // implicit lexemes
            using GraphKind = Ast::GraphKind;
            qi::rule<It, Ast::GraphViz()> start;
            qi::rule<It, GraphKind()> kind_;
            qi::rule<It, Ast::Id()> ID_;
            qi::rule<It, std::string()> string_;
        };
    }

> ### DEMO TIME
>
> In fact, this part already parses GraphViz documents. No online compiler was willing to accept this (exceeding the resource limits). Here's the fully sample from this stage: https://wandbox.org/permlink/AYmxpD6lzOdhOeiS
>
> The output, from my machine (with full debug info in [pastebin](http://paste.ubuntu.com/25592283/))
>
>     Parse success
>     (0 directed ( G {(graph {["rankdir"="LR"; ];
>                       });
>                   (node {["shape"="record"; ];
>                    });
>                   ((Bar 	 _) {["label"="{ \"Bar\"|{<p1>pin 1|<p2>     2|<p3>     3|<p4>     4|<p5>     5} }"; ];
>                    });
>                   ((Foo 	 _) {["label"="{ {<data0>data0|<data1>data1|<data2>data2|<data3>data3|<data4>data4}|\"Foo\" |{<out0>out0|<out1>out1|<out2>out2|<GND>gnd|<ex0>ex0|<hi>hi|<lo>lo} }"; ];
>                    });
>                   ((Bew 	 _) {["label"="{ {<clk>clk|<syn>syn|<mux0>mux0|<mux1>mux1|<signal>signal}|\"Bew\" |{<out0>out0|<out1>out1|<out2>out2} }"; ];
>                    });
>                   ({(Bar p1	 _);
>                    (Foo data0	 _);
>                    } {});
>                   ({(Bar p2	 _);
>                    (Foo data1	 _);
>                    } {});
>                   ({(Bar p3	 _);
>                    (Foo data2	 _);
>                    } {});
>                   ({(Bar p4	 _);
>                    (Foo data3	 _);
>                    } {});
>                   ({(Bar p5	 _);
>                    (Foo data4	 _);
>                    } {});
>                   ((hijacked 	 _) {});
>                   ({(Foo out0	 _);
>                    (Bew mux0	 _);
>                    } {});
>                   ({(Foo out1	 _);
>                    (Bew mux1	 _);
>                    } {});
>                   ({(Bew clk	 _);
>                    (Foo ex0	 _);
>                    } {});
>                   ((Gate 	 _) {["label"="{ {<a>a|<b>b}|OR|{<ab>a|b} }"; ];
>                    });
>                   ({(Foo hi	 _);
>                    (Gate a	 _);
>                    } {});
>                   ({(Foo lo	 _);
>                    (Gate b	 _);
>                    } {});
>                   ({(Gate ab	 _);
>                    (Bew signal	 _);
>                    } {});
>                   ((subgraph 	 _) {});
>                   ((cluster1 	 _) {});
>                   ({(-- {(graph {["label"="G1"; ];
>                           });
>                      ((2 	 _) {});
>                      ((3 	 _) {});
>                      ({(2 	 _);
>                       (4 	 _);
>                       } {});
>                      ({(3 	 _);
>                       (9 	 _);
>                       } {});
>                      ({(3 	 _);
>                       (12 	 _);
>                       } {});
>                      ({(9 	 _);
>                       (11 	 _);
>                       } {});
>                      ({(9 	 _);
>                       (10 	 _);
>                       } {});
>                      ({(10 	 _);
>                       (3 	 _);
>                       } {});
>                   });
>                   } {});
>                   ((subgraph 	 _) {});
>                   ((cluster2 	 _) {});
>                   ({(-- {(graph {["label"="G2"; ];
>                           });
>                      ({(10 	 _);
>                       (3 	 _);
>                       } {});
>                      ((more 	 _) {});
>                      ((subgraph 	 _) {});
>                      ((clusterNested 	 _) {});
>                      ({(-- {(graph {["label"="nested"; ];
>                              });
>                         ((innermost 	 _) {});
>                         ((hijacked 	 _) {["shape"="diamond"; ];
>                          });
>                         });
>                       } {});
>                      });
>                    } {});
>                   ((subgraph 	 _) {});
>                   ((cluster1 	 _) {});
>                   ({(-- {(graph {["label"="G1_override"; ];
>                           });
>                      ({(11 	 _);
>                       (4 	 _);
>                       } {});
>                      ((last 	 _) {});
>                      ((hijacked 	 _) {});
>                      ((subgraph 	 _) {});
>                      ((clusterNested 	 _) {});
>                      ({(-- {(graph {["label"="can override nested?"; ];
>                              });
>                         ({(-- {((unnested 	 _) {});
>                            ((first_override 	 _) {});
>                            });
>                          } {["color"="red"; ];
>                          });
>                         });
>                       } {});
>                      });
>                    } {});
>                   ((10 	 _) {["shape"="circle"; ];
>                    ["color"="red"; ];
>                    });
>                   ((10 	 _) {["color"="red"; "shape"="circle"; ];
>                    });
>                   ((10 	 _) {["color"="red"; "shape"="circle"; ];
>                    });
>                   ((subgraph 	 _) {});
>                   ((clusterNested 	 _) {});
>                   ({(-- {(graph {["label"="can't override nested"; ];
>                           });
>                      ((unnested 	 _) {});
>                      ((second_override 	 _) {});
>                      });
>                    } {});
>                   ({(more 	 _);
>                    (last 	 _);
>                    } {});
>     }))
>     Remaining unparsed input: '
>     '

-----

¹ https://stackoverflow.com/questions/8259440/boost-spirit-semantic-actions-are-evil

² I classify this category of complexities "Impedance Mismatch", a term I originally learnt from Object-Relation mapping frameworks
