//#define BOOST_SPIRIT_DEBUG
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/repository/include/qi_distinct.hpp>
#include <boost/fusion/adapted.hpp>
#include <boost/lexical_cast.hpp>
#include <map>

namespace Ast {
    using Id     = std::string;
    using NodeId = Id;

    using Attributes = std::map<Id, std::string>;

    static inline std::string get(Attributes const& attr, std::string const& key, std::string const& default_value = {}) {
        auto it = attr.find(key);
        return it == attr.end()? default_value : it->second;
    }

    struct Node {
        NodeId id;
        Attributes attributes;

        std::string label() const { return get(attributes, "label", id); }
    };

    enum class CompassPoint { n, ne, e, se, s, sw, w, nw, c, _ };

    struct NodeRef {
        NodeId id;
        Id port;
        CompassPoint compass_pt = CompassPoint::_;
    };

    struct Edge {
        NodeRef from, to;
        Attributes attributes;

        std::string label() const { return get(attributes, "label"); }
    };

    using SubGraphs = std::map<Id, struct SubGraph>;

    struct SubGraph {
        Id id;
        Attributes attributes;
        std::set<NodeId> owned;
        SubGraphs subgraphs;

        std::string label() const { return get(attributes, "label", id); }
    };

    struct GraphViz {
        enum Kind { directed, undirected } kind = undirected;

        SubGraph graph;
    };

    using Nodes = std::map<NodeId, Node>;
}

BOOST_FUSION_ADAPT_STRUCT(Ast::NodeRef, id, port, compass_pt)
BOOST_FUSION_ADAPT_STRUCT(Ast::Node, id, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::Edge, from, to, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::SubGraph, id, attributes/*, subgraphs*/)
BOOST_FUSION_ADAPT_STRUCT(Ast::GraphViz, kind, graph)

#ifndef NDEBUG
#include <iomanip>
    namespace std {
        static inline std::ostream& operator<<(std::ostream& os, Ast::Attributes const& v) { 
            os << '[';
            for (auto& p : v)
                os << std::quoted(p.first) << '=' << std::quoted(p.second) << "; ";
            return os << ']';
        }
    }
    namespace Ast {
        static inline std::ostream& operator<<(std::ostream& os, GraphViz::Kind k) {
            switch(k) {
                case GraphViz::directed: return os << "directed";
                case GraphViz::undirected: return os << "undirected";
            };
            return os << "unknown";
        }

        static inline std::ostream& operator<<(std::ostream& os, CompassPoint cp) {
            switch(cp) {
                case CompassPoint::n : return os << "n";
                case CompassPoint::ne: return os << "ne";
                case CompassPoint::e : return os << "e";
                case CompassPoint::se: return os << "se";
                case CompassPoint::s : return os << "s";
                case CompassPoint::sw: return os << "sw";
                case CompassPoint::w : return os << "w";
                case CompassPoint::nw: return os << "nw";
                case CompassPoint::c : return os << "c";
                case CompassPoint::_:  default: return os << "_";
            };
        }

        static inline std::ostream& operator<<(std::ostream& os, NodeRef  const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Node     const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Edge     const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, SubGraph const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, GraphViz const& v) { return os << boost::fusion::as_vector(v); }
    }
#endif

namespace Ast {

    struct Builder {
        GraphViz document;

        struct Scope {
            std::reference_wrapper<SubGraph> subject;
            Attributes graph, node, edge;

            Scope(SubGraph& g) : subject(g) {}
            Scope(SubGraph& g, Scope const& inherit)
                : subject(g), graph(inherit.graph), node(inherit.node), edge(inherit.edge) {}

        };
        std::list<Scope> stack { document.graph };

        Scope&    inner_frame() { return stack.back();          }
        SubGraph& graph()       { return inner_frame().subject; }

        struct AmbientAction {
            Builder& self;
            Attributes (Scope::*ambient);

            void operator()(Attributes const& a_list) const {
                auto& into = self.inner_frame().*ambient;
                for (auto& attr: a_list)
                    into[attr.first] = attr.second;
            }
        };

        AmbientAction graph_attributes() { return { *this, &Scope::graph }; }
        AmbientAction node_attributes()  { return { *this, &Scope::node  }; }
        AmbientAction edge_attributes()  { return { *this, &Scope::edge  }; }

        struct EnterAction {
            Builder& self;
            void operator()(Id const& id) const {
                auto& sub = self.stack.size() == 1
                    ? self.graph()                // main graph
                    : self.graph().subgraphs[id]; // subgraph

                sub.id = id;
                self.stack.emplace_back(sub, self.inner_frame());
            }
        };

        EnterAction enter() { return { *this }; };

        struct LeaveAction {
            Builder& self;
            void operator()() const {
                assert(self.stack.size()>1);
                self.stack.pop_back();
            }
        };

        LeaveAction leave() { return { *this }; };

        struct KindAction {
            Builder& self;
            void operator()(GraphViz::Kind kind) const {
                self.document.kind = kind;
            }
        };

        KindAction set_kind() { return { *this }; };
    };

}

namespace Parser {

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
    namespace qi = boost::spirit::qi;
    namespace px = boost::phoenix;
    using namespace qi;
    using boost::spirit::repository::qi::distinct;

    template <typename It>
    struct GraphViz : grammar<It, Ast::GraphViz()> {
        GraphViz(Ast::Builder& builder) : GraphViz::base_type(start), _builder(builder) {
            using namespace std::string_literals;
#define NOPE _val=_val
#define TODO eps[NOPE]
            auto kw = distinct(char_("a-zA-Z0-9_"));

            start   = skip(space) [-kw["strict"] >> kind_ >> graph_];

            kind_   = kw["digraph"] [ set_kind(Kind::directed),   add_arrow_(px::val("->")) ]
                    | kw["graph"]   [ set_kind(Kind::undirected), add_arrow_(px::val("--")) ]
                    ;

            graph_  = ((ID_|attr(""s)) >> &lit('{')) [enter(_1)] >> stmt_list [leave()];

            string_ = '"' >> *('\\' >> char_ | ~char_('"')) >> '"';

            ID_ = string_
                | +char_("a-zA-Z0-9_")
                ;

            stmt_list
                = '{' >> *(stmt [NOPE] >> -lit(';')) >> '}'
                ;

            subgraph_ 
                = -(kw["subgraph"] >> (ID_|attr(""s))) >> stmt_list [NOPE]
                ;

            stmt 
                = kw["graph"] >> +a_list [ set_ambient_graph(_1) ]
                | kw["node"] >> +a_list  [ set_ambient_node(_1) ]
                | kw["edge"] >> +a_list  [ set_ambient_edge(_1) ]
                | attribute
                | node_stmt
                | edge_stmt
                | subgraph_
                ;

            node_stmt = node_id >> *a_list >> !arrow_;

            node_id 
                = ID_ >> (
                        (attr(Ast::Id{}))              >> (':' >> kw[compass_pt]) >> !lit(':')
                      | (':' >> ID_ | attr(Ast::Id{})) >> (':' >> kw[compass_pt] | attr(Ast::CompassPoint::_))
                )
                ;

            attribute = ID_ >> '=' >> ID_;
            a_list    = '[' >> *(attribute >> -omit[char_(",;")]) >> ']';

            edge_stmt
                = (node_id | subgraph_) % arrow_ >> *a_list [NOPE];
                ;

            BOOST_SPIRIT_DEBUG_NODES(
                    (start)(kind_)(node_id)(a_list)(graph_)
                    (ID_)(string_)
                    (subgraph_)(stmt_list)(edge_stmt) // call marketing now :)
                    (stmt)(node_stmt)
                )
        }

      private:
        Ast::Builder& _builder;

        px::function<Ast::Builder::EnterAction> enter    { _builder.enter() };
        px::function<Ast::Builder::LeaveAction> leave    { _builder.leave() };
        px::function<Ast::Builder::KindAction>  set_kind { _builder.set_kind() };
        px::function<Ast::Builder::AmbientAction> 
            set_ambient_graph { _builder.graph_attributes() },
            set_ambient_node  { _builder.node_attributes() },
            set_ambient_edge  { _builder.edge_attributes() };

        ////////////////////////
        rule<It, Ast::GraphViz()> start;
        using Skipper = qi::space_type;

        ////////////////////////
        // Kind dependent stuff
        struct add_arrow_t {
            symbols<const char>& _ref;
            void operator()(const char* op) const { _ref.add(op); }
        };
        symbols<const char> arrow_;
        px::function<add_arrow_t> add_arrow_ { {arrow_} };

        struct CompassPoint : symbols<const char, Ast::CompassPoint> {
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

        using Kind = Ast::GraphViz::Kind;

        rule<It, Ast::SubGraph(),   Skipper> graph_;
        rule<It, Ast::Attributes(), Skipper> a_list;
        rule<It, std::pair<Ast::Id, Ast::Id>(), Skipper> attribute;
        rule<It, Ast::NodeRef(),    Skipper> node_id;

        // implicit lexemes
        rule<It, Kind()> kind_;
        rule<It, Ast::Id()> ID_;
        rule<It, std::string()> string_;

        // TODO
        rule<It, Skipper> stmt, node_stmt;
        using NodeSet = std::vector<Ast::NodeRef>;
        rule<It, NodeSet(), Skipper> subgraph_, stmt_list;
        rule<It, NodeSet(), Skipper> edge_stmt;

    };

#pragma GCC diagnostic pop 
}

#include <fstream>
#include <iomanip>

int main() {
    using It = boost::spirit::istream_iterator;
    Ast::Builder builder;
    Parser::GraphViz<It> parser {builder};

    std::ifstream ifs("g.dot");
    It f{ifs >> std::noskipws}, l;

    Ast::GraphViz into;
    try {
        bool ok = parse(f, l, parser, into);

        if (ok) {
            std::cout << "Parse success\n";
            //std::cout << builder.document.kind << " graph " << std::quoted(builder.document.graph.id) << "\n";
            std::cout << builder.document.kind << " graph " << std::quoted(builder.document.graph.id) << "\n";
        } else {
            std::cout << "Parse failed\n";
        }

        if (f != l)
            std::cout << "Remaining unparsed input: '" << std::string(f,l) << "'\n";
    } catch (Parser::expectation_failure<It> const& e) {
        std::cout << e.what() << ": " << e.what_ << " at " << std::string(e.first, e.last) << "\n";
    }
}
