#define BOOST_SPIRIT_DEBUG
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/repository/include/qi_distinct.hpp>
#include <boost/fusion/adapted.hpp>
#include <map>
#include <set>

namespace Model {
    ////////////////////////
    // Shared primitives
    using Id = std::string;

    using Attributes = std::map<Id, std::string>;

    enum class CompassPoint { n, ne, e, se, s, sw, w, nw, c, _ };
    enum class GraphKind { directed, undirected };

    struct NodeRef {
        Id id;
        Id port;
        CompassPoint compass_pt = CompassPoint::_;
    };

    ////////////////////////
    // Domain Representation
    static inline std::string get(Attributes const& attr, std::string const& key, std::string const& default_value = {}) {
        auto it = attr.find(key);
        return it == attr.end()? default_value : it->second;
    }

    struct Node {
        Id id;
        Attributes attributes;

        std::string label() const { return get(attributes, "label", id); }
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
        std::set<Id> owned_nodes;
        SubGraphs subgraphs;

        std::string label() const { return get(attributes, "label", id); }
    };

    using Nodes = std::map<Id, Node>;

    struct MainGraph {
        GraphKind kind;
        Nodes all_nodes;
        SubGraph graph;
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

BOOST_FUSION_ADAPT_STRUCT(Model::NodeRef, id, port, compass_pt)
BOOST_FUSION_ADAPT_STRUCT(Ast::AttrStmt, group, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::Attr, key, value)
BOOST_FUSION_ADAPT_STRUCT(Ast::NodeStmt, node_id, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::EdgeStmt, hops, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::Graph, id, stmt_list)
BOOST_FUSION_ADAPT_STRUCT(Ast::GraphViz, strict, kind, graph)

#ifndef NDEBUG
#include <iomanip>
#include <boost/optional/optional_io.hpp>
    namespace std {
        template <typename... Arg>
        static inline std::ostream& operator<<(std::ostream& os, std::map<Arg...> const& v) { 
            os << '[';
            for (auto& p : v)
                os << std::quoted(p.first) << '=' << std::quoted(p.second) << "; ";
            return os << ']';
        }

        template <typename... Arg>
        static inline std::ostream& operator<<(std::ostream& os, std::vector<Arg...> const& v) { 
            os << '{';
            for (auto& el : v) os << el << ";\n";
            return os << '}';
        }
    }
    namespace Model {
        static inline std::ostream& operator<<(std::ostream& os, GraphKind k) {
            switch(k) {
                case GraphKind::directed: return os << "directed";
                case GraphKind::undirected: return os << "undirected";
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

        static inline std::ostream& operator<<(std::ostream& os, NodeRef   const& v) { return os << boost::fusion::as_vector(v); }
    }
    namespace Ast {

        static inline std::ostream& operator<<(std::ostream& os, AttrStmt::Group g) {
            switch(g) {
                case AttrStmt::Group::graph: return os << "graph";
                case AttrStmt::Group::node:  return os << "node";
                case AttrStmt::Group::edge:  return os << "edge";
            };
            return os << "unknown";
        }

        static inline std::ostream& operator<<(std::ostream& os, Model::NodeRef const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Ast::AttrStmt  const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Ast::Attr      const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Ast::NodeStmt  const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Ast::EdgeStmt  const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Ast::Graph     const& v) { return os << boost::fusion::as_vector(v); }
        static inline std::ostream& operator<<(std::ostream& os, Ast::GraphViz  const& v) { return os << boost::fusion::as_vector(v); }
    }
#endif

namespace Parser {

    namespace qi = boost::spirit::qi;
    namespace px = boost::phoenix;

    template <typename It>
    struct GraphViz : qi::grammar<It, Ast::GraphViz()> {
        GraphViz() : GraphViz::base_type(start) {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
#pragma GCC diagnostic pop
            using namespace qi;
            using boost::spirit::repository::qi::distinct;
            auto kw = distinct(char_("a-zA-Z0-9_"));

            skipper 
                = space
                | ("//" >> *(char_ - eol))
                | ("/*" >> (char_ - "*/") >> "*/")
                ;

            start   = skip(skipper) [matches[kw["strict"]] >> kind_ >> graph_];

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
        using Skipper = qi::rule<It>;
        Skipper skipper;

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

#include <fstream>
#include <iomanip>

int main() {
    using It = boost::spirit::istream_iterator;
    Parser::GraphViz<It> parser;

    std::ifstream ifs("g.dot");
    It f{ifs >> std::noskipws}, l;

    Ast::GraphViz into;
    try {
        bool ok = parse(f, l, parser, into);

        if (ok) {
            std::cout << "Parse success\n";
            std::cout << into << "\n";
        } else {
            std::cout << "Parse failed\n";
        }

        if (f != l)
            std::cout << "Remaining unparsed input: '" << std::string(f,l) << "'\n";
    } catch (Parser::qi::expectation_failure<It> const& e) {
        std::cout << e.what() << ": " << e.what_ << " at " << std::string(e.first, e.last) << "\n";
    }
}
