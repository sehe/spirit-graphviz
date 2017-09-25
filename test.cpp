#define BOOST_SPIRIT_DEBUG
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/phoenix.hpp>
#include <boost/spirit/repository/include/qi_distinct.hpp>
#include <boost/fusion/adapted.hpp>
#include <boost/lexical_cast.hpp>
#include <map>

namespace Ast {
    using Id     = std::string;
    using NodeId = Id;
    using FielId = Id;

    using Attributes = std::map<Id, std::string>;

    template <typename T = std::string>
    static inline T get(Attributes const& attr, std::string const& key, T default_value = {}) {
        auto it = attr.find(key);
        return it == attr.end()? default_value : boost::lexical_cast<T>(it->second);
    }

    struct NodeRef {
        NodeId id;
        boost::optional<FielId> field;
    };

    struct Node {
        NodeId id;
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
        SubGraphs subgraphs;

        std::string label() const { return get(attributes, "label", id); }
    };

    struct GraphViz {
        enum Kind { directed, undirected } kind;

        SubGraph graph;
    };

    using Nodes = std::map<NodeId, Node>;
}

BOOST_FUSION_ADAPT_STRUCT(Ast::NodeRef, id, field)
BOOST_FUSION_ADAPT_STRUCT(Ast::Node, id, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::Edge, from, to, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::SubGraph, id, attributes, subgraphs)
BOOST_FUSION_ADAPT_STRUCT(Ast::GraphViz, kind, graph)

#ifndef NDEBUG
    namespace Ast {
        static inline std::ostream& operator<<(std::ostream& os, GraphViz::Kind k) {
            switch(k) {
                case GraphViz::directed: return os << "directed";
                case GraphViz::undirected: return os << "undirected";
            };
            return os << "unknown";
        }

        template <typename T>
        static inline std::ostream& operator<<(std::ostream& os, T const& v) {
            return os << boost::fusion::as_vector(v);
        }
    }
#endif

namespace Ast { namespace Builder {

    struct Pool {
        Nodes nodes;
    };

} }

namespace Parser {

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsequence-point"
    namespace qi = boost::spirit::qi;
    namespace px = boost::phoenix;
    using namespace qi;
    using boost::spirit::repository::qi::distinct;

    template <typename It>
    struct GraphViz : grammar<It, Ast::GraphViz()> {
        GraphViz() : GraphViz::base_type(start) {
#define NOPE _val=_val
#define TODO eps[NOPE]
            auto kw = distinct(char_("a-zA-Z0-9_"));

            start   = skip(space) [kind_ >> graph_];
            kind_  %= kw["digraph"] >> attr(Kind::directed)   [ add_arrow_(px::val("->")) ]
                    | kw["graph"]   >> attr(Kind::undirected) [ add_arrow_(px::val("--")) ]
                    ;

            graph_  = id_ >> block_ >> TODO;

            string_ = '"' >> *('\\' >> char_ | ~char_('"')) >> '"';

            id_ = string_
                | +char_("a-zA-Z0-9_")
                ;

            block_ 
                = '{' >> *(graph_element >> *lit(';')) >> '}'
                ;

            graph_element 
                = graph_attrs 
                | node_attrs
                | edge_attrs
                | rankdir
                | kw["subgraph"] >> graph_
                | chain_
                | (char_ - '}') [NOPE]
                ;

            graph_attrs = kw["graph"] >> attributes_;
            node_attrs  = kw["node"]  >> attributes_;
            edge_attrs  = kw["edge"]  >> attributes_;
            rankdir     = kw["rankdir"] >> '=' >> kw[lit("LR")|"RL"|"TB"|"BT"];

            nodeid_     = id_;
            noderef_    = id_ >> -(':' >> +id_);
            attributes_ = '[' >> (*~char_(']')) [NOPE] >> ']';

            chain_ 
                = nodeid_ >> !(':'|arrow_) >> -attributes_
                | (block_ | noderef_) >> -(arrow_ >> chain_) [NOPE]
                ;

            BOOST_SPIRIT_DEBUG_NODES(
                    (start)(kind_)(noderef_)(attributes_)(graph_)
                    (id_)(string_)
                    (block_)(chain_) // it must be great!
                    (graph_element)(graph_attrs)(node_attrs)(edge_attrs)(rankdir)
                )
        }

      private:

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

        using Kind = Ast::GraphViz::Kind;

        rule<It, Ast::SubGraph(),   Skipper> graph_;
        rule<It, Ast::Attributes(), Skipper> attributes_;

        // implicit lexemes
        rule<It, Ast::NodeRef()> noderef_;
        rule<It, Ast::NodeId()> nodeid_;
        rule<It, Kind()> kind_;
        rule<It, Ast::Id()> id_;
        rule<It, std::string()> string_;

        // TODO
        rule<It, Skipper> graph_element, graph_attrs, node_attrs, edge_attrs, rankdir;
        using NodeSet = std::vector<Ast::NodeRef>;
        rule<It, NodeSet(), Skipper> block_;
        rule<It, NodeSet(), Skipper> chain_;

    };

#pragma GCC diagnostic pop 
}

#include <fstream>

int main() {
    using It = boost::spirit::istream_iterator;
    Parser::GraphViz<It> gv;

    std::ifstream ifs("g.dot");
    It f{ifs >> std::noskipws}, l;

    Ast::GraphViz g;
    bool ok = parse(f, l, gv, g);

    if (ok) {
        std::cout << "Parse success\n";
    } else {
        std::cout << "Parse failed\n";
    }

    if (f != l)
        std::cout << "Remaining unparsed input: '" << std::string(f,l) << "'\n";
}
