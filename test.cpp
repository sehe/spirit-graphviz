//#define BOOST_SPIRIT_DEBUG
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/repository/include/qi_distinct.hpp>
#include <boost/fusion/adapted.hpp>
#include <map>
#include <set>

namespace Model {
    ////////////////////////
    // Shared primitives
    using Id = std::string;
    using OptionalId = boost::optional<Id>;

    using Attributes = std::map<Id, std::string>;

    enum class CompassPoint { n, ne, e, se, s, sw, w, nw, c, _ };
    enum class GraphKind { directed, undirected };

    struct NodeRef {
        Id id;
        Id port;
        CompassPoint compass_pt = CompassPoint::_;

        bool operator<(NodeRef const& o) const {
            return std::tie(id, port, compass_pt) < std::tie(o.id, o.port, o.compass_pt);
        }
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
        Attributes mutable attributes; // non-key field mutable

        bool operator<(Edge const& o) const {
            return std::tie(from, to) < std::tie(o.from, o.to);
        }

        struct IdsOnlyCmp {
            bool operator()(Edge const& a, Edge const& b) const {
                return std::tie(a.from.id, a.to.id)
                     < std::tie(b.from.id, b.to.id);
            }
        };

        std::string label() const { return get(attributes, "label"); }
    };

    using SubGraphs = std::multimap<Id, struct SubGraph>;

    struct SubGraph {
        SubGraph(Id id = {}) : id(id) {}
        SubGraph& ensure_subgraph(OptionalId const& id);

        Id id;
        Attributes attributes;
        std::set<Id> owned_nodes;
        SubGraphs subgraphs;

        std::string label() const { return get(attributes, "label", id); }
    };

    using Nodes = std::map<Id, Node>;
    using Edges = std::multiset<Edge, Edge::IdsOnlyCmp>; // Note: builder assumes reference stability

    struct MainGraph {
        bool      strict;
        GraphKind kind;
        Nodes     all_nodes;
        Edges     all_edges;
        SubGraph  graph;

        Edge const& ensure_edge(NodeRef from, NodeRef to);
    };
}

namespace Ast {
    using Model::CompassPoint;
    using Model::Id;
    using Model::NodeRef;
    using Model::GraphKind;
    using Model::OptionalId;   

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
            boost::recursive_wrapper<EdgeStmt>, // includes sub graphs
            NodeStmt
        >;

    using StmtList = std::vector<Stmt>;
    
    struct Graph {
        OptionalId id;
        StmtList stmt_list;
    };

    using Endpoint = boost::variant<NodeRef, Graph>;

    struct EdgeStmt {
        std::vector<Endpoint> hops;
        AttrList attributes;
    };

    struct GraphViz {
        bool strict;
        GraphKind kind;
        Graph graph;
    };

    Model::MainGraph buildModel(GraphViz const&);
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

            start   = skip(copy(skipper)) [matches[kw["strict"]] >> kind_ >> graph_];

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
                      | node_stmt // precede edge_stmt
                      | edge_stmt
                      ;

            attr_stmt = kw[attr_group] >> attr_list;

            attribute = ID_ >> '=' >> ID_;

            node_stmt = node_id >> -attr_list >> !arrow_;

            node_or_sub = subgraph_ | node_id;

            edge_stmt
                = node_or_sub % arrow_ >> -attr_list
                //= node_or_sub >> +(arrow_ >> node_or_sub) >> -attr_list
                ;

            a_list    = '[' >> *(attribute >> -omit[char_(",;")]) >> ']';
            attr_list = +a_list;

            node_id = 
                !kw[attr_group|"subgraph"] >>  // weed out "graph", "node", "edge", "subgraph" for efficiency (this line is optional)
                ID_ >> (
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
                 //(skipper)
            )
        }

      private:
        ////////////////////////
        using Skipper = qi::rule<It>;
        Skipper skipper 
            = +qi::space
            | ("//" >> *(qi::char_ - qi::eol))
            | ("/*" >> *(qi::char_ - "*/") >> "*/")
            ;

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
        qi::rule<It, Ast::Endpoint(),  Skipper> node_or_sub;
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

            auto x = buildModel(into);
        } else {
            std::cout << "Parse failed\n";
        }

        if (f != l)
            std::cout << "Remaining unparsed input: '" << std::string(f,l) << "'\n";
    } catch (Parser::qi::expectation_failure<It> const& e) {
        std::cout << e.what() << ": " << e.what_ << " at " << std::string(e.first, e.last) << "\n";
    }
}

namespace Model {

    SubGraph& SubGraph::ensure_subgraph(OptionalId const& id) {
        // anonymous subgraphs never alias;
        // `{ }` is anonymous
        // `subgraph { }` is anonymous
        // `subgraph "" { }` is not anonymous
        if (id) {
            auto range = subgraphs.equal_range(*id);

            if (range.first != range.second) {
                return range.first->second;
            }
        }

        Id key = id.get_value_or(Id{});
        return subgraphs.emplace(key, SubGraph(key))->second;
    }

    Edge const& MainGraph::ensure_edge(NodeRef from, NodeRef to) {
        if (kind != GraphKind::directed) {
            if (to < from)
                std::swap(from, to); // fix order for consistency
        }

        Edge addendum { from, to, {} };

        if (strict) {
            auto range = all_edges.equal_range(addendum);

            if (range.first != range.second) { // already present

                // get the known attrs, and remove
                addendum = *range.first;
                range.first = all_edges.erase(range.first);

                // assert there wasn't a dup already
                assert(range.first == range.second);

                // behaviour of Graphviz is to take port/compass from the last spelling encountered
                addendum.from = from;
                addendum.to = to;
            }
        }

        return *all_edges.insert(std::move(addendum));
    }
            
}

namespace Ast {
    namespace detail {
        using NodeRefSet = std::set<NodeRef>;

        static void apply_attributes(Ast::AttrList const& source, Model::Attributes& target) {
            for (auto& a_list : source)
                for (auto& a : a_list)
                    target[a.first] = a.second;
        }

        struct ModelBuilder {
            /////////////////////
            // state & facilities
            struct Frame {
                Model::MainGraph& main;
                Model::SubGraph& graph;

                // ambient attributes
                Model::Attributes graph_attrs, node_attrs, edge_attrs;

                Frame(Model::MainGraph& main) : main(main), graph(main.graph) {}
                Frame(Model::SubGraph& graph, Frame const& inherit)
                    : main(inherit.main), graph(graph),
                      graph_attrs(inherit.graph_attrs), node_attrs(inherit.node_attrs), edge_attrs(inherit.edge_attrs)
                { }
            };

            using Stack = std::list<Frame>;
            Stack _stack;

            struct Leaver { // exception safe stack frame cleanup
                Leaver(Stack& stack) : _ref(stack) { }
                Leaver(Leaver const&) = delete;
                Leaver(Leaver&& other) : _ref(other._ref) { other.disarm(); }

                ~Leaver() {
                    if (_armed) {
                        assert(_ref.size());
                        _ref.pop_back();
                    }
                }

                void disarm() { _armed = false; }
              private:
                Stack& _ref;
                bool _armed = true;
            };

            Frame& ToS()                  { assert(_stack.size()); return _stack.back(); }
            Model::MainGraph& cur_main()  { return ToS().main; }
            Model::SubGraph&  cur_graph() { return ToS().graph; }

            Leaver enter(Model::MainGraph& main) {
                _stack.emplace_back(main);
                return Leaver(_stack);
            }

            Leaver enter(Model::SubGraph& subgraph) {
                _stack.emplace_back(subgraph, ToS());
                return Leaver(_stack);
            };

            // allow use as visitor
            template <typename T>
                decltype(auto) operator()(T const& src) { return transform(src); }

            ///////////////////////////////////////////////////////
            // logic
            Model::MainGraph transform(Ast::GraphViz const& src) {
                Model::MainGraph target;

                target.strict = src.strict;
                target.kind   = src.kind;

                auto hold = enter(target);
                transform(src.graph, true);

                return target;
            }

            NodeRefSet transform(Ast::Graph const& src, bool root_graph = false) {
                if (root_graph) {
                    if (src.id) {
                        cur_graph().id = *src.id;
                    }

                    return transform(src.stmt_list);
                } else {
                    auto hold = enter(cur_graph().ensure_subgraph(src.id));

                    return transform(src.stmt_list);
                }
            };

            NodeRefSet transform(Ast::StmtList const& src) {
                NodeRefSet all_referenced;
                for (auto& stmt : src) {
                    auto&& referenced = apply_visitor(*this, stmt);
                    all_referenced.insert(referenced.begin(), referenced.end());
                }

                return all_referenced;
            };

            NodeRefSet transform(Ast::AttrStmt const& src) {
                switch (src.group) {
                    case Ast::AttrStmt::Group::graph: apply_attributes(src.attributes, ToS().graph_attrs); break;
                    case Ast::AttrStmt::Group::node:  apply_attributes(src.attributes, ToS().node_attrs); break;
                    case Ast::AttrStmt::Group::edge:  apply_attributes(src.attributes, ToS().edge_attrs); break;
                    default: throw std::domain_error("AttrStmt::group");
                }
                return {};
            }

            NodeRefSet transform(Ast::Attr const& src) {
                ToS().graph_attrs[src.key] = src.value;
                return {};
            }

            NodeRefSet transform(Ast::EdgeStmt const& src) {
                NodeRefSet all_referenced;
                std::vector<std::reference_wrapper<Model::Edge const>> path;

                assert(src.hops.size());

                if (src.hops.size() > 1) {
                    for (auto lhs = src.hops.begin(), rhs = std::next(lhs); rhs != src.hops.end(); ++lhs, ++rhs) {
                        auto lhs_refs = apply_visitor(*this, *lhs);
                        auto rhs_refs = apply_visitor(*this, *lhs);

                        for (auto from : lhs_refs)
                            for (auto to : rhs_refs)
                                path.push_back(cur_main().ensure_edge(from, to));

                        all_referenced.insert(lhs_refs.begin(), lhs_refs.end());
                        all_referenced.insert(rhs_refs.begin(), rhs_refs.end());
                    }
                } 
                else if (src.hops.size() == 1) {
                    auto refs = apply_visitor(*this, src.hops.front());
                    all_referenced.insert(refs.begin(), refs.end());
                }

                // apply attributes to all edges in this statement
                for (Model::Edge const& e : path) {
                    apply_attributes(src.attributes, e.attributes);
                }

                return all_referenced;
            }

            NodeRefSet transform(NodeRef const& src) {
                std::cout << "TODO IMPLEMENT " << __PRETTY_FUNCTION__ << "\n";
                return { src };
            }

            //void transform(NodeStmt const& src) {
            //}

            template <typename T>
            NodeRefSet transform(T&&) const {
                std::cout << "TODO IMPLEMENT " << __PRETTY_FUNCTION__ << "\n";
                return {};
            }
        };
    }

    Model::MainGraph buildModel(Ast::GraphViz const& g) {
        detail::ModelBuilder builder;
        return builder(g);
    }
}
