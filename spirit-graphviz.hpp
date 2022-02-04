#pragma once
#include <boost/fusion/adapted.hpp>
#include <boost/multi_index/mem_fun.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/sequenced_index.hpp>
#include <boost/multi_index_container.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/repository/include/qi_distinct.hpp>
#include <map>
#include <set>

static std::ostream debug{nullptr};

namespace Model {
    ////////////////////////
    // Shared primitives
    using Id         = std::string;
    using OptionalId = boost::optional<Id>;

    using Attributes = std::map<Id, std::string>;

    enum class CompassPoint { n, ne, e, se, s, sw, w, nw, c, _ };
    enum class GraphKind    { directed, undirected             };

    struct NodeRef {
        Id id;
        Id mutable port; // non-key field
        CompassPoint mutable compass_pt = CompassPoint::_; // non-key field

        bool operator<(NodeRef const& o) const {
            return std::tie(id, port, compass_pt) < std::tie(o.id, o.port, o.compass_pt);
        }
    };

    ////////////////////////
    // Domain Representation
    static std::string get(Attributes const& attr, std::string const& key, std::string const& default_value = {}) {
        auto it = attr.find(key);
        return it == attr.end()? default_value : it->second;
    }

    struct Node {
        Id id;
        Attributes mutable attributes; // non-key field mutable

        Node(Id id) : id(id) {}

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
        SubGraphs subgraphs;

        std::string label() const { return get(attributes, "label", id); }
    };

    struct OwnedNode {
        Node node;
        SubGraph const* owner;

        Id const& id() const { return node.id; }
    };

    namespace bmi = boost::multi_index;

    using Nodes = boost::multi_index_container<
        OwnedNode,
        bmi::indexed_by<
            bmi::ordered_unique<
                bmi::tag<struct by_node_id>,
                bmi::const_mem_fun<OwnedNode, Id const&, &OwnedNode::id>
            >,
            bmi::sequenced<bmi::tag<struct by_insertion>>
        > > ; // std::map<Id, OwnedNode>;
    using Edges = std::multiset<Edge, Edge::IdsOnlyCmp>; // Note: builder assumes reference stability

    struct MainGraph {
        bool      strict;
        GraphKind kind;
        Nodes     all_nodes;
        Edges     all_edges;
        SubGraph  graph;

        Edge const& ensure_edge(NodeRef from, NodeRef to, SubGraph const& owner);
        Node const& ensure_node(Id id, SubGraph const& owner);
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

BOOST_FUSION_ADAPT_STRUCT(Model::NodeRef, id,      port,       compass_pt)
BOOST_FUSION_ADAPT_STRUCT(Ast::AttrStmt,  group,   attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::Attr,      key,     value)
BOOST_FUSION_ADAPT_STRUCT(Ast::NodeStmt,  node_id, attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::EdgeStmt,  hops,    attributes)
BOOST_FUSION_ADAPT_STRUCT(Ast::Graph,     id,      stmt_list)
BOOST_FUSION_ADAPT_STRUCT(Ast::GraphViz,  strict,  kind,       graph)

#ifndef NDEBUG
#include <iomanip>
#include <boost/optional/optional_io.hpp>
    namespace std {
        template <typename... Arg>
        std::ostream& operator<<(std::ostream& os, std::map<Arg...> const& v) { 
            os << '[';
            for (auto& p : v)
                os << std::quoted(p.first) << '=' << std::quoted(p.second) << "; ";
            return os << ']';
        }

        template <typename... Arg>
        std::ostream& operator<<(std::ostream& os, std::vector<Arg...> const& v) { 
            os << '{';
            for (auto& el : v) os << el << ";\n";
            return os << '}';
        }
    }
    namespace Model {
        std::ostream& operator<<(std::ostream& os, GraphKind k) {
            switch(k) {
                case GraphKind::directed: return os << "directed";
                case GraphKind::undirected: return os << "undirected";
            };
            return os << "unknown";
        }

        std::ostream& operator<<(std::ostream& os, CompassPoint cp) {
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

        std::ostream& operator<<(std::ostream& os, NodeRef   const& v) { return os << boost::fusion::as_vector(v); }
    }
    namespace Ast {

        std::ostream& operator<<(std::ostream& os, AttrStmt::Group g) {
            switch(g) {
                case AttrStmt::Group::graph: return os << "graph";
                case AttrStmt::Group::node:  return os << "node";
                case AttrStmt::Group::edge:  return os << "edge";
            };
            return os << "unknown";
        }

        std::ostream& operator<<(std::ostream& os, Model::NodeRef const& v) { return os << boost::fusion::as_vector(v); }
        std::ostream& operator<<(std::ostream& os, Ast::AttrStmt  const& v) { return os << boost::fusion::as_vector(v); }
        std::ostream& operator<<(std::ostream& os, Ast::Attr      const& v) { return os << boost::fusion::as_vector(v); }
        std::ostream& operator<<(std::ostream& os, Ast::NodeStmt  const& v) { return os << boost::fusion::as_vector(v); }
        std::ostream& operator<<(std::ostream& os, Ast::EdgeStmt  const& v) { return os << boost::fusion::as_vector(v); }
        std::ostream& operator<<(std::ostream& os, Ast::Graph     const& v) { return os << boost::fusion::as_vector(v); }
        std::ostream& operator<<(std::ostream& os, Ast::GraphViz  const& v) { return os << boost::fusion::as_vector(v); }
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

            opt_port    = copy(':' >> ID_ | attr(Ast::Id{}));
            opt_compass = copy(':' >> kw[compass_pt] | attr(Ast::CompassPoint::_));
            no_port_id  = ID_ >> attr(Ast::Id{}) >> opt_compass >> !lit(':');
            full_id     = ID_ >> opt_port >> opt_compass;
            node_id     = !kw[attr_group | "subgraph"] >> // prohibited node ids
                        (no_port_id | full_id);

            BOOST_SPIRIT_DEBUG_NODES(
                (graph_)(subgraph_)                                           //
                (a_list)(attr_list)                                           //
                (stmt)(attr_stmt)(attribute)(node_stmt)(edge_stmt)(stmt_list) //
                (node_id)(opt_port)(opt_compass)                              //
                (no_port_id)(full_id)                                         //
                (start)(kind_)                                                //
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
        qi::rule<It, Ast::Graph(),        Skipper> graph_, subgraph_;
        qi::rule<It, Ast::AList(),        Skipper> a_list;
        qi::rule<It, Ast::AttrList(),     Skipper> attr_list;
        qi::rule<It, Ast::NodeRef(),      Skipper> node_id, no_port_id, full_id;
        qi::rule<It, Ast::Id(),           Skipper> opt_port;
        qi::rule<It, Ast::CompassPoint(), Skipper> opt_compass;

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

        // Separate grammar for ID parsing (slightly optimizing isBareId)
        friend bool isBareId(std::string const&);

        struct IdParser : qi::grammar<It, Ast::Id()> {
            IdParser() : IdParser::base_type(ID) {
                using namespace qi;
                quoted   = '"' >> *('\\' >> char_ | ~char_('"')) >> '"';
                HTML     = eps(false); // TODO implement?
                bareId   = char_("a-zA-Z_\200-\377") >> *char_("a-zA-Z0-9_\200-\377")
                         | raw [ -lit('-') >> ('.' >> +digit | +digit >> -('.' >> *digit)) ]
                         ;
                ID       = quoted 
                         | bareId
                         | HTML
                         ;

                BOOST_SPIRIT_DEBUG_NODES((ID)(HTML)(quoted)(bareId))
            }

            qi::rule<It, Ast::Id()> ID, HTML, quoted, bareId;
        };

        IdParser ID_;
    };

    bool isBareId(std::string const& id) {
        static GraphViz<std::string::const_iterator>::IdParser const s_id_parser;
        return parse(id.begin(), id.end(), s_id_parser.bareId >> qi::eoi);
    }

}

#include <iomanip>

namespace Model {
    namespace IO {

        struct graphviz_printer {
            MainGraph const& _main;
            std::string const _indent;

            friend std::ostream& operator<<(std::ostream& os, graphviz_printer const& manip) {
                manip.print(os);
                return os;
            }

          private:
            struct ID_Wrap {
                std::string const& _s;

                friend std::ostream& operator<<(std::ostream& os, ID_Wrap const& manip) {
                    if (Parser::isBareId(manip._s))
                        return os << manip._s;
                    else
                        return os << std::quoted(manip._s);
                }
            };

            static ID_Wrap ID(std::string const& s) { return {s}; }

            void print(std::ostream& os) const {
                if (_main.strict) os << "strict ";
                switch (_main.kind) {
                    case GraphKind::undirected: os << "graph "; break;
                    case GraphKind::directed: os << "digraph "; break;
                }
                print(os, _main.graph);
            }

            void print(std::ostream& os, SubGraph const& g) const {
                if (g.id.size())
                    os << ID(g.id) << " ";
                os << "{";

                graphviz_printer{_main, _indent+"    "}.print_contents(os, g);

                os << "\n" << _indent << "}";
            }

            void print_contents(std::ostream& os, SubGraph const& g) const {
                if (!g.attributes.empty()) {
                    os << "\n" << _indent << "graph";
                    print(os, g.attributes);
                    os << ";";
                }
                    
                graphviz_printer nested{_main, _indent + "  "};
                for (auto& owned_node : _main.all_nodes.get<by_insertion>()) {
                    if (owned_node.owner == &g) {
                        os << "\n" << _indent;
                        nested.print(os, owned_node.node);
                    }
                }

                for (auto& sub : g.subgraphs) {
                    os << "\n" << _indent << "subgraph ";
                    print(os, sub.second);
                }

                if (&g == &_main.graph) {
                    for (auto& edge : _main.all_edges) {
                        print(os, edge);
                    }
                }
            }

            void print(std::ostream &os, Edge const &edge) const {
                os << "\n" << _indent;
                print(os, edge.from);
                os << (_main.kind==GraphKind::directed?" -> ":" -- ");
                print(os, edge.to);
                print(os, edge.attributes);
                os << ";";
            }

            void print(std::ostream &os, NodeRef const &nref) const {
                os << nref.id;
                if (!nref.port.empty()) os << ":" << nref.port;
                if (nref.compass_pt!=CompassPoint::_) os << ":" << nref.compass_pt;
            }

            void print(std::ostream &os, Attributes const &attributes) const {
                if (attributes.empty())
                    return;
                os << '[';
                graphviz_printer nested{ _main, _indent + "         " };
                bool first = true;
                for (auto &attr : attributes) {
                    if (first) {
                        first = false;
                    } else {
                        //os << "\n" << _indent;
                    }
                    os << ID(attr.first) << '=' << ID(attr.second);
                    if (&attr != &*attributes.rbegin())
                        os << ",";
                }
                os << ']';
            }

            void print(std::ostream& os, Node const& n) const {
                os << ID(n.id);
                print(os, n.attributes);
                os << ";";
            }
        };

    }

    IO::graphviz_printer graphviz(MainGraph const& mg) {
        return { mg, "" };
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

    Node const& MainGraph::ensure_node(Id id, SubGraph const& owner) {
        OwnedNode addendum {Node {id}, &owner}; 

        auto [it, inserted] = all_nodes.insert(addendum);

        if (inserted) {
            debug << "Creating node " << id << " in graph " << owner.id << "\n";
        } else {
            if (&owner != it->owner)
                debug << "Reassigning node " << id
                      << (it->owner ? " from graph '" + it->owner->id + "'"
                                    : "")
                      << " to graph '" << owner.id << "'\n";
        }

        all_nodes.modify(it, [&owner](OwnedNode& on) { /*if (not on.owner)*/ on.owner = &owner; });
        return it->node;
    }

    Edge const& MainGraph::ensure_edge(NodeRef from, NodeRef to, SubGraph const& owner) {
        debug << "Ensure edge " << from << " .. " << to << "\n";

        ensure_node(from.id, owner);
        ensure_node(to.id, owner);
        
        Edge addendum { from, to, {} };

        if (strict) {
            bool inverted = false;
            auto range = all_edges.equal_range(addendum);

            if (range.first == range.second && kind != GraphKind::directed) {
                inverted = true;
                std::swap(addendum.from, addendum.to);
                range = all_edges.equal_range(addendum);
                std::swap(addendum.from, addendum.to); // restore in case no match
            }

            if (range.first != range.second) { // already present

                // get the known attrs, and remove
                addendum = *range.first;

                debug << "DEBUG: " << kind << " edge duplicates " << addendum.from << " - " << addendum.to << " " << (inverted?"(inv)":"") << "\n";

                range.first = all_edges.erase(range.first);

                // assert there wasn't a dup already
                assert(range.first == range.second);

                // behaviour of Graphviz is to take port/compass from the last spelling encountered
                if (inverted) {
                    addendum.from = to;
                    addendum.to = from;
                } else {
                    addendum.from = from;
                    addendum.to = to;
                }
            }
        }

        debug << " - inserted edge " << addendum.from << " .. " << addendum.to << "\n";
        return *all_edges.insert(std::move(addendum));
    }

}

namespace Ast {
    namespace detail {
        using NodeRefSet = std::set<NodeRef>;

        static void apply_attributes(Ast::AList const& a_list, Model::Attributes& target) {
            for (auto& a : a_list)
                target[a.first] = a.second;
        }

        static void apply_attributes(Ast::AttrList const& attr_list, Model::Attributes& target) {
            for (auto& a_list : attr_list)
                apply_attributes(a_list, target);
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
                Leaver(Stack& stack) : _ptr(&stack) { }
                Leaver(Leaver const&) = delete;
                Leaver(Leaver&& other) : _ptr(other._ptr) { other.disarm(); }
                Leaver& operator=(Leaver&& other) {
                    _ptr   = other._ptr;
                    _armed = other._armed;
                    other._armed = false;
                    return *this;
                }

                ~Leaver() {
                    if (_armed) {
                        assert(_ptr && _ptr->size());
                        debug << "*** Leaving subgraph '" << _ptr->back().graph.id << "'\n";
                        _ptr->pop_back();
                    }
                }

                void disarm() { _armed = false; }
              private:
                Stack* _ptr;
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
                debug << "*** Entering subgraph '" << subgraph.id << "'\n";
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
                NodeRefSet reffed;
                boost::optional<Leaver> hold;

                if (root_graph) { // maingraph root already exists; just set properties
                    if (src.id) { cur_graph().id = *src.id; }
                } else {
                    hold = enter(cur_graph().ensure_subgraph(src.id));
                }

                reffed = transform(src.stmt_list);
                apply_attributes(ToS().graph_attrs, cur_graph().attributes);

                return reffed;
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
                        auto rhs_refs = apply_visitor(*this, *rhs);

                        for (auto from : lhs_refs)
                            for (auto to : rhs_refs)
                                path.push_back(cur_main().ensure_edge(from, to, cur_graph()));

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
                    apply_attributes(ToS().edge_attrs, e.attributes); // ambient
                    apply_attributes(src.attributes, e.attributes); // declared
                }

                // Subgraphs return "naked" node ids, not full refs
                // so we clear out any ports with respect to any edge endpoints
                for (auto& ref : all_referenced) {
                    ref.port = "";
                    ref.compass_pt = CompassPoint::_;
                }
                return all_referenced;
            }

            NodeRefSet transform(NodeRef const& src) {
                auto& node = cur_main().ensure_node(src.id, cur_graph());
                apply_attributes(ToS().node_attrs, node.attributes); // ambient

                // single node refs return the full ref for optional use as
                // edge endpoint, even if it implicitly declared a node.
                return { src };
            }

            NodeRefSet transform(NodeStmt const& src) {
                auto& node = cur_main().ensure_node(src.node_id.id, cur_graph());

                apply_attributes(ToS().node_attrs, node.attributes); // ambient
                apply_attributes(src.attributes, node.attributes); // declared

                // single node refs return the full ref for optional use as
                // edge endpoint, even if it implicitly declared a node.
                return { src.node_id };
            }

            //template <typename T>
            //NodeRefSet transform(T&& src) const {
                //debug << "TODO IMPLEMENT " << __PRETTY_FUNCTION__ << "\n";
                //debug << "src: " << src << "\n";
                //return {};
            //}
        };
    }

    Model::MainGraph buildModel(Ast::GraphViz const& g) {
        detail::ModelBuilder builder;
        return builder(g);
    }
}
