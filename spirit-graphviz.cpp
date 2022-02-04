//#define BOOST_SPIRIT_DEBUG
#include "spirit-graphviz.hpp"
#include <fstream>
#include <iostream>
#include <deque>

int main(int argc, char** argv) {
    using It = boost::spirit::istream_iterator;
    Parser::GraphViz<It> parser;

    std::deque<std::string_view> args(argv + 1, argv + argc);
    if (args.front() == "-v") {
        debug.rdbuf(std::cerr.rdbuf());
        args.pop_front();
    }

    if (args.empty()) args.emplace_back("g.dot");
    assert(args.size() == 1);

    std::ifstream ifs(args.front().data());
    It f{ifs >> std::noskipws}, l;

    Ast::GraphViz into;
    try {
        bool ok = parse(f, l, parser, into);

        if (ok) {
            std::cerr << "Parse success\n";
            //std::cerr << into << "\n";

            Model::MainGraph g = buildModel(into);

            std::cout << "// generated from c++\n" << graphviz(g) << "\n";
        } else {
            std::cerr << "Parse failed\n";
        }

        if (f != l)
            std::cerr << "Remaining unparsed input: '" << std::string(f,l) << "'\n";
    } catch (Parser::qi::expectation_failure<It> const& e) {
        std::cerr << e.what() << ": " << e.what_ << " at " << std::string(e.first, e.last) << "\n";
    }
}
