#include <Rcpp.h>
#include <string>
#include <vector>


// [[Rcpp::export]]
Rcpp::StringVector parse_arff_levels(std::string & buffer) {
    std::string::const_iterator c, first_char;
    const std::string::const_iterator end = buffer.end();
    std::vector<std::string> result;
    enum states { DULL, BEFORE_WORD, IN_WORD, AFTER_WORD, IN_QUOTE, ESCAPING } state = DULL;
    bool stop = false;

    for (c = buffer.begin(); !stop & c != end; ++c) {
        switch(state) {

            case DULL:
                switch(*c) {
                    case '{': 
                        state = BEFORE_WORD;
                        break;
                    default:
                        if (!std::isspace(*c)) {
                            Rcpp::stop("Set of categorical attributes must start with '{'.");
                        }
                }
                break;


            case BEFORE_WORD:
                switch(*c) {
                    case ',':
                        result.push_back("");
                        break;
                    case '\'':
                        state = IN_QUOTE;
                        first_char = c + 1;
                        break;
                    default:
                        if (!std::isspace(*c)) {
                            state = IN_WORD;
                            first_char = c;
                        }
                }
                break;


            case AFTER_WORD:
                switch(*c) {
                    case ',':
                        state = BEFORE_WORD;
                        break;
                    case '}':
                        stop = true;
                        break;
                    default: 
                        if (!std::isspace(*c)) {
                            Rcpp::Rcout << *c << std::endl;
                            Rcpp::stop("Malformated set of categorical attributes, expected ',' or '}' after category.");
                        }
                }
                break;


            case IN_WORD:
                switch(*c) {
                    case '}': 
                        stop = true;
                    case ',':
                        result.push_back(std::string(first_char, c));
                        state = BEFORE_WORD;
                        break;
                    default:
                        if (std::isspace(*c)) {
                            result.push_back(std::string(first_char, c));
                            state = AFTER_WORD;
                        }
                }
                break;


            case IN_QUOTE:
                switch(*c) {
                    case '\\':
                        state = ESCAPING;
                        break;
                    case '\'':
                        result.push_back(std::string(first_char, c));
                        state = AFTER_WORD;
                        break;
                }
                break;


            case ESCAPING:
                state = IN_QUOTE;
                break;
        }
    }

    if (!stop) {
        Rcpp::stop("Incomplete set of categorical attributes detected.");
    }

    return Rcpp::wrap(result);
}
