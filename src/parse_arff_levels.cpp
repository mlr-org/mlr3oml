#define R_NO_REMAP
#include <string>
#include <vector>
#include <R.h>
#include <Rdefines.h>

extern "C" {

SEXP c_parse_arff_levels(SEXP buffer_) {
    std::string buffer(Rf_translateCharUTF8(STRING_ELT(buffer_, 0)));
    std::string::const_iterator c, first_char, last_char;
    const std::string::const_iterator end = buffer.end();
    std::vector<std::string> levels;
    enum states { DULL, BEFORE_WORD, IN_WORD, AFTER_WORD, IN_QUOTE, ESCAPING } state = DULL;
    bool stop = false;

    for (c = buffer.begin(); !stop && c != end; ++c) {
        switch(state) {

            case DULL:
                switch(*c) {
                    case '{':
                        state = BEFORE_WORD;
                        break;
                    default:
                        if (!std::isspace(*c)) {
                            Rf_error("Set of categorical attributes must start with '{'.");
                        }
                }
                break;


            case BEFORE_WORD:
                switch(*c) {
                    case ',':
                        levels.push_back("");
                        break;
                    case '\'':
                        state = IN_QUOTE;
                        first_char = c + 1;
                        break;
                    default:
                        if (!std::isspace(*c)) {
                            state = IN_WORD;
                            first_char = last_char = c;
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
                            Rf_error("Malformated set of categorical attributes, expected ',' or '}' after category.");
                        }
                }
                break;


            case IN_WORD:
                switch(*c) {
                    case '}':
                        stop = true;
                    case ',':
                        levels.push_back(std::string(first_char, last_char + 1));
                        state = BEFORE_WORD;
                        break;
                    default:
                        if (std::isspace(*c)) {
                            // Many ARFF files do not stick to this specification, so it is disabled here.
                            /* Rf_error("Malformated set of categorical attributes, nominal values which include spaces must be quoted."); */
                        } else {
                            last_char = c;
                        }
                }
                break;


            case IN_QUOTE:
                switch(*c) {
                    case '\\':
                        state = ESCAPING;
                        break;
                    case '\'':
                        levels.push_back(std::string(first_char, c));
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
        Rf_error("Incomplete set of categorical attributes detected.");
    }

    SEXP result = PROTECT(Rf_allocVector(STRSXP, levels.size()));
    for (size_t i = 0; i < levels.size(); i++) {
        SET_STRING_ELT(result, i, Rf_mkCharCE(levels[i].c_str(), CE_UTF8));
    }
    UNPROTECT(1);
    return result;
}

}
