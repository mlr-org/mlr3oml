#include <R.h>
#include <Rdefines.h>
#include <string>

static int remove_comment(std::string line) {
    enum states { DULL, IN_QUOTE, ESCAPING } state = DULL;

    for (size_t i = 0; i < line.size(); i++) {
        switch(state) {
            case DULL:
                switch(line[i]) {
                    case '\'':
                        state = IN_QUOTE;
                        break;
                    case '%':
                        return i;
                }
                break;

            case IN_QUOTE:
                switch(line[i]) {
                    case '\'':
                        state = DULL;
                        break;
                    case '\\':
                        state = ESCAPING;
                        break;
                }
                break;

            case ESCAPING:
                state = IN_QUOTE;
                break;
        }
    }

    return -1;
}


extern "C" {

SEXP c_remove_comment(SEXP lines_) {
    const R_len_t n = length(lines_);
    SEXP result_ = PROTECT(allocVector(STRSXP, n));

    for (R_len_t i = 0; i < n; i++) {
        std::string line = Rf_translateCharUTF8(STRING_ELT(lines_, i));
        int pos = remove_comment(line);
        if (pos < 0) {
            SET_STRING_ELT(result_, i, STRING_ELT(lines_, i));
        } else {
            SET_STRING_ELT(result_, i, Rf_mkCharCE(line.substr(0, pos).c_str(), CE_UTF8));
        }
    }

    UNPROTECT(1);
    return result_;
}

}
