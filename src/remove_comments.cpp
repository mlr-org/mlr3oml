#include <R.h>
#include <Rdefines.h>
#include <string>
#include <vector>


extern "C" {
static int remove_comment(std::string buffer) {
    enum states { DULL, IN_QUOTE, ESCAPING } state = DULL;
    int i;

    for (i = 0; i < buffer.size(); i++) {
        char c = buffer[i];

        Rprintf("c: %c,  state: %d\n", c, state);

        switch(state) {
            case DULL:
                switch(c) {
                    case '\'':
                        state = IN_QUOTE;
                        break;
                        break;
                    case '%':
                        return i;
                }
                break;

            case IN_QUOTE:
                switch(c) {
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

    return i;
}

SEXP c_remove_comment(SEXP lines_) {
    const R_len_t n = length(lines_);
    SEXP result_ = PROTECT(allocVector(STRSXP, n));


    for (R_len_t i = 0; i < n; i++) {
        std::string buffer = Rf_translateCharUTF8(STRING_ELT(lines_, i));
        int pos = remove_comment(buffer);
        if (pos == buffer.size()) {
            SET_STRING_ELT(result_, i, STRING_ELT(lines_, i));
        } else {
            SET_STRING_ELT(result_, i, Rf_mkCharCE(buffer.substr(0, pos).c_str(), CE_UTF8));

        }
    }

    UNPROTECT(1);
    return result_;
}

}
