/* Kilo -- A very simple editor in less than 1-kilo lines of code (as counted
 *         by "cloc"). Does not depend on libcurses, directly emits VT100
 *         escapes on the terminal.
 *
 * -----------------------------------------------------------------------
 *
 * Copyright (C) 2016 Salvatore Sanfilippo <antirez at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *  *  Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  *  Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#define KILO_VERSION "0.0.1"

#ifdef __linux__
#define _POSIX_C_SOURCE 200809L
#endif

#include <termios.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>
#include <signal.h>
#include <utility>
#include <stdexcept>
#include <string_view>
#include <vector>
#include <array>
#include <memory>
#include <sstream>

/* Syntax highlight types */
#define HL_NORMAL 0
#define HL_NONPRINT 1
#define HL_COMMENT 2   /* Single line comment. */
#define HL_MLCOMMENT 3 /* Multi-line comment. */
#define HL_KEYWORD1 4
#define HL_KEYWORD2 5
#define HL_STRING 6
#define HL_NUMBER 7
#define HL_MATCH 8 /* Search match. */

#define HL_HIGHLIGHT_STRINGS (1 << 0)
#define HL_HIGHLIGHT_NUMBERS (1 << 1)

namespace kilopp
{

    class file_descriptor
    {
    public:
        file_descriptor(int fd, bool should_close) : fd(fd),
                                                     should_close(should_close)
        {
            if (fd == -1)
            {
                throw std::runtime_error("Error opening a file");
            }
        }

        file_descriptor(const file_descriptor &) = delete;
        file_descriptor &operator=(const file_descriptor &) = delete;

        void truncate(size_t size)
        {
            if (ftruncate(fd, size) == -1)
            {
                throw std::runtime_error("Error truncating");
            }
        }

        template <typename T>
        void write(const T &buffer)
        {
            if (::write(fd, buffer.data(), buffer.size()) == -1)
            {
                throw std::runtime_error("Write error");
            }
        }

        template <std::size_t N>
        void write(const char (&buffer)[N])
        {
            if (::write(fd, buffer, N) == -1)
            {
                throw std::runtime_error("Write error");
            }
        }

        ~file_descriptor()
        {
            if (should_close && fd != -1)
            {
                close(fd);
            }
        }

    private:
        int fd = -1;
        bool should_close;
    };

    struct syntax
    {
        std::vector<const char *> extensions;
        std::vector<const char *> keywords;
        char singleline_comment_start[3];
        char multiline_comment_start[3];
        char multiline_comment_end[3];
        int flags;
    };

    /* This structure represents a single line of the file we are editing. */
    struct erow
    {
        erow(const char *s) : chars(s)
        {
            hl = NULL;
            hl_oc = 0;
            render = NULL;
            rsize = 0;
        }

        erow(const erow &) = delete;
        erow &operator=(const erow &) = delete;
        erow(erow &&other)
        {
            *this = std::move(other);
        };

        erow &operator=(erow &&other)
        {

            std::swap(chars, other.chars);
            std::swap(hl, other.hl);
            std::swap(hl_oc, other.hl_oc);
            std::swap(render, other.render);
            std::swap(rsize, other.rsize);
            other.hl = NULL;
            other.render = NULL;
            return *this;
        };

        ~erow()
        {
            free(hl);
            free(render);
        }

        int rsize;         /* Size of the rendered row. */
        std::string chars; /* Row content. */
        char *render;      /* Row content "rendered" for screen (for TABs). */
        unsigned char *hl; /* Syntax highlight type for each character in render.*/
        int hl_oc;         /* Row had open comment at end in last syntax highlight
                           check. */
    };

    typedef struct hlcolor
    {
        int r, g, b;
    } hlcolor;

    struct config
    {
        size_t cx, cy;         /* Cursor x and y position in characters */
        size_t rowoff;         /* Offset of row displayed. */
        size_t coloff;         /* Offset of column displayed. */
        size_t screenrows;     /* Number of rows that we can show */
        size_t screencols;     /* Number of cols that we can show */
        std::vector<erow> row; /* Rows */
        bool dirty;            /* File modified but not saved. */
        char *filename;        /* Currently open filename */
        std::string status_message;
        time_t statusmsg_time;
        const struct syntax *syntax; /* Current syntax highlight, or NULL. */
    };

    static struct config E;

    enum KEY_ACTION
    {
        KEY_NULL = 0,    /* NULL */
        CTRL_C = 3,      /* Ctrl-c */
        CTRL_D = 4,      /* Ctrl-d */
        CTRL_F = 6,      /* Ctrl-f */
        CTRL_H = 8,      /* Ctrl-h */
        TAB = 9,         /* Tab */
        CTRL_L = 12,     /* Ctrl+l */
        ENTER = 13,      /* Enter */
        CTRL_Q = 17,     /* Ctrl-q */
        CTRL_S = 19,     /* Ctrl-s */
        CTRL_U = 21,     /* Ctrl-u */
        ESC = 27,        /* Escape */
        BACKSPACE = 127, /* Backspace */
        /* The following are just soft codes, not really reported by the
         * terminal directly. */
        ARROW_LEFT = 1000,
        ARROW_RIGHT,
        ARROW_UP,
        ARROW_DOWN,
        DEL_KEY,
        HOME_KEY,
        END_KEY,
        PAGE_UP,
        PAGE_DOWN
    };

    class output
    {
    public:
        template <typename T, typename... Args>
        std::string args(T first, Args... more)
        {
            output << first;
            return args(more...);
        }

        std::string args() { return std::move(output.str()); }

    private:
        std::stringstream output;
    };

    template <typename... Args>
    constexpr std::string format(Args... args)
    {
        return output().args(args...);
    }

    template <typename... Args>
    void set_status(Args... args)
    {
        output o;
        E.status_message = std::move(o.args(args...));
        E.statusmsg_time = time(NULL);
    }

    constexpr const char *WELCOME = "Kilo editor -- verison " KILO_VERSION "\x1b[0K\r\n";

    /* =========================== Syntax highlights DB =========================
 *
 * In order to add a new syntax, define two arrays with a list of file name
 * matches and keywords. The file name matches are used in order to match
 * a given syntax with a given file name: if a match pattern starts with a
 * dot, it is matched as the last past of the filename, for example ".c".
 * Otherwise the pattern is just searched inside the filenme, like "Makefile").
 *
 * The list of keywords to highlight is just a list of words, however if they
 * a trailing '|' character is added at the end, they are highlighted in
 * a different color, so that you can have two different sets of keywords.
 *
 * Finally add a stanza in the HLDB global variable with two two arrays
 * of strings, and a set of flags in order to enable highlighting of
 * comments and numbers.
 *
 * The characters for single and multi line comments must be exactly two
 * and must be provided as well (see the C language example).
 *
 * There is no support to highlight patterns currently. */

    /* C / C++ */

    /* Here we define an array of syntax highlights by extensions, keywords,
 * comments delimiters and flags. */
    std::array<struct syntax, 1>
        HLDB = {
            {/* C / C++ */
             std::vector<const char *>{".c", ".h", ".cpp", ".hpp", ".cc"},
             std::vector<const char *>{
                 /* C Keywords */
                 "auto", "break", "case", "continue", "default", "do", "else", "enum",
                 "extern", "for", "goto", "if", "register", "return", "sizeof", "static",
                 "struct", "switch", "typedef", "union", "volatile", "while", "NULL",

                 /* C++ Keywords */
                 "alignas", "alignof", "and", "and_eq", "asm", "bitand", "bitor", "class",
                 "compl", "constexpr", "const_cast", "deltype", "delete", "dynamic_cast",
                 "explicit", "export", "false", "friend", "inline", "mutable", "namespace",
                 "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
                 "private", "protected", "public", "reinterpret_cast", "static_assert",
                 "static_cast", "template", "this", "thread_local", "throw", "true", "try",
                 "typeid", "typename", "virtual", "xor", "xor_eq",

                 /* C types */
                 "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|",
                 "void|", "short|", "auto|", "const|", "bool|"},
             "//",
             "/*",
             "*/",
             HL_HIGHLIGHT_STRINGS | HL_HIGHLIGHT_NUMBERS}};

    /* ======================= Low level terminal handling ====================== */

    class raw_mode
    {
    public:
        raw_mode(const raw_mode &) = delete;
        raw_mode &operator=(const raw_mode &) = delete;

        raw_mode()
        {
            struct termios raw;

            if (!isatty(STDIN_FILENO))
            {
                throw std::runtime_error("stdin isn't a TTY");
            }
            if (tcgetattr(STDIN_FILENO, &previous_state) == -1)
            {
                throw std::runtime_error("Unable to get the current terminal state");
            }

            raw = previous_state; /* modify the original mode */
            /* input modes: no break, no CR to NL, no parity check, no strip char,
     * no start/stop output control. */
            raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
            /* output modes - disable post processing */
            raw.c_oflag &= ~(OPOST);
            /* control modes - set 8 bit chars */
            raw.c_cflag |= (CS8);
            /* local modes - choing off, canonical off, no extended functions,
     * no signal chars (^Z,^C) */
            raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
            /* control chars - set return condition: min number of bytes and timer. */
            raw.c_cc[VMIN] = 0;  /* Return each byte, or zero for timeout. */
            raw.c_cc[VTIME] = 1; /* 100 ms timeout (unit is tens of second). */

            /* put terminal in raw mode after flushing */
            if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) < 0)
            {
                throw std::runtime_error("Unable to set the terminal to raw mode");
            }
        }

        ~raw_mode()
        {
            file_descriptor(STDIN_FILENO, false).write("\x1b[0;0H\033[2J");
            tcsetattr(STDIN_FILENO, TCSAFLUSH, &previous_state);
        }

    private:
        struct termios previous_state;
    };

    /* Read a key from the terminal put in raw mode, trying to handle
 * escape sequences. */
    int read_key(int fd)
    {
        int nread;
        char c, seq[3];
        while ((nread = read(fd, &c, 1)) == 0)
            ;
        if (nread == -1)
            exit(1);

        while (1)
        {
            switch (c)
            {
            case ESC: /* escape sequence */
                /* If this is just an ESC, we'll timeout here. */
                if (read(fd, seq, 1) == 0)
                    return ESC;
                if (read(fd, seq + 1, 1) == 0)
                    return ESC;

                /* ESC [ sequences. */
                if (seq[0] == '[')
                {
                    if (seq[1] >= '0' && seq[1] <= '9')
                    {
                        /* Extended escape, read additional byte. */
                        if (read(fd, seq + 2, 1) == 0)
                            return ESC;
                        if (seq[2] == '~')
                        {
                            switch (seq[1])
                            {
                            case '3':
                                return DEL_KEY;
                            case '5':
                                return PAGE_UP;
                            case '6':
                                return PAGE_DOWN;
                            }
                        }
                    }
                    else
                    {
                        switch (seq[1])
                        {
                        case 'A':
                            return ARROW_UP;
                        case 'B':
                            return ARROW_DOWN;
                        case 'C':
                            return ARROW_RIGHT;
                        case 'D':
                            return ARROW_LEFT;
                        case 'H':
                            return HOME_KEY;
                        case 'F':
                            return END_KEY;
                        }
                    }
                }

                /* ESC O sequences. */
                else if (seq[0] == 'O')
                {
                    switch (seq[1])
                    {
                    case 'H':
                        return HOME_KEY;
                    case 'F':
                        return END_KEY;
                    }
                }
                break;
            default:
                return c;
            }
        }
    }

    /* Use the ESC [6n escape sequence to query the horizontal cursor position
 * and return it. On error -1 is returned, on success the position of the
 * cursor is stored at *rows and *cols and 0 is returned. */
    std::pair<int, int>
    get_cursor_position(int ifd, int ofd)
    {
        auto result = std::pair<int, int>();
        char buf[32];
        unsigned int i = 0;

        /* Report cursor location */
        file_descriptor(ofd, false).write("\x1b[6n");

        /* Read the response: ESC [ rows ; cols R */
        while (i < sizeof(buf) - 1)
        {
            if (read(ifd, buf + i, 1) != 1)
                break;
            if (buf[i] == 'R')
                break;
            i++;
        }
        buf[i] = '\0';

        /* Parse it. */
        if (buf[0] != ESC || buf[1] != '[')
        {
            throw std::runtime_error{"ioctl failed"};
        }

        if (sscanf(buf + 2, "%d;%d", &result.first, &result.second) != 2)
        {
            throw std::runtime_error{"ioctl failed"};
        }

        return result;
    }

    /* Try to get the number of columns in the current terminal. If the ioctl()
 * call fails the function will try to query the terminal itself.
 * Returns 0 on success, -1 on error. */
    void get_window_size(int ifd, int ofd, size_t &rows, size_t &cols)
    {
        struct winsize ws;

        if (ioctl(1, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
        {
            /* ioctl() failed. Try to query the terminal itself. */
            auto orig_position = get_cursor_position(ifd, ofd);
            file_descriptor fd(ofd, false);

            /* Go to right/bottom margin and get position. */
            fd.write("\x1b[999C\x1b[999B");

            auto position = get_cursor_position(ifd, ofd);
            rows = position.first;
            cols = position.second;

            /* Restore position. */
            fd.write(format("\x1b[", orig_position.first, ";", orig_position.second, "H"));
        }
        else
        {
            cols = ws.ws_col;
            rows = ws.ws_row;
        }
    }

    /* ====================== Syntax highlight color scheme  ==================== */

    constexpr bool is_separator(int c)
    {
        return c == '\0' || isspace(c) || strchr(",.()+-/*=~%[];", c) != NULL;
    }

    /* Return true if the specified row last char is part of a multi line comment
 * that starts at this row or at one before, and does not end at the end
 * of the row but spawns to the next row. */
    constexpr bool has_open_comment(const erow &row)
    {
        return (row.hl && row.rsize && row.hl[row.rsize - 1] == HL_MLCOMMENT &&
                (row.rsize < 2 || (row.render[row.rsize - 2] != '*' ||
                                   row.render[row.rsize - 1] != '/')));
    }

    /* Set every byte of row->hl (that corresponds to every character in the line)
 * to the right syntax highlight type (HL_* defines). */
    void update_syntax(erow &row, size_t row_index)
    {
        row.hl = static_cast<unsigned char *>(realloc(row.hl, row.rsize));
        memset(row.hl, HL_NORMAL, row.rsize);

        if (E.syntax == NULL)
            return; /* No syntax, everything is HL_NORMAL. */

        int i, prev_sep, in_string, in_comment;
        char *p;
        const auto &keywords = E.syntax->keywords;
        auto scs = E.syntax->singleline_comment_start;
        auto mcs = E.syntax->multiline_comment_start;
        auto mce = E.syntax->multiline_comment_end;

        /* Point to the first non-space char. */
        p = row.render;
        i = 0; /* Current char offset */
        while (*p && isspace(*p))
        {
            p++;
            i++;
        }
        prev_sep = 1;   /* Tell the parser if 'i' points to start of word. */
        in_string = 0;  /* Are we inside "" or '' ? */
        in_comment = 0; /* Are we inside multi-line comment? */

        /* If the previous line has an open comment, this line starts
     * with an open comment state. */
        if (row_index > 0 && has_open_comment(E.row[row_index - 1]))
            in_comment = 1;

        while (*p)
        {
            /* Handle // comments. */
            if (prev_sep && *p == scs[0] && *(p + 1) == scs[1])
            {
                /* From here to end is a comment */
                memset(row.hl + i, HL_COMMENT, row.chars.size() - i);
                return;
            }

            /* Handle multi line comments. */
            if (in_comment)
            {
                row.hl[i] = HL_MLCOMMENT;
                if (*p == mce[0] && *(p + 1) == mce[1])
                {
                    row.hl[i + 1] = HL_MLCOMMENT;
                    p += 2;
                    i += 2;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                }
                else
                {
                    prev_sep = 0;
                    p++;
                    i++;
                    continue;
                }
            }
            else if (*p == mcs[0] && *(p + 1) == mcs[1])
            {
                row.hl[i] = HL_MLCOMMENT;
                row.hl[i + 1] = HL_MLCOMMENT;
                p += 2;
                i += 2;
                in_comment = 1;
                prev_sep = 0;
                continue;
            }

            /* Handle "" and '' */
            if (in_string)
            {
                row.hl[i] = HL_STRING;
                if (*p == '\\')
                {
                    row.hl[i + 1] = HL_STRING;
                    p += 2;
                    i += 2;
                    prev_sep = 0;
                    continue;
                }
                if (*p == in_string)
                    in_string = 0;
                p++;
                i++;
                continue;
            }
            else
            {
                if (*p == '"' || *p == '\'')
                {
                    in_string = *p;
                    row.hl[i] = HL_STRING;
                    p++;
                    i++;
                    prev_sep = 0;
                    continue;
                }
            }

            /* Handle non printable chars. */
            if (!isprint(*p))
            {
                row.hl[i] = HL_NONPRINT;
                p++;
                i++;
                prev_sep = 0;
                continue;
            }

            /* Handle numbers */
            if ((isdigit(*p) && (prev_sep || row.hl[i - 1] == HL_NUMBER)) ||
                (*p == '.' && i > 0 && row.hl[i - 1] == HL_NUMBER))
            {
                row.hl[i] = HL_NUMBER;
                p++;
                i++;
                prev_sep = 0;
                continue;
            }

            /* Handle keywords and lib calls */
            if (prev_sep)
            {

                for (const auto &word : keywords)
                {
                    int klen = strlen(word);
                    int kw2 = word[klen - 1] == '|';
                    if (kw2)
                        klen--;

                    if (!memcmp(p, word, klen) &&
                        is_separator(*(p + klen)))
                    {
                        /* Keyword */
                        memset(row.hl + i, kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                        p += klen;
                        i += klen;
                        prev_sep = 0;
                        break;
                    }
                }
            }

            /* Not special chars */
            prev_sep = is_separator(*p);
            p++;
            i++;
        }

        /* Propagate syntax change to the next row if the open commen
     * state changed. This may recursively affect all the following rows
     * in the file. */
        bool oc = has_open_comment(row);
        if (row.hl_oc != oc && row_index + 1 < E.row.size())
            update_syntax(E.row[row_index + 1], row_index + 1);
        row.hl_oc = oc;
    }

    /* Maps syntax highlight token types to terminal colors. */
    constexpr int syntax_to_color(int hl)
    {
        switch (hl)
        {
        case HL_COMMENT:
        case HL_MLCOMMENT:
            return 36; /* cyan */
        case HL_KEYWORD1:
            return 33; /* yellow */
        case HL_KEYWORD2:
            return 32; /* green */
        case HL_STRING:
            return 35; /* magenta */
        case HL_NUMBER:
            return 31; /* red */
        case HL_MATCH:
            return 34; /* blu */
        default:
            return 37; /* white */
        }
    }

    /* Select the syntax highlight scheme depending on the filename,
 * setting it in the global state E.syntax. */
    void select_syntax_highlight(const std::string_view &&filename)
    {
        for (const auto &syntax : HLDB)
        {

            for (const auto &raw_extension : syntax.extensions)
            {
                std::string_view extension(raw_extension);
                auto position = filename.find_last_of(extension);
                if ((position != std::string_view::npos) && ((filename.length() - position) == extension.length()))
                {
                    E.syntax = &syntax;
                    return;
                }
            }
        }
    }

    /* ======================= Editor rows implementation ======================= */

    /* Update the rendered version and the syntax highlight of a row. */
    void update_row(erow &row, size_t row_index)
    {
        unsigned int tabs = 0, nonprint = 0;

        /* Create a version of the row we can directly print on the screen,
     * respecting tabs, substituting non printable characters with '?'. */
        free(row.render);
        for (const auto c : row.chars)
            if (c == TAB)
                tabs++;

        unsigned long long allocsize =
            (unsigned long long)row.chars.size() + tabs * 8 + nonprint * 9 + 1;
        if (allocsize > UINT32_MAX)
        {
            printf("Some line of the edited file is too long for kilo\n");
            exit(1);
        }

        row.render = static_cast<char *>(malloc(row.chars.size() + tabs * 8 + nonprint * 9 + 1));
        auto idx = 0;
        for (const auto c : row.chars)
        {
            if (c == TAB)
            {
                row.render[idx++] = ' ';
                while ((idx + 1) % 8 != 0)
                    row.render[idx++] = ' ';
            }
            else
            {
                row.render[idx++] = c;
            }
        }
        row.rsize = idx;
        row.render[idx] = '\0';

        /* Update the syntax highlighting attributes of the row. */
        update_syntax(row, row_index);
    }

    /* Insert a row at the specified position, shifting the other rows on the bottom
 * if required. */
    void insert_row(size_t at, const char *s)
    {
        if (at > E.row.size())
            return;

        E.row.emplace(E.row.begin() + at, s);
        update_row(E.row[at], at);
        E.dirty = true;
    }

    /* Remove the row at the specified position, shifting the remainign on the
 * top. */
    void delete_row(size_t at)
    {
        if (at >= E.row.size())
            return;

        E.row.erase(E.row.begin() + at);
        E.dirty = true;
    }

    /* Turn the editor rows into a single heap-allocated string.
 * Returns the pointer to the heap-allocated string and populate the
 * integer pointed by 'buflen' with the size of the string, escluding
 * the final nulterm. */
    std::vector<char> to_string()
    {
        size_t length = 0;

        /* Compute count of bytes */
        for (auto const &row : E.row)
        {
            length += row.chars.size() + 1; /* +1 is for "\n" at end of every row */
        }

        std::vector<char> result;
        result.reserve(length);
        for (auto const &row_struct : E.row)
        {
            for (const auto c : row_struct.chars)
            {
                result.push_back(c);
            }

            result.push_back('\n');
        }

        return result;
    }

    /* Insert a character at the specified position in a row, moving the remaining
 * chars on the right if needed. */
    void insert_character_to_row(erow &row, size_t char_index, int c, size_t row_index)
    {
        if (char_index > row.chars.size())
        {
            /* Pad the string with spaces if the insert location is outside the
         * current length by more than a single character. */
            int padlen = char_index - row.chars.size();
            /* In the next line +2 means: new char and null term. */
            for (auto i = 0; i < padlen; ++i)
            {
                row.chars.push_back(' ');
            }
        }

        row.chars.insert(row.chars.begin() + char_index, static_cast<char>(c));
        update_row(row, row_index);
        E.dirty = true;
    }

    /* Append the string 's' at the end of a row */
    void append_to_row(erow &row, const char *s, size_t row_index)
    {
        row.chars.append(s);
        update_row(row, row_index);
        E.dirty = true;
    }

    /* Delete the character at offset 'at' from the specified row. */
    void delete_character_from_row(erow &row, size_t at, size_t row_index)
    {
        if (row.chars.size() <= at)
            return;
        row.chars.erase(at, 1);
        update_row(row, row_index);
        E.dirty = true;
    }

    /* Insert the specified char at the current prompt position. */
    void insert_character(char c)
    {
        const auto filerow = E.rowoff + E.cy;
        const auto filecol = E.coloff + E.cx;
        erow *row = (filerow >= E.row.size()) ? NULL : &E.row[filerow];

        /* If the row where the cursor is currently located does not exist in our
     * logical representaion of the file, add enough empty rows as needed. */
        if (!row)
        {
            while (E.row.size() <= filerow)
                insert_row(E.row.size(), "");
        }
        row = &E.row[filerow];
        insert_character_to_row(*row, filecol, c, filerow);
        if (E.cx == E.screencols - 1)
            E.coloff++;
        else
            E.cx++;
        E.dirty = true;
    }

    /* Inserting a newline is slightly complex as we have to handle inserting a
 * newline in the middle of a line, splitting the line as needed. */
    void insert_newline(void)
    {
        const auto filerow = E.rowoff + E.cy;
        auto filecol = E.coloff + E.cx;
        erow *row = (filerow >= E.row.size()) ? NULL : &E.row[filerow];

        if (!row)
        {
            if (filerow == E.row.size())
            {
                insert_row(filerow, "");
                goto fixcursor;
            }
            return;
        }
        /* If the cursor is over the current line size, we want to conceptually
     * think it's just over the last character. */
        if (filecol >= row->chars.size())
            filecol = row->chars.size();
        if (filecol == 0)
        {
            insert_row(filerow, "");
        }
        else
        {
            /* We are in the middle of a line. Split it between two rows. */
            const auto split = row->chars.substr(filecol);
            insert_row(filerow + 1, split.c_str());
            row = &E.row[filerow];
            row->chars.erase(filecol);
            update_row(*row, filerow);
        }
    fixcursor:
        if (E.cy == E.screenrows - 1)
        {
            E.rowoff++;
        }
        else
        {
            E.cy++;
        }
        E.cx = 0;
        E.coloff = 0;
    }

    /* Delete the char at the current prompt position. */
    void delete_character()
    {
        const auto filerow = E.rowoff + E.cy;
        auto filecol = E.coloff + E.cx;
        erow *row = (filerow >= E.row.size()) ? NULL : &E.row[filerow];

        if (!row || (filecol == 0 && filerow == 0))
            return;
        if (filecol == 0)
        {
            /* Handle the case of column 0, we need to move the current line
         * on the right of the previous one. */
            filecol = E.row[filerow - 1].chars.size();
            auto row_index = filerow - 1;
            append_to_row(E.row[row_index], row->chars.c_str(), row_index);
            delete_row(filerow);
            row = NULL;
            if (E.cy == 0)
                E.rowoff--;
            else
                E.cy--;
            E.cx = filecol;
            if (E.cx >= E.screencols)
            {
                int shift = (E.screencols - E.cx) + 1;
                E.cx -= shift;
                E.coloff += shift;
            }
        }
        else
        {
            delete_character_from_row(*row, filecol - 1, filerow);
            if (E.cx == 0 && E.coloff)
                E.coloff--;
            else
                E.cx--;
        }
        if (row)
            update_row(*row, filerow);
        E.dirty = true;
    }

    /* Load the specified program in the editor memory and returns 0 on success
 * or 1 on error. */
    int open_file(const char *filename)
    {
        FILE *fp;

        E.dirty = false;
        free(E.filename);
        size_t fnlen = strlen(filename) + 1;
        E.filename = static_cast<char *>(malloc(fnlen));
        memcpy(E.filename, filename, fnlen);

        fp = fopen(filename, "r");
        if (!fp)
        {
            if (errno != ENOENT)
            {
                perror("Opening file");
                exit(1);
            }
            return 1;
        }

        char *line = NULL;
        size_t linecap = 0;
        ssize_t linelen;
        while ((linelen = getline(&line, &linecap, fp)) != -1)
        {
            if (linelen && (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
                line[--linelen] = '\0';
            insert_row(E.row.size(), line);
        }
        free(line);
        fclose(fp);
        E.dirty = false;
        return 0;
    }

    /* Save the current file on disk. Return 0 on success, 1 on error. */
    int save(void)
    {
        const auto buffer = to_string();
        const auto length = buffer.size();

        try
        {
            file_descriptor fd(open(E.filename, O_RDWR | O_CREAT, 0644), true);

            fd.truncate(length);
            fd.write(buffer);
            E.dirty = false;
            set_status(length, " bytes written on disk");
        }
        catch (const std::runtime_error &e)
        {
            set_status("Can't save! I/O error: ", strerror(errno));
        }

        return 1;
    }

    /* ============================= Terminal update ============================ */

    /* This function writes the whole screen using VT100 escape characters
 * starting from the logical state of the editor in the global state 'E'. */
    void refresh_screen(void)
    {
        erow *r;
        //char buf[32];
        std::stringstream output;

        output << "\x1b[?25l"; /* Hide cursor. */
        output << "\x1b[H";    /* Go home. */
        for (size_t y = 0; y < E.screenrows; y++)
        {
            const auto filerow = E.rowoff + y;

            if (filerow >= E.row.size())
            {
                if (E.row.size() == 0 && y == E.screenrows / 3)
                {

                    auto welcomelen = strlen(WELCOME);
                    int padding = (E.screencols - welcomelen) / 2;
                    if (padding)
                    {
                        output << '~';
                        padding--;
                    }
                    while (padding--)
                        output << ' ';
                    output << WELCOME;
                }
                else
                {
                    output << "~\x1b[0K\r\n";
                }
                continue;
            }

            r = &E.row[filerow];

            auto len = r->rsize - E.coloff;
            auto current_color = -1;
            if (len > 0)
            {
                if (len > E.screencols)
                    len = E.screencols;
                const auto c = r->render + E.coloff;
                const auto hl = r->hl + E.coloff;
                for (size_t j = 0; j < len; j++)
                {
                    if (hl[j] == HL_NONPRINT)
                    {
                        char sym;
                        output << "\x1b[7m";
                        if (c[j] <= 26)
                            sym = '@' + c[j];
                        else
                            sym = '?';
                        output << sym;
                        output << "\x1b[0m";
                    }
                    else if (hl[j] == HL_NORMAL)
                    {
                        if (current_color != -1)
                        {
                            output << "\x1b[39m";
                            current_color = -1;
                        }
                        output << c[j];
                    }
                    else
                    {
                        int color = syntax_to_color(hl[j]);
                        if (color != current_color)
                        {
                            output << "\x1b[" << color << 'm';
                            current_color = color;
                        }
                        output << c[j];
                    }
                }
            }
            output << "\x1b[39m";
            output << "\x1b[0K";
            output << "\r\n";
        }

        /* Create a two rows status. First row: */
        output << "\x1b[0K";
        output << "\x1b[7m";
        char status[80], rstatus[80];
        size_t len = snprintf(status, sizeof(status), "%.20s - %lu lines %s",
                              E.filename, E.row.size(), E.dirty ? "(modified)" : "");
        size_t rlen = snprintf(rstatus, sizeof(rstatus),
                               "%lu/%lu", E.rowoff + E.cy + 1, E.row.size());
        if (len > E.screencols)
            len = E.screencols;
        output << status;
        while (len < E.screencols)
        {
            if (E.screencols - len == rlen)
            {
                output << rstatus;
                break;
            }
            else
            {
                output << ' ';
                len++;
            }
        }
        output << "\x1b[0m\r\n";

        /* Second row depends on E.statusmsg and the status message update time. */
        output << "\x1b[0K";
        if (!E.status_message.empty() && time(NULL) - E.statusmsg_time < 5)
            output << E.status_message; //, msglen <= E.screencols ? msglen : E.screencols);

        /* Put cursor at its current position. Note that the horizontal position
     * at which the cursor is displayed may be different compared to 'E.cx'
     * because of TABs. */
        auto cx = 1;
        const auto filerow = E.rowoff + E.cy;
        erow *row = (filerow >= E.row.size()) ? NULL : &E.row[filerow];
        if (row)
        {
            for (auto j = E.coloff; j < (E.cx + E.coloff); j++)
            {
                if (j < row->chars.size() && row->chars[j] == TAB)
                    cx += 7 - ((cx) % 8);
                cx++;
            }
        }

        output << "\x1b[" << E.cy + 1 << ';' << cx << 'H';
        output << "\x1b[?25h"; /* Show cursor. */
        auto const data = output.str();
        write(STDOUT_FILENO, data.data(), data.size());
    }

    /* =============================== Find mode ================================ */

#define KILO_QUERY_LEN 256

    void find(int fd)
    {
        char query[KILO_QUERY_LEN + 1] = {0};
        int qlen = 0;
        int last_match = -1;    /* Last line where a match was found. -1 for none. */
        int find_next = 0;      /* if 1 search next, if -1 search prev. */
        int saved_hl_line = -1; /* No saved HL */
        char *saved_hl = NULL;

#define FIND_RESTORE_HL                                                            \
    do                                                                             \
    {                                                                              \
        if (saved_hl)                                                              \
        {                                                                          \
            memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize); \
            free(saved_hl);                                                        \
            saved_hl = NULL;                                                       \
        }                                                                          \
    } while (0)

        /* Save the cursor position in order to restore it later. */
        int saved_cx = E.cx, saved_cy = E.cy;
        int saved_coloff = E.coloff, saved_rowoff = E.rowoff;

        while (1)
        {
            set_status(
                "Search: ", query, " (Use ESC/Arrows/Enter)");
            refresh_screen();

            int c = read_key(fd);
            if (c == DEL_KEY || c == CTRL_H || c == BACKSPACE)
            {
                if (qlen != 0)
                    query[--qlen] = '\0';
                last_match = -1;
            }
            else if (c == ESC || c == ENTER)
            {
                if (c == ESC)
                {
                    E.cx = saved_cx;
                    E.cy = saved_cy;
                    E.coloff = saved_coloff;
                    E.rowoff = saved_rowoff;
                }
                FIND_RESTORE_HL;
                set_status();
                return;
            }
            else if (c == ARROW_RIGHT || c == ARROW_DOWN)
            {
                find_next = 1;
            }
            else if (c == ARROW_LEFT || c == ARROW_UP)
            {
                find_next = -1;
            }
            else if (isprint(c))
            {
                if (qlen < KILO_QUERY_LEN)
                {
                    query[qlen++] = c;
                    query[qlen] = '\0';
                    last_match = -1;
                }
            }

            /* Search occurrence. */
            if (last_match == -1)
                find_next = 1;
            if (find_next)
            {
                char *match = NULL;
                int match_offset = 0;
                int current = last_match;

                for (size_t i = 0; i < E.row.size(); i++)
                {
                    current += find_next;
                    if (current == -1)
                        current = E.row.size() - 1;
                    else if (static_cast<size_t>(current) == E.row.size())
                        current = 0;
                    match = strstr(E.row[current].render, query);
                    if (match)
                    {
                        match_offset = match - E.row[current].render;
                        break;
                    }
                }
                find_next = 0;

                /* Highlight */
                FIND_RESTORE_HL;

                if (match)
                {
                    erow *row = &E.row[current];
                    last_match = current;
                    if (row->hl)
                    {
                        saved_hl_line = current;
                        saved_hl = static_cast<char *>(malloc(row->rsize));
                        memcpy(saved_hl, row->hl, row->rsize);
                        memset(row->hl + match_offset, HL_MATCH, qlen);
                    }
                    E.cy = 0;
                    E.cx = match_offset;
                    E.rowoff = current;
                    E.coloff = 0;
                    /* Scroll horizontally as needed. */
                    if (E.cx > E.screencols)
                    {
                        int diff = E.cx - E.screencols;
                        E.cx -= diff;
                        E.coloff += diff;
                    }
                }
            }
        }
    }

    /* ========================= Editor events handling  ======================== */

    /* Handle cursor position change because arrow keys were pressed. */
    void move_cursor(int key)
    {
        auto filerow = E.rowoff + E.cy;
        auto filecol = E.coloff + E.cx;
        size_t rowlen;
        erow *row = (filerow >= E.row.size()) ? NULL : &E.row[filerow];

        switch (key)
        {
        case ARROW_LEFT:
            if (E.cx == 0)
            {
                if (E.coloff)
                {
                    E.coloff--;
                }
                else
                {
                    if (filerow > 0)
                    {
                        E.cy--;
                        E.cx = E.row[filerow - 1].chars.size();
                        if (E.cx > E.screencols - 1)
                        {
                            E.coloff = E.cx - E.screencols + 1;
                            E.cx = E.screencols - 1;
                        }
                    }
                }
            }
            else
            {
                E.cx -= 1;
            }
            break;
        case ARROW_RIGHT:
            if (row && filecol < row->chars.size())
            {
                if (E.cx == E.screencols - 1)
                {
                    E.coloff++;
                }
                else
                {
                    E.cx += 1;
                }
            }
            else if (row && filecol == row->chars.size())
            {
                E.cx = 0;
                E.coloff = 0;
                if (E.cy == E.screenrows - 1)
                {
                    E.rowoff++;
                }
                else
                {
                    E.cy += 1;
                }
            }
            break;
        case ARROW_UP:
            if (E.cy == 0)
            {
                if (E.rowoff)
                    E.rowoff--;
            }
            else
            {
                E.cy -= 1;
            }
            break;
        case ARROW_DOWN:
            if (filerow < E.row.size())
            {
                if (E.cy == E.screenrows - 1)
                {
                    E.rowoff++;
                }
                else
                {
                    E.cy += 1;
                }
            }
            break;
        }
        /* Fix cx if the current line has not enough chars. */
        filerow = E.rowoff + E.cy;
        filecol = E.coloff + E.cx;
        row = (filerow >= E.row.size()) ? NULL : &E.row[filerow];
        rowlen = row ? row->chars.size() : 0;
        if (filecol > rowlen)
        {
            const auto reminder = filecol - rowlen;
            if (reminder > E.cx)
            {
                E.coloff -= reminder - E.cx;
                E.cx = 0;
            }
            else
            {
                E.cx -= reminder;
            }
        }
    }

/* Process events arriving from the standard input, which is, the user
 * is typing stuff on the terminal. */
#define KILO_QUIT_TIMES 3
    bool process_keypress(int fd)
    {
        /* When the file is modified, requires Ctrl-q to be pressed N times
     * before actually quitting. */
        static int quit_times = KILO_QUIT_TIMES;

        int c = read_key(fd);
        switch (c)
        {
        case ENTER: /* Enter */
            insert_newline();
            break;
        case CTRL_C: /* Ctrl-c */
            /* We ignore ctrl-c, it can't be so simple to lose the changes
         * to the edited file. */
            break;
        case CTRL_Q: /* Ctrl-q */
            /* Quit if the file was already saved. */
            if (E.dirty && quit_times)
            {
                set_status("WARNING!!! File has unsaved changes. "
                           "Press Ctrl-Q ",
                           quit_times,
                           " more times to quit.");
                quit_times--;
                return true;
            }
            return false;
        case CTRL_S: /* Ctrl-s */
            save();
            break;
        case CTRL_F:
            find(fd);
            break;
        case BACKSPACE: /* Backspace */
        case CTRL_H:    /* Ctrl-h */
        case DEL_KEY:
            delete_character();
            break;
        case PAGE_UP:
        case PAGE_DOWN:
            if (c == PAGE_UP && E.cy != 0)
                E.cy = 0;
            else if (c == PAGE_DOWN && E.cy != E.screenrows - 1)
                E.cy = E.screenrows - 1;
            {
                int times = E.screenrows;
                while (times--)
                    move_cursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            move_cursor(c);
            break;
        case CTRL_L: /* ctrl+l, clear screen */
            /* Just refresht the line as side effect. */
            break;
        case ESC:
            /* Nothing to do for ESC in this mode. */
            break;
        default:
            insert_character(c);
            break;
        }

        quit_times = KILO_QUIT_TIMES; /* Reset it to the original value. */
        return true;
    }

    int editorFileWasModified(void)
    {
        return E.dirty;
    }

    void update_window_size(void)
    {
        get_window_size(STDIN_FILENO, STDOUT_FILENO,
                        E.screenrows, E.screencols);
        E.screenrows -= 2; /* Get room for status bar. */
    }

    void signal_handler(int unused __attribute__((unused)))
    {
        update_window_size();
        if (E.cy > E.screenrows)
            E.cy = E.screenrows - 1;
        if (E.cx > E.screencols)
            E.cx = E.screencols - 1;
        refresh_screen();
    }

    void init(void)
    {
        E.cx = 0;
        E.cy = 0;
        E.rowoff = 0;
        E.coloff = 0;
        E.dirty = false;
        E.filename = NULL;
        E.syntax = NULL;
        update_window_size();
        signal(SIGWINCH, signal_handler);
    }

} // namespace kilopp

using namespace kilopp;

int main(int argc, char **argv)
{
    if (argc != 2)
    {
        fprintf(stderr, "Usage: kilo <filename>\n");
        exit(1);
    }

    init();
    select_syntax_highlight(argv[1]);
    open_file(argv[1]);
    raw_mode rm;

    set_status(
        "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");
    do
    {
        refresh_screen();
    } while (process_keypress(STDIN_FILENO));
    return 0;
}