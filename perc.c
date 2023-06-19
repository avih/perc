/*********************************************************************
 * perc: PE resources command line utility
 *       list/add/delete or extract resources in Windows PE file
 *
 * Copyright 2023 Avi Halachmi   avihpit@yahoo.com   https://github.com/avih
 * License: MIT - see LICENSE file
 *
 * build (with unicode file/resource names support):
 *   mingw: gcc -municode perc.c -o perc.exe
 *   tcc:   tcc perc.c
 *   msvc:  cl perc.c
 *
 * to disable unicode: remove -municode, add -DANSI (gcc/tcc) or /DANSI (msvc).
 *
 * Note: for win10 1903+ one could disable unicode and attach a UTF-8 manifest,
 *       but listing seem broken in some cases. see the end of this file.
 ********************************************************************/
 
#if !defined(UNICODE) && !defined(ANSI)
    #define UNICODE
#endif
#if defined(UNICODE) && !defined(_UNICODE)
    #define _UNICODE
#endif

#define __USE_MINGW_ANSI_STDIO 0  /* mingw: msvcrt printf is enough for us */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <tchar.h>
#include <windows.h>


static LPCTSTR argv0;


/*************************************************************
 * general utils
 ************************************************************/

#ifndef UNUSED
    #define UNUSED(var) (void)(var)
#endif

#ifndef LOAD_LIBRARY_AS_IMAGE_RESOURCE
    #define LOAD_LIBRARY_AS_IMAGE_RESOURCE 0x00000020
#endif

#define lang_neutral MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL)

#define ISCONSOLE(handle) (GetConsoleMode((handle), &(DWORD){0}))

#define ARRLEN(a) (sizeof(a) / sizeof *(a))


#define xmalloc(siz) xrealloc(0, (siz))

static void *xrealloc(void *p, size_t siz) {
    p = realloc(p, siz);
    if (p)
        return p;

    fputs("OOM\n", stderr);
    abort();
}


/*************************************************************
 * unicode-aware output routines
 ************************************************************/

// mfputs[_len], mfprintf are passthrough to fputs and fprintf when compiled
// without unicode support, but in unicode, they take wchar_t strings, and
// output using console API (if a tty), or as utf8 (otherwise, e.g. redirect)
#define mfputs(s, f) mfputs_len((s), (f), -1)

static int mfputs_len(LPCTSTR s, FILE *f, int inlen)
{
    if (!s && inlen)
        return EOF;  // allow null s if inlen==0

    int tlen = inlen >= 0 ? inlen : (int)_tcslen(s);
    if (!tlen)
        return 0;  // no-op. also, WideCharToMultiByte is murky with len 0

#ifndef UNICODE
    return fwrite(s, 1, (size_t)tlen, f) == (size_t)tlen ? 0 : EOF;
#else
    HANDLE h = (HANDLE)_get_osfhandle(_fileno(f));
    if (ISCONSOLE(h))
        return WriteConsoleW(h, s, (DWORD)tlen, 0, 0) ? 0 : EOF;

    int n = WideCharToMultiByte(CP_UTF8, 0, s, tlen, 0, 0, 0, 0);
    if (n <= 0)
        return EOF;

    char buf[256], *us = n <= (int)ARRLEN(buf) ? buf : xmalloc((size_t)n);
    int ret = WideCharToMultiByte(CP_UTF8, 0, s, tlen, us, n, 0, 0) == n &&
              fwrite(us, 1, (size_t)n, f) == (size_t)n ? 0 : EOF;

    if (us != buf)
        free(us);
    return ret;
#endif  /* defined(UNICODE) */
}


#ifndef UNICODE
    #define mfprintf fprintf
#else
static int mfprintf(FILE *f, const wchar_t *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    int len = _vsnwprintf(0, 0, fmt, ap);
    va_end(ap);
    if (len <= 0)  // 0 is valid but no-op
        return len;

    wchar_t wbuf[256], *out = wbuf;
    if ((size_t)len > ARRLEN(wbuf))
        out = xmalloc((size_t)len * sizeof(wchar_t));

    va_start(ap, fmt);
    int r = len == _vsnwprintf(out, (size_t)len, fmt, ap) ?
            mfputs_len(out, f, len) : -1;
    va_end(ap);

    if (out != wbuf)
        free(out);
    return r;
}
#endif  /* unicode mfprintf */


/*************************************************************
 * the rest of the file is unicode-agnostic by using tchar types/names
 ************************************************************/

static LPCTSTR winerrmsg(DWORD e)  // reused static output buffer
{
    static _TCHAR ebuf[512];
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM |
                  FORMAT_MESSAGE_IGNORE_INSERTS |
                  FORMAT_MESSAGE_MAX_WIDTH_MASK,
                  0, e, lang_neutral, ebuf, ARRLEN(ebuf), 0);
    return ebuf;
}

#define LASTERRMSG() winerrmsg(GetLastError())

// expecting at least one arg after fmt, possibly dummy, e.g. Err("foo%s","")
#define Err(fmt, ...) mfprintf(stderr, _T("%s: ") _T(fmt), argv0, __VA_ARGS__)
#define ErrCleanup(...)  do { Err(__VA_ARGS__); goto cleanup; } while(0)


// NULL + err msg on failure, else pointer to malloced buf and *out_len set
static void *file2mem(LPCTSTR fname, size_t *out_len)
{
    static const size_t LIMIT = 1e9;  // maximum allocation (data < LIMIT)
    unsigned char *buf = 0;
    size_t len = 0, siz = 8192;
    FILE *f = fname[0] == '-' && !fname[1] ? stdin : _tfopen(fname, _T("rb"));
    LPCTSTR disp = f == stdin ? _T("<stdin>") : fname;

    if (f == stdin)
        _setmode(_fileno(stdin), _O_BINARY);

    if (!f)
        ErrCleanup("cannot open file -- %s\n", disp);

    do {
        siz = siz <= LIMIT/2 ? siz*2 : LIMIT;
        buf = xrealloc(buf, siz);
        len += fread(buf+len, 1, siz-len, f);
    } while (len == siz && siz < LIMIT);

    if (len == LIMIT)
        ErrCleanup("file too big -- %s\n", disp);
    if (ferror(f))
        ErrCleanup("read error -- %s\n", disp);

    if (f != stdin)
        fclose(f);
    buf = xrealloc(buf, len);
    *out_len = len;
    return buf;

cleanup:
    free(buf);
    if (f != stdin)
        fclose(f);
    return 0;
}


/*************************************************************
 * resource utilities
 ************************************************************/

// rc3 is either a full rc id of type+name+lang, or a subset used as a filter

// type and name value of NULL (== MAKEINTRESOURCE(0)) is unset/invalid, but
// lang can be valid 0-0xffff, so compound of low WORD val, high/all "isset"
#define makeclang(v) (1<<16 | (WORD)(v))

typedef struct rc3 {
    LPCTSTR type;
    LPCTSTR name;
    DWORD clang;  // compound lang
} rc3;


/*************************************************************
 * symbolic resource type strings mapping, printout
 ************************************************************/
#define int_INTRESOURCE(v) ((int)(USHORT)(ULONG_PTR)(v))

#define rtp(name) {_T(#name), RT_ ## name} /* rtp(FOO) -> {[L]"FOO", RT_FOO} */
static LPCTSTR rtmap[][2] = {
    rtp(ACCELERATOR), rtp(ANICURSOR), rtp(ANIICON), rtp(BITMAP), rtp(CURSOR),
    rtp(DIALOG), rtp(DLGINCLUDE), rtp(FONT), rtp(FONTDIR), rtp(GROUP_CURSOR),
    rtp(GROUP_ICON), rtp(HTML), rtp(ICON), rtp(MANIFEST), rtp(MENU), rtp(VXD),
    rtp(MESSAGETABLE), rtp(PLUGPLAY), rtp(RCDATA), rtp(STRING), rtp(VERSION),
};

// resource pointer -> symbol str "FOO" or NULL
static LPCTSTR symbol_str_from_int_rt(const LPCTSTR irt)
{
    for (size_t i = 0; i < ARRLEN(rtmap); ++i)
        if (rtmap[i][1] == irt)
            return rtmap[i][0];
    return 0;
}
// "FOO" -> RT_FOO or NULL
static LPCTSTR int_rt_from_symbol_str(const LPCTSTR s)
{
    for (size_t i = 0; i < ARRLEN(rtmap); ++i)
        if (!_tcscmp(s, rtmap[i][0]))
            return rtmap[i][1];
    return 0;
}

// for type and name: prints SYMBOL/int/"STR"/<any>
static void print_dual_val(FILE *f, LPCTSTR v, int rtsymbols)
{
    if (!v) {
        fputs("<any>", f);
    } else if (IS_INTRESOURCE(v)) {
        LPCTSTR sym = rtsymbols ? symbol_str_from_int_rt(v) : 0;
        if (sym)
            mfputs(sym, f);
        else
            fprintf(f, "%d", int_INTRESOURCE(v));
    } else {
        // C "STRING": '\\' and '"' are escaped, 127 and c<32 are '\nnn' octal
        fputc('"', f);
        for (LPCTSTR e = v; ; v = ++e) {
            while ((unsigned)*e >= 32 && *e != 127 && *e != '\\' && *e != '\"')
                ++e; // max str to avoid break mid surrogate pair, and less IO
            mfputs_len(v, f, (int)(e - v));
            if (!*e)
                break;
            if (*e == '\\' || *e == '\"')
                fprintf(f, "\\%c", (char)*e);
            else
                fprintf(f, "\\%03o", (char)*e);
        }
        fputc('"', f);
    }
}

static void print_rc(FILE *f, const rc3 *rc, int rtsymbols)
{
    print_dual_val(f, rc->type, rtsymbols);
    fputc('/', f);
    print_dual_val(f, rc->name, 0);
    if (rc->clang)
        fprintf(f, "/%u", (unsigned)(WORD)rc->clang);
    else
        fputs("/<any>", f);
}


/*************************************************************
 * "stand alone" extract - resource rc to stdout or print an error
 ************************************************************/
static int extract(LPCTSTR pefile, rc3 *rc, int dummy)
{
    int ret = 1;

    HMODULE hm = LoadLibraryEx(pefile, 0, DONT_RESOLVE_DLL_REFERENCES |
                                          LOAD_LIBRARY_AS_DATAFILE |
                                          LOAD_LIBRARY_AS_IMAGE_RESOURCE);
    if (!hm)
        ErrCleanup("LoadLibraryEx: %s\n", LASTERRMSG());

    HRSRC hr = FindResourceEx(hm, rc->type, rc->name, (WORD)rc->clang);
    if (!hr)
        ErrCleanup("FindResourceEx: %s\n", LASTERRMSG());

    HGLOBAL hg = LoadResource(hm, hr);
    if (!hg)
        ErrCleanup("LoadResource: %s\n", LASTERRMSG());

    void *rdata = LockResource(hg);
    if (!rdata)
        ErrCleanup("LockResource: %s\n", LASTERRMSG());

    if (!dummy) {
        _setmode(_fileno(stdout), _O_BINARY);

        DWORD siz = SizeofResource(hm, hr);
        if (siz != fwrite(rdata, 1, siz, stdout))
            ErrCleanup("extract: write error\n%s","");
    }

    ret = 0;

cleanup:
    if (hm)
        FreeLibrary(hm);
    return ret;
}


/*************************************************************
 * iterator function "iter": takes a filter, calls back on matches
 ************************************************************/

// hm,rc,index,ctx  return: 0 continues, >0 aborts and returned by "iter"
// non-int-resource type/name are valid only during the cb - dup if needed
typedef int (*rccb_fn)(HMODULE, const rc3*, unsigned, void *);

typedef struct iter_ctx {
    rc3 *filter;  // 0 values are ignored, else required values
    rccb_fn cb;
    void *user_ctx;
    unsigned index;
    int user_ret;
} iter_ctx;

#define filter_match(fv, rcv)  /* filter, val - type and name comparison */ \
    (!(fv) || (!IS_INTRESOURCE(fv) == !IS_INTRESOURCE(rcv) && \
               (IS_INTRESOURCE(fv) ? (fv) == (rcv) : !_tcscmp((fv), (rcv)))))

static BOOL CALLBACK
EnumLangCb(HMODULE hm, LPCTSTR rtype, LPCTSTR rname, WORD rlang, LONG_PTR vctx)
{
    iter_ctx *ictx = (iter_ctx*)vctx;
    ictx->index++;  // ignoring wrap around at 4G indices...

    if (!filter_match(ictx->filter->type, rtype) ||
        !filter_match(ictx->filter->name, rname) ||
        (ictx->filter->clang && (WORD)ictx->filter->clang != rlang))
    {
        return TRUE;
    }

    rc3 rc = { .type = rtype, .name = rname, .clang = makeclang(rlang) };
    int ret = ictx->cb(hm, &rc, ictx->index, ictx->user_ctx);
    ictx->user_ret = ret;
    return !ret;  // ret==0 continue, else abort and ret is returned by "iter"
}

static BOOL CALLBACK
EnumNameCb(HMODULE hm, LPCTSTR rtype, LPTSTR rname, LONG_PTR ictx)
{
    BOOL r = EnumResourceLanguages(hm, rtype, rname, EnumLangCb, ictx);
    if (!r && !((iter_ctx*)ictx)->user_ret) {
        // aborted, but not by the user callback. warn and continue
        Err("warning: EnumResourceLanguages: %s\n", LASTERRMSG());
        return TRUE;
    }
    return r;
}

static BOOL CALLBACK
EnumTypeCb(HMODULE hm, LPTSTR rtype, LONG_PTR ictx)
{
    BOOL r = EnumResourceNames(hm, rtype, EnumNameCb, ictx);
    if (!r && !((iter_ctx*)ictx)->user_ret) {
        Err("warning: EnumResourceNames: %s\n", LASTERRMSG());
        return TRUE;
    }
    return r;
}

// fn should return 0 to continue, else >0. iter returns it, or <0 for others
static int iter(LPCTSTR pefile, rc3 *filter, rccb_fn cb, void *ctx)
{
    HMODULE hm = LoadLibraryEx(pefile, 0, DONT_RESOLVE_DLL_REFERENCES |
                                          LOAD_LIBRARY_AS_DATAFILE |
                                          LOAD_LIBRARY_AS_IMAGE_RESOURCE);
    if (!hm)
        ErrCleanup("LoadLibraryEx: %s\n", LASTERRMSG());

    // to maintain correct index, we enum all rcs even if some values are known
    iter_ctx ictx = { .filter = filter, .cb = cb, .user_ctx = ctx };
    BOOL ok = EnumResourceTypes(hm, EnumTypeCb, (LONG_PTR)&ictx);
    DWORD le = GetLastError();

    FreeLibrary(hm);

    if (ictx.user_ret)  // abort from user callback
        return ictx.user_ret;
    if (ok || le == ERROR_RESOURCE_DATA_NOT_FOUND /* no .rsrc section */)
        return 0;
    Err("EnumResourceTypes: %s\n", winerrmsg(le));

cleanup:
    return -1;
}


/*************************************************************
 * resource utilities which use the iterator
 ************************************************************/

static int del_cb(HMODULE hm, const rc3 *rc, unsigned index, void *vctx)
{
    UNUSED(hm); UNUSED(index);

    if (!UpdateResource((HANDLE)vctx, rc->type, rc->name, (WORD)rc->clang, 0, 0)) {
        Err("UpdateResource: %s\n", LASTERRMSG());
        return 1;
    }

    return 0;
}
// del matched resources. dummy is applied at EndUpdateResource, not at del_cb
static int del_rcs(LPCTSTR pefile, rc3* filter, HANDLE hupdate)
{
    return iter(pefile, filter, del_cb, hupdate);
}


typedef struct fictx { unsigned target; rc3* out; } fictx;
static int find_cb(HMODULE hm, const rc3 *rc, unsigned index, void *vctx)
{
    UNUSED(hm);
    fictx* ctx = (fictx*)vctx;

    if (ctx->target == index) {  // non-int-resources are transient
        ctx->out->type = IS_INTRESOURCE(rc->type) ? rc->type : _tcsdup(rc->type);
        ctx->out->name = IS_INTRESOURCE(rc->name) ? rc->name : _tcsdup(rc->name);
        ctx->out->clang = rc->clang;
        return 1;
    }
    return 0;
}
// get the TYPE+ID+LANG of resource INDEX
// <0: error, 0: not found, 1: found. out name/type are dups if not int rsrc
static int find_index(LPCTSTR pefile, unsigned index, rc3 *out)
{
    fictx ctx = { .target = index, .out = out };
    return iter(pefile, &(rc3){0}, find_cb, &ctx);
}


typedef struct lictx { int rtsymbols; int dummy; } lictx;
static int list_cb(HMODULE hm, const rc3 *rc, unsigned index, void *vctx)
{
    lictx *ctx = (lictx*)vctx;
    if (ctx->dummy)
        return 0;

    printf("%u: ", index);
    print_rc(stdout, rc, ctx->rtsymbols);

    HRSRC hr = FindResourceEx(hm, rc->type, rc->name, (WORD)rc->clang);
    if (hr)
        printf(" (%u)\n", (unsigned)SizeofResource(hm, hr));
    else
        fputs(" (UNKNOWN)\n", stdout);

    return 0;
}
// prints INDEX: TYPE/ID/LANG (SIZE) for matched items, TYPE possibly symbolic
static int list(LPCTSTR pefile, rc3* filter, int rtsymbols, int dummy)
{
    lictx ctx = { .rtsymbols = rtsymbols, .dummy = dummy };
    return iter(pefile, filter, list_cb, &ctx);
}


/*************************************************************
 * arguments parsing
 ************************************************************/
// plan9 args processing by avih, parametric for char (T)ype and (I)nit expr.
// unlike the plan9 ARGBEGIN, this one allows repeated use of [E]ARGF().
// also, argc==0 is allowed (some implementations are buggy/crashy if argc==0)
// vars: a_: whether to advance to the next argv item on next iteration
//       p_: points to next unprocessed char in an argv item, or 0 if argv ends
#define ARGS_(T, I) { T *a_ = *argv, *p_ = 0, c_; (void)(I); \
                      while ((void)(a_ && (a_ = 0, --argc, p_ = *++argv)), \
                             p_ && (p_ != *argv || (*p_ == '-' && *++p_ && \
                             (*p_ != '-' || p_[1] || (--argc, ++argv, 0))))) \
                        switch (c_ = *p_, *++p_ || (--argc, p_ = *++argv), c_)

#define ARGBEGIN    ARGS_(char, argv0 || (argv0 = *argv ? *argv : ""))
#define TARGBEGIN   ARGS_(_TCHAR, argv0 || (argv0 = *argv ? *argv : _T("")))

#define     ARGC()      c_
#define     ARGF()      (a_ = p_)
#define     EARGF(x)    (p_ ? ARGF() : ((x), abort(), p_))

#define ARGEND      }


/*************************************************************
 * main, and supporting utilities
 ************************************************************/

static LPCTSTR usage_fmt =
    _T("Usage: %s  -h  |  -{D|L|R|a:|d|x} [-{t:i:l:}] [INDEX] PE\n");

static const char *help =
    "Add, replace, delete, extract or list resources in Windows PE file.\n"
    "\n"
    "  Actions - exactly one must be specified, possibly with -n:\n"
    "    -h       Print this help and exit.\n"
    "    -n       No-write: don't modify PE, don't write list or extracted resource.\n"
    "    -D       Delete all (*) resources in PE.\n"
    "    -L       List all (*) resources in PE as  INDEX: TYPE/ID/LANG (SIZE).\n"
    "    -R       Same as -L, but TYPE is printed as raw integer, if possible.\n"
    "    -d       Delete the resource TYPE/ID/LANG in PE.\n"
    "    -x       Extract the resource TYPE/ID/LANG in PE to stdout (**).\n"
    "    -a RSRC  Add resource file RSRC (stdin if '-') as TYPE/ID/LANG to PE (**).\n"
    "  (*)  If any of TYPE, LANG, ID are given: only items with all given values.\n"
    "  (**) Raw data directly from/to PE. E.g. ICON is without header, etc.\n"
    "\n"
    "  Target - at most one TYPE, one ID, and one LANG (INDEX overrides others):\n"
    "    -t TYPE  Symbol/int/@STR resource type. E.g. -t ICON, -t 2, -t @\"foo bar\".\n"
    "    -i ID    Int/@STR resource ID/NAME.\n"
    "    -l LANG  Int resource language. Default for -a/-x/-d is %u (neutral).\n"
    "    INDEX    Sets TYPE+ID+LANG of existing resource INDEX (from -L/-R output).\n"
    "\n"
    "perc v0.1  PE Resource utility  https://github.com/avih/perc\n";

static int  // 0 if bad, else 1
strtol_in_range(LPCTSTR s, long low, long high, long *out)
{
    LPTSTR end;
    errno = 0;  // to detect out of range
    *out = _tcstol(s, &end, 0);
    return !errno && *s && !*end && *out >= low && *out <= high;
}

// MAKEINTRESOURCE(0) is 0, but it's invalid type/name, can be used as error
static LPCTSTR user_input_to_rsrc(LPCTSTR s, LPCTSTR disp, int rtsymbols)
{
    if (*s == '@')
        return s + 1;

    // MAKEINTRESOURCE only uses the low 16 bits, but we allow int range
    // because the docs say it takes "integer", and some places use e.g. -1
    long val;
    if (strtol_in_range(s, INT_MIN, INT_MAX, &val)) {
        if (MAKEINTRESOURCE((int)val) == 0) {
            if (val)
                ErrCleanup("%s %d maps to 0, which is illegal\n", disp, (int)val);
            else
                ErrCleanup("%s 0 is illegal\n", disp);
        }
        return MAKEINTRESOURCE((int)val);
    }

    if (!rtsymbols)
        ErrCleanup("invalid %s number -- %s\n", disp, s);

    LPCTSTR irc = int_rt_from_symbol_str(s);
    if (irc)
        return irc;

    Err("invalid %s number or unknown symbol -- %s\n", disp, s);

cleanup:
    return 0;
}


int _tmain(int, _TCHAR **); // gcc wants wmain proto with -Wmissing-prototypes
int _tmain(int argc, _TCHAR **argv)
{
    #define Usage(f) mfprintf((f), usage_fmt, argv0)
    #define UsageCleanup() { Usage(stderr); goto cleanup; }
    #define ErrUsageCleanup(...) { Err(__VA_ARGS__); UsageCleanup(); }
    #define GetArgf() if (!(argf = ARGF())) \
                ErrUsageCleanup("option requires an argument -- %c\n", ARGC())

    int ret = 1;
    int do_dummy = 0, action = 0;
    rc3 rc = {0};
    unsigned index = 0;
    LPCTSTR rfile = 0;
    void *rdata = 0;
    size_t rlen = 0;

    HANDLE h;
    LPTSTR argf;
    long argl;


    TARGBEGIN {
    case 'h':
        Usage(stdout);
        printf(help, (unsigned)lang_neutral);
        return 0;

    case 'n':
        do_dummy = 1;
        break;

    case 'L': case 'R': case 'D': case 'a': case 'd': case 'x':
        if (action)
            ErrUsageCleanup("expecting exactly one of -[LRDadx].\n%s","");
        action = ARGC();

        if (action == 'a') {
            GetArgf();
            rfile = argf;
        }
        break;

    case 't':
        GetArgf();
        if (!(rc.type = user_input_to_rsrc(argf, _T("TYPE"), 1)))
            goto cleanup;
        break;

    case 'i':
        GetArgf();
        if (!(rc.name = user_input_to_rsrc(argf, _T("ID"), 0)))
            goto cleanup;
        break;

    case 'l':
        GetArgf();
        if (!strtol_in_range(argf, 0, (WORD)-1, &argl))
            ErrCleanup("invalid LANG number -- %s\n", argf);
        rc.clang = makeclang(argl);
        break;

    default:
        ErrUsageCleanup("illegal option -- %c\n", ARGC());
    } ARGEND

    if (!action || argc < 1 || argc > 2)
        UsageCleanup();

    if (argc == 2) {
        if (!strtol_in_range(*argv, 1, INT_MAX, &argl))
            ErrCleanup("invalid INDEX number -- %s\n", *argv);
        index = (unsigned)argl;
        --argc, ++argv;
    }
    if (index && (argl = find_index(*argv, index, &rc)) <= 0) {
        if (argl == 0)  // <0 error already has message
            Err("INDEX not found -- %u\n", index);
        index = 0;  // else cleanup will try to free rc.name/type
        goto cleanup;
    }

    // arguments seem good. try to execute


    if (action == 'L' || action == 'R') {
        ret = list(*argv, &rc, action == 'L', do_dummy);
        goto cleanup;
    }

    if (action == 'a' || action == 'd' || action == 'x') {
        if (!rc.clang)
            rc.clang = makeclang(lang_neutral);
        if (!rc.type || !rc.name)
            ErrCleanup("missing %s\n", rc.type ? _T("ID") : _T("TYPE"));
    }

    if (action == 'x') {
        ret = extract(*argv, &rc, do_dummy);
        goto cleanup;
    }


    // remaining actions: a/d/D - all use begin/end UpdateResource

    // note that if del_all is set but the file doesn't have .rsrc section,
    // then it's created. we don't detect/avoid it - we're an API wrapper.
    int del_all = action == 'D' && !rc.type && !rc.name && !rc.clang;

    h = BeginUpdateResource(*argv, del_all);
    if (!h)
        ErrCleanup("BeginUpdateResource: %s\n", LASTERRMSG());

    switch(action) {
    case 'D':
        if (!del_all && del_rcs(*argv, &rc, h))
            goto cleanup;  // already printed any errors
        break;

    case 'a':
        rdata = file2mem(rfile, &rlen);
        if (!rdata)
            goto cleanup;  // file2mem prints messages
        if (rlen > (DWORD)-1)
            ErrCleanup("file too big -- %s\n", rfile);
        // FALLTHROUGH
    case 'd':
        if (!UpdateResource(h, rc.type, rc.name, (WORD)rc.clang, rdata, (DWORD)rlen))
            ErrCleanup("UpdateResource: %s\n", LASTERRMSG());
        break;

    default:
        ErrCleanup("internal error\n%s","");
    }

    if (!EndUpdateResource(h, do_dummy))
        ErrCleanup("EndUpdateResource: %s\n", LASTERRMSG());

    ret = 0;

cleanup:
    if (index) {  // type/name are int resource or malloced
        if (!IS_INTRESOURCE(rc.type))
            free((void*)(uintptr_t)rc.type);
        if (!IS_INTRESOURCE(rc.name))
            free((void*)(uintptr_t)rc.name);
    }
    free(rdata);
    return ret;
}


/**************  MS bug with ANSI build + UTF-8 manifest? ****************
adding a type with backslash and then a type with some asian chars seem to
break listing. e.g. compile percU.exe as unicode, and percu8.exe as ANSI
with UTF-8 manifest, then compile 'int main() {return 0;}' as x.exe, and then
(in cmd.exe prompt):

echo x | percU -a- -i1 -t@x\y x.exe   (x backslash y - it's taken literally)
echo x | percU -a- -i1 -t@开开心心过每一天 x.exe
@rem the 2nd type is codepoints: 5F00 5F00 5FC3 5FC3 8FC7 6BCF 4E00 5929
@rem using percu8 above instead of percU results in identical x.exe (md5sum)

@rem this works fine and lists both types
percU -L x.exe

@rem this lists only the 1st type, and can't enumerate the 2nd
percu8 -L x.exe

the issue seems to be that rtype at the EnumTypeCb callback has an incorrect
utf8 string, and then EnumResourceNames(... rtype ...) errors.

Note that not all types with asian chars trigger this bug. rather, most
unicode types enumerate just fine with percu8 (ANSI + UTF-8 manifest).
*************************************************************************/
