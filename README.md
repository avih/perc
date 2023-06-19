# perc
Command line PE resource utility:
Add, replace, delete, extract or list resources in Windows PE file

`perc` uses Windows API to access, enumerate or modify resources in a PE file.

Pre-built 32/64 Windows binaries are available at the
[releases page](https://github.com/avih/perc/releases).

### Examples

List all resources in `example.dll`:
```sh
perc -L example.dll
```

Add a manifest resource file `manifest.xml` as ID `1` with default
(neutral) language to file `example.exe`:
```sh
perc -a manifest.xml -t MANIFEST -i 1 example.exe
```

Extract the manifest with neutral language and ID `1` from `example.exe` to
`manifest.xml`:
```sh
perc -x -t MANIFEST -i 1 example.exe > manifest.xml
```

### Usage:
```
Usage: perc.exe  -h  |  -{D|L|R|a:|d|x} [-{t:i:l:}] [INDEX] PE
Add, replace, delete, extract or list resources in Windows PE file.

  Actions - exactly one must be specified, possibly with -n:
    -h       Print this help and exit.
    -n       No-write: don't modify PE, don't write list or extracted resource.
    -D       Delete all (*) resources in PE.
    -L       List all (*) resources in PE as  INDEX: TYPE/ID/LANG (SIZE).
    -R       Same as -L, but TYPE is printed as raw integer, if possible.
    -d       Delete the resource TYPE/ID/LANG in PE.
    -x       Extract the resource TYPE/ID/LANG in PE to stdout (**).
    -a RSRC  Add resource file RSRC (stdin if '-') as TYPE/ID/LANG to PE (**).
  (*)  If any of TYPE, LANG, ID are given: only items with all given values.
  (**) Raw data directly from/to PE. E.g. ICON is without header, etc.

  Target - at most one TYPE, one ID, and one LANG (INDEX overrides others):
    -t TYPE  Symbol/int/@STR resource type. E.g. -t ICON, -t 2, -t @"foo bar".
    -i ID    Int/@STR resource ID/NAME.
    -l LANG  Int resource language. Default for -a/-x/-d is 0 (neutral).
    INDEX    Sets TYPE+ID+LANG of existing resource INDEX (from -L/-R output).

perc v0.1  PE Resource utility  https://github.com/avih/perc
```
