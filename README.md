`ada_caser` is a tool to adjust the casing in an Ada source code to a "normal" form.

The normal casing form in Ada source is

* keywords are in lower case
* identifiers are in title case (`Title_Case`).

There are some exceptions to the normal rule: for example, `IO` in `Ada.Text_IO`, and `GNAT`.

### Usage

`ada_caser [switches] files`

| Switch | |
|------ |------ |
| `-v`, `--verbose` | Report progress |
| `-c`, `--charset=ARG` | Specify the character set (default "utf-8") |
| `-l`, `--language=ARG` | Specify the language version (default "Ada_2022") |
| `-p`, `--pipe` | Send the output to standard output |
| `-D`, `--dictionary=ARG` | Add a casing dictionary |
| `-P`, `--project=ARG` | Use a GNAT Project (default none) |
| `-V`, `--version` | Report the version |

### Processing with a GNAT Project

If a project file is supplied, `ada_caser` reads the project file, and the tree of referenced projects. It then uses Libadalang to find the declaration (if any) of each identifier in the input source code and replaces the source text with that of the declaration.

If no project file is supplied, `ada_caser` uses a default project which allows it to access the standard library (`Ada*`, `GNAT*`, `Interfaces*`).

So, for example,
```
package duration_io is new ada.text_io.fixed_io (duration);
```
will be replaced by
```
package duration_io is new Ada.Text_IO.Fixed_IO (Duration);
```
(`duration_io` isn't altered because this _is_ its declaration.)

If you're using `ada_caser` with a project in an Alire crate, you should run it in an `alr exec` context so that it can find all the referenced projects.

### Processing with dictionaries

For previously-declared identifiers, `ada_caser` will convert the identifier to the casing with which it was declared.

The default processing of undeclared identifiers is to convert the first character of each word to upper-case (reserved words are converted to lower-case, other tokens are left unchanged).

Considering `proj.regular_io`, `proj` is a single-word identifier and will be rendered as `Proj`, while `regular_io` is a two-word identifier which will be rendered as `Regular_Io`.

For identifiers, the default behaviour can be altered using exception dictionaries.

Exception dictionaries are line-oriented. Blank lines and comments (lines whose first non-blank character is `#`) are ignored. Other lines are exceptions; only the first word on a line is counted as an exception.

A casing exception can either be a full or a part exception.

* A full exception matches a whole identifier.
* A part exception matches any word in an identifier.

A part exception is marked by a leading `*`.

An exception dictonary containing
```
GNAT
*IO
```
would render `gnat.command_line` as `GNAT.Command_Line`, `gnat_support` as `Gnat_Support`, and `ada.text_io` as `Ada.Text_IO`. Note, this casing is in fact provided by `ada_caser` for standard library projects (which contain the `Ada` and `GNAT` package hierarchies), but probably not for embedded projects.

It is an error to provide differing exceptions, for example `Id` and `*ID`.

### Installation

#### Building

For the present, because of issues related to the use of aggregates in `Langkit_Support` ([GCC PR 104751](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=104751)), it's necessary to build using
```
alr build -- -gnatwJ
```

#### Installation

`alr install` rebuilds the crate without the opportunity to provide extra switches, as above. This means you have to install "by hand":
```
alr exec -- gprinstall -f -P ada_caser.gpr -p --prefix=$HOME/.alire
```

### Testing

Testing is done using Black Box Testing (`bbt`), available via Alire. See the [test scenarios](test.md). 

These scenarios demonstrate `ada_caser`'s features.

Run using
```
bbt --exact_match --cleanup test.md
```

