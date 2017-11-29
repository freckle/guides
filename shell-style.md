# Shell Style

## Global Rules

1. All scripts should pass [Shellcheck][]

   [shellcheck]: https://www.shellcheck.net/

   Add explicit annotations as needed. When adding new scripts, confirm that our
   CI machinery is configured to run on your files.

1. All scripts end in a `.sh` extension

   This is typically an anti-pattern, but we currently rely on it to locate
   shell scripts within the source tree for the purposes of Shellcheck on CI.

1. Always use double-quotes, except where interpolation must be prevented

   NOTE: It's acceptable to use single-quotes as a matter of course for script
   arguments to other interpreters like `sed`, `awk`, and `psql` even if there
   would be no unintentional interpolations currently.

   **Tip**: quoting a here-doc terminator will prevent interpolation:

   ```sh
   cat <<EOM
   Uh-oh, I need to \$escape this
   EOM

   cat <<'EOM'
   But not in $this case.
   EOM
   ```

## Interpreter

1. Prefer Bash

   We write all of our scripts as Bash. It's available everywhere we need
   scripts to run and selecting an explicit shell will ensure consistent
   behavior.

1. Use `#!/usr/bin/env bash`, not `#!/bin/bash`

   This is specifically for OSX folks, where `/bin/bash` is severely out of
   date. A newer version, installed via Homebrew, may appear first in `$PATH`
   and should be used when possible.

### Bashisms

Though our scripts are using a Bash shebang, prefer POSIX alternatives where
possible. This is not about portability, rather it's to reduce to scope of
understanding required on the part of the reader by preferring simpler,
more-common, and well-defined constructs.

If you decide to use a Bashism, be consistent within that script. For example,
if you use `[[` because you want regex or glob matching in one place, don't use
`[` in other places in the same script.

## Shell Options

1. Use `set -eu`

   `-e` ensures that any unchecked, non-zero exit code halts the script. For
   cases where a non-zero exit is OK, explicitly append `|| true`.

   `-u` will halt the script if an unset variable is used.

   This avoids bugs like:

   ```sh
   dir=/tmp/foo
   rm -rf "$dirr/bar" # typo made this rm -rf /bar
   ```

2. Avoid `set -x`

   This option outputs every statement before it's executed. Only use this if
   your script has no function definitions, control flow structures, or
   conditional expressions; otherwise it is overly noisy.

   ```sh
   # Bad
   set -eux

   heyoh() { tar ... }

   if [ this ]; then
     curl ...
   fi

   heyoh || mv ...

   # OK
   set -eux

   curl ...
   tar ...
   mv ...
   ```

## Syntax

1. Use `;` to join `if`, `for` and `while` statements

   ```sh
   # Bad
   if
   then
   fi

   while
   do
   done

   for x
   do
   done

   # Good
   if; then
   fi

   while; do
   done

   for x; do
   done
   ```

1. Avoid redundant braces:

   ```sh
   # Bad
   touch "foo/${bar}/baz"

   # Good
   touch "foo/$bar/baz"

   # But required here
   touch "foo/${bar}_backup/bat
   ```

1. Avoid redundant quoting on assignment:

   ```sh
   # Bad, suprisingly
   foo="barbaz"
   foo="$bar$baz"

   # Good
   foo=barbaz
   foo=$bar$baz

   # But required for
   foo="bar baz"
   foo="$bar $baz"
   ```

## Best Practices

1. Scripts should have a header comment, formatted like so:

   ```sh
   #!/usr/bin/env bash
   #
   # Do the thing with the other thing. Assumes you've got this that and the
   # other present. Invoke with NO_FOOBAR=1 set to prevent a foobar.
   #
   set -eu

   ```

1. Scripts should validate their options and error with proper [usage
   syntax][usage-syntax] and conventional [exit code][exit-code]:

   [usage-syntax]: https://stackoverflow.com/a/9727046
   [exit-code]: https://www.freebsd.org/cgi/man.cgi?query=sysexits&apropos=0&sektion=0&manpath=FreeBSD%204.3-RELEASE&format=html

   ```sh
   if [ "$#" -lt 2 ]; then
     echo "usage: foo-bar count file [limit]" >&2
     exit 64
   fi
   ```

1. Scripts should be quiet when successful. Ideally the output of the commands
   invoked by the script stands on their own, without additional print
   statements. See the *Rule of Silence* [here][rules].

   [rules]: https://en.wikipedia.org/wiki/Unix_philosophy#Eric_Raymond.E2.80.99s_17_Unix_Rules

1. Scripts should output error information to `stderr` and use exit codes
   responsibly.

1. If a script uses `sudo` on all or most commands, consider assuming and
   requiring the script is itself run as root.

1. Scripts should be good Unix citizens and accept input on `stdin` and produce
   results to `stdout`. Be a filter. If you can't seem to do this because your
   operation is multi-stage or writes multiple inputs and outputs, consider
   writing more, smaller scripts that each do one thing.

   ```sh
   # Bad
   ./some-script -i input.txt -o output.txt

   # Good
   ./some-script < input.txt > output.txt
   ```

   ```sh
   # Bad
   ./some-script *.input.txt -o output.txt

   # Better
   ./some-script *.input.txt > output.txt

   # Best
   for x in *.input.txt; do
     ./some-script < "$x"
   done > output.txt
   ```

1. Define function-local variables with `local`

   ```sh
   # Bad
   foo() {
     bar=$1

     # ...
   }

   # Good
   foo() {
     local bar=$1

     # ...
   }
   ```

   **Tip**: perform multiple assignments in one statement:

   ```sh
   foo() {
     local bar=$1 baz=$2

     # ...
   }
   ```

1. Use `mktemp` and `trap EXIT` to ensure uniqueness and clean up of temporary
   resources:

   ```sh
   tmp=$(mktemp)
   trap 'rm $tmp' EXIT

   # use $tmp file
   ```

   ```sh
   tmp=$(mktemp -d)
   trap 'rm -f $tmp' EXIT

   # use $tmp directory
   ```

1. Only use `echo` for the simplest case of `echo "static string"`, otherwise
   use `printf`.

   `echo` with options, escapes, or variable interpolation is a portability
   nightmare. See [here][echo] as just one example reference.

   [echo]: http://wiki.bash-hackers.org/scripting/nonportable#echo_command

   ```sh
   # Bad
   echo -e "Results written to $output\nThanks!"

   # Good
   printf "Results written to %s\nThanks!\n" "$output"

   # Also good
   printf "Results written to %s\n" "$output"
   echo "Thanks!"
   ```

## Utilities

Shells are frequently glue code, pulling together other tools such as `grep` or
`sed`. These guides are about how to best use those tools.

1. Avoid any [Useless Use of Cat][cat]

   ```sh
   # Bad
   cat PATH | grep ...
   cat PATH | sed ...
   cat PATH | awk ...

   # Good
   grep ... PATH
   sed ... PATH
   awk ... PATH
   ```

   [cat]: http://porkmail.org/era/unix/award.html

1. Use `-F`/`-E`, not `fgrep`/`egrep`

1. Use `grep -F` if you don't require regex

1. Use `grep -x`, not `grep '^foo$'`

1. `grep re | awk '{ cmd }'` can be `awk '/re/ { cmd }'`

1. `grep re | sed 's/re/x/'` can be `sed '/re/!d; s//x/'` (notice the reuse of
   `re` through an empty `s//`)

1. `awk` is usually better than `cut` for splitting on whitespace; the former
   handles tab-separation better and has `$NF` available for "last"

## When Not To Use Shell

The following features should give you pause:

1. A line-count approaching 100
1. Complex options, use of `getopts` or similar
1. Bash arrays, especially associative arrays (i.e. hashes)
1. Multiple-levels of `source` dependency

Some alternatives:

1. Is this part of CI/Deployment? Try writing it in Shake
1. Is this called from something like `yarn`? Consider using js-specific tooling
1. Is the process infrequent and made simpler by hard-coding some information
   about the local environment (paths, passwords, etc)? Consider just
   documenting the command with dummy values to be copied, edited, and run
1. Is the process frequent, useful, and complex? Consider adding to FRTool or
   creating a standalone Haskell binary

## Additional References

- http://mywiki.wooledge.org/BashFAQ
- https://robots.thoughtbot.com/the-unix-shells-humble-if
