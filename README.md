# The not so great automatic Haskell licensor

[![][2]](https://www.stackage.org/lts/package/licensor)
[![][3]](https://www.stackage.org/nightly/package/licensor)

[2]: https://www.stackage.org/package/licensor/badge/lts
[3]: https://www.stackage.org/package/licensor/badge/nightly

Licensor is a program that generates a report of the dependencies and
transitive dependencies of a Haskell project and their licenses.

NOTE: Currently, [Stack](https://haskellstack.org/) and a `stack.yaml` file are
required.

## Description

Choosing a license for a software project or determining whether a
particular dependency can be added to a project can be projects
themselves. Unless starting from scratch, programmers should consider
the licenses of the dependencies and transitive dependencies of their
projects to make informed decisions and avoid license compatibility
issues.

Of course, this is just a starting point. "Beyond (...) general
observations, it is difficult, if not impossible, to provide precise
guidance about what licenses may or may not be compatible with each
other. (...) Programmers who are considering combining code governed
by two or more different licenses should proceed cautiously" (Andrew
M. St. Laurent).

## Disclaimer

Licensor is not a lawyer and does not provide legal advice.

For more information about licenses and license compatibility issues,
read the text of the licenses or consult with a lawyer before making
any decision.

## Related programs

Licensor is not the only license compatibility helper for Haskell:

- Licensor uses a Cabal library and Stack program approach for
  detecting licenses and listing dependencies, respectively. For a
  Cabal library and program approach, consider using
  the [cabal-dependency-licenses][rp-01] program or the [cabal-plan][rp-02]
  utility.

[rp-01]: https://hackage.haskell.org/package/cabal-dependency-licenses
[rp-02]: https://hackage.haskell.org/package/cabal-plan

## Installation and usage

To install Licensor, use Cabal:

```
$ cabal update && cabal install licensor
```

Then, run the `licensor` executable inside a Haskell project:

```
$ licensor
```

To see the license report for Licensor, clone the repository:

```
$ git clone https://github.com/jpvillaisaza/licensor
```

And run `licensor` inside the project:

```
$ cd licensor/ && licensor
```

Or build and run `licensor` inside the project:

```
$ cd licensor/ && stack build --exec licensor
```

To generate a report including license files for all dependencies,
use the `--licenses` option and redirect the output to a file:

```
$ licensor --quiet --licenses > licenses.md
```

See [licenses.md](licenses.md) for an example.

NOTE: This is currently using the Hackage API, which doesn't require
building any package, but could probably be improved to use the
installed packages if present.

For more information, run `licensor --help`:

```
usage: licensor
  -q, -s  --quiet, --silent  Quiet/silent mode
          --licenses         Fetch license files
  -h      --help             Display help message
  -V      --version          Print version information
```

## Notes

### Dependencies

Licensor uses the Stack program to list dependencies for a Haskell
project. A future enhancement could be to use the Stack library.

### Licenses and license detection

Licensor uses the Cabal library to detect the license of a Haskell
project and its dependencies (including transitive dependencies). To
do so, it uses the license field in the package description. A future
enhancement could be to use both the license and licence file fields
in the package description.

Cabal provides an enumeration of common open source and free software
licenses. These are the licenses that appear in the reports generated
by Licensor:

License                   | Description
------------------------- | -------------------------
GPL                       | GNU General Public License
AGPL                      | GNU Affero General Public License
LGPL                      | GNU Lesser General Public License
BSD2                      | BSD 2-Clause License
BSD3                      | BSD 3-Clause License
BSD4                      | BSD 4-Clause License
MIT                       | MIT License
ISC                       | ISC License
MPL                       | Mozilla Public License
Apache                    | Apache License
PublicDomain              | Public domain
AllRightsReserved         | All rights reserved
UnspecifiedLicense        | Unspecified license (All rights reserved)
OtherLicense              | Other license
UnknownLicense            | Unknown license

## Contribution guidelines

Feel free to create issues for reporting bugs and suggesting
enhancements, or to fork the repository and open a pull request.

## License

Licensor is licensed under the MIT License.
See [LICENSE.md](LICENSE.md).

### License report

Licensor (0.5.0) depends on the following libraries:

Library                   | License
------------------------- | -------------------------
Cabal                     | BSD-3
base                      | BSD-3
bytestring                | BSD-3
containers                | BSD-3
directory                 | BSD-3
http-client               | MIT
http-client-tls           | MIT
process                   | BSD-3
tar                       | BSD-3
temporary                 | BSD-3
zlib                      | BSD-3

And the following licenses (including transitive dependencies):

License                   | Number of libraries
------------------------- | -------------------------
BSD-2                     | 1
BSD-3                     | 22
MIT                       | 5

(Tested with Licensor 0.5.0, Stack 2.7.1, and LTS Haskell 17.13.)

## Additional resources

- [Choose a License](https://choosealicense.com/)
- [The Legal Side of Open Source](https://opensource.guide/legal/)
- [License compatibility][ar-01]
- [Understanding open source and free software licensing][ar-02]
  (Andrew M. St. Laurent)

[ar-01]: https://en.wikipedia.org/wiki/License_compatibility
[ar-02]: http://www.oreilly.com/openbook/osfreesoft/book/
