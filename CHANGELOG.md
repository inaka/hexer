# Change Log

## [0.2.0](https://github.com/inaka/hexer/tree/0.2.0) (2016-01-28)
[Full Changelog](https://github.com/inaka/hexer/compare/stable...0.2.0)

**Fixed bugs:**

- Bad formatting for requirements [\#38](https://github.com/inaka/hexer/issues/38)
- Packages should only be published in hex.pm if all their dependencies are also published there [\#36](https://github.com/inaka/hexer/issues/36)
- Error Code 500 when hexer try to publish jiffy repository [\#27](https://github.com/inaka/hexer/issues/27)

**Closed issues:**

- Add support for docs [\#32](https://github.com/inaka/hexer/issues/32)
- HTTP 400 error when using `{vsn, git}` in src/appname.app.src [\#31](https://github.com/inaka/hexer/issues/31)

**Merged pull requests:**

- Fixed bad formatting for requirements [\#40](https://github.com/inaka/hexer/pull/40) ([davecaos](https://github.com/davecaos))
- \[\#36\] Added hex only for dependencies in Makefile DEPS list [\#39](https://github.com/inaka/hexer/pull/39) ([davecaos](https://github.com/davecaos))
- \[\#32\] Added publish.docs to hexer [\#37](https://github.com/inaka/hexer/pull/37) ([davecaos](https://github.com/davecaos))
- \[\#31\] Added transform\_vsn\_git\_to\_tag/1 for {vsn, git} case [\#34](https://github.com/inaka/hexer/pull/34) ([davecaos](https://github.com/davecaos))

## [stable](https://github.com/inaka/hexer/tree/stable) (2016-01-11)
[Full Changelog](https://github.com/inaka/hexer/compare/0.1.0...stable)

## [0.1.0](https://github.com/inaka/hexer/tree/0.1.0) (2016-01-11)
[Full Changelog](https://github.com/inaka/hexer/compare/0.0.1...0.1.0)

**Merged pull requests:**

- Version Bump to 0.1.0 [\#30](https://github.com/inaka/hexer/pull/30) ([elbrujohalcon](https://github.com/elbrujohalcon))
- 'files' in metadata should be a list of binaries [\#29](https://github.com/inaka/hexer/pull/29) ([elbrujohalcon](https://github.com/elbrujohalcon))

## [0.0.1](https://github.com/inaka/hexer/tree/0.0.1) (2016-01-08)
**Implemented enhancements:**

- Add a feature to change pkg\_name value from the \*.app.src to upload package with different names  [\#16](https://github.com/inaka/hexer/issues/16)

**Fixed bugs:**

- Hexer is not uploading Emakefiles to hex [\#22](https://github.com/inaka/hexer/issues/22)
- TEST and SHELL DEPS should be ignored [\#24](https://github.com/inaka/hexer/pull/24) ([elbrujohalcon](https://github.com/elbrujohalcon))

**Closed issues:**

- Change the default value of build\_tools field from to other more generic [\#19](https://github.com/inaka/hexer/issues/19)
- Change the value of build\_tools field from \[\<\<"make"\>\>\] to other more generic [\#18](https://github.com/inaka/hexer/issues/18)
- Add escript binary to repo so it can be downloaded by hexer.mk [\#13](https://github.com/inaka/hexer/issues/13)
- Update README with details on how to publish your erlang.mk packages [\#12](https://github.com/inaka/hexer/issues/12)
- Command: publish package [\#7](https://github.com/inaka/hexer/issues/7)
- Command: register new user [\#6](https://github.com/inaka/hexer/issues/6)
- Command: generate API key [\#5](https://github.com/inaka/hexer/issues/5)
- Erlang.mk 3rd-party plug-in [\#4](https://github.com/inaka/hexer/issues/4)
- Fulfil the open-source checklist [\#3](https://github.com/inaka/hexer/issues/3)
- Initial Project Setup [\#2](https://github.com/inaka/hexer/issues/2)

**Merged pull requests:**

- Publish this project on hex.pm [\#26](https://github.com/inaka/hexer/pull/26) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#24\] Collateral steps to make this project similar to the others at … [\#25](https://github.com/inaka/hexer/pull/25) ([elbrujohalcon](https://github.com/elbrujohalcon))
- \[\#22\] Added Emakefile to files to upload default list [\#23](https://github.com/inaka/hexer/pull/23) ([davecaos](https://github.com/davecaos))
- Package name fix [\#21](https://github.com/inaka/hexer/pull/21) ([davecaos](https://github.com/davecaos))
- \[\#19\] Added build\_tools field settings [\#20](https://github.com/inaka/hexer/pull/20) ([davecaos](https://github.com/davecaos))
- \[\#16\] Added pkg\_name value to set package names [\#17](https://github.com/inaka/hexer/pull/17) ([davecaos](https://github.com/davecaos))
- \[Closes \#12\] Update readme [\#15](https://github.com/inaka/hexer/pull/15) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#13\] Add escript binary [\#14](https://github.com/inaka/hexer/pull/14) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#7\] Command: publish package [\#11](https://github.com/inaka/hexer/pull/11) ([jfacorro](https://github.com/jfacorro))
- \[\#6\] Register new user [\#10](https://github.com/inaka/hexer/pull/10) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#5\] Generate API key [\#9](https://github.com/inaka/hexer/pull/9) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#2\] Initial setup [\#8](https://github.com/inaka/hexer/pull/8) ([jfacorro](https://github.com/jfacorro))
- Update LICENSE [\#1](https://github.com/inaka/hexer/pull/1) ([elbrujohalcon](https://github.com/elbrujohalcon))



\* *This Change Log was automatically generated by [github_changelog_generator](https://github.com/skywinder/Github-Changelog-Generator)*