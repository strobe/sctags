This is a patched version of SCTags for another experemantal project.


About
-----

SCTags is ctags and etags extractor for Scala language. It will fallback
to ctags if language other than Scala was forced

Usage
-----

### As library

Add following to your build.sbt

```
libraryDependencies += "cc.evgeniy" %% "sctags" % "1.0.1"

resolvers += Resolver.bintrayRepo("strobe", "sbt-plugins")
```

### As CLI

```
sctags [-f|-o file] [-R|--recurse] [--list-languages] [--version] <files>

-f file | -o file
  sets the file the tags are written to, default is "tags"
  (Use "-" for standard output)

-R --recurse
  recurse through directories specified on the command line

--list-languages
  list the supported languages (inlcuding the ctags suported ones)

--language-force=lang
  it will fall back to ctags if the lang is not Scala

--version
  print fake version for exhuberant ctags compatibility
```

Build, Install and Running
-----------------
```
$ sbt assembly
$ cp target/scala-2.11/sctags ~/bin
```

The executable uncludes the compiled classes. It will look for
the scala libraries relatively to the scala executable that should
be on your $PATH. Other option is to set SCALA_HOME to the root
of your Scala distribution or SCALA_LIB to the lib directory of
SCALA_HOME.

Example Vim + TagBar config
---------------------------
```vim
function! SCTags()
    if executable("sctags")
        let g:tagbar_ctags_bin = "sctags"
        let g:tagbar_type_scala = {
            \ 'ctagstype' : 'scala',
            \ 'sro'       : '.',
            \ 'kinds'     : [
                \ 'p:packages',
                \ 'V:values',
                \ 'v:variables',
                \ 'T:types',
                \ 't:traits',
                \ 'o:objects',
                \ 'O:case objects',
                \ 'c:classes',
                \ 'C:case classes',
                \ 'm:methods:1'
            \ ],
            \ 'kind2scope'  : {
                \ 'p' : 'package',
                \ 'T' : 'type',
                \ 't' : 'trait',
                \ 'o' : 'object',
                \ 'O' : 'case_object',
                \ 'c' : 'class',
                \ 'C' : 'case_class',
                \ 'm' : 'method'
            \ },
            \ 'scope2kind'  : {
                \ 'package' : 'p',
                \ 'type' : 'T',
                \ 'trait' : 't',
                \ 'object' : 'o',
                \ 'case_object' : 'O',
                \ 'class' : 'c',
                \ 'case_class' : 'C',
                \ 'method' : 'm'
            \ }
        \ }
    endif
endfunction

if has("autocmd")
    autocmd FileType scala call SCTags()
endif
```

This will give you nice package/class/object/method hierarchy in Tagbar. Here is an example:

```
▼+types : package
  ▼+Membership : trait
     +C[T] : type
     +T : type
     +contains(a:C[T], b:T) : method
  ▼+BloomFilterMembership : class
     +T : type
     +<init>() : method
     +contains(a:BloomFilter[T], b:T) : m
  ▼+MinMaxMembership : class
     +T : type
     +<init>() : method
     +contains(a:MinMax[T], b:T) : metho
  ▼+SetMembership : class
     +T : type
     +<init>() : method
     +contains(a:Set[T], b:T) : method
  ▼+MembershipOps : object
    ▼+Contained : class
       +C[T] : type
       +T : type
        [values]
       -a
       -tc
       +<@(b:C[T]) : method
       +<init>(a:T)(tc:Membership[T, C])
```

Copyright
----------

This package is based on http://programmer-monk.net/darcs/repos/sctags

It is simplified and extended to support scope tracking.

Released under the Apache 2.0 License.

Copyright 2008 Geoff Reedy

Copyright 2015 Luben Karavelov
