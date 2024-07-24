#lang scribble/manual
@require[@for-label[soup-lib
                    (except-in racket/base do) racket/contract racket/control racket/function racket/mutability racket/undefined
                    json]]

@title{Soup: A library of useful routines}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

A collection of useful functions not important enough to spin off into their own packages.

@section{Top level interface}

@defmodule[soup-lib]

Provides all the functions exported by the modules below.

@section{Additional functions for paths and files}

@defmodule[soup-lib/files]

@defproc[(make-directory* [dirname path-string?] [permissions (integer-in 0 65535) #o777]) boolean?]{

 Create a directory without raising an error if it can't be.

 Returns true if the directory was created, false on failure.

}

@defproc[(delete-file* [filename path-string?]) boolean?]{

 Deletes a file without raising an error if it can't be.

 Returns true if the file was deleted, false on failure.

}

@defproc[(delete-directory* [dirname path-string?]) boolean?]{

 Deletes a directory without raising an error if it can't be.

 Returns true if the directory was deleted, false on failure.

}

@defproc[(directory-tree [dirname (and/c path-string? directory-exists?)] [#:follow-links? follow-links? any/c #t])
         (listof (flat-rec-contract entry path? (cons/c path? (listof entry))))]{

 Scans the given directory and returns a list that's the root of a tree structure of its contents. Each element is either a list where the first entry is the name of a
 subdirectory and the rest of the list that directory's contents, or paths of non-directory files. If @tt{#:follow-links?} is true, symbolic links to directories are
 traversed, if false, just the name of the link is included.

}

@section{Additional @code{for} and @code{for*} style comprehensions}

@defmodule[soup-lib/for]

@subsection{for loops}

@defform[(for/max (for-clauses ...) body-or-break ... body)]{

 @code{body} must evaluate to a single real number at each iteration, the maximum of which is returned.

}

@defform[(for*/max (for-clauses) body-or-break ... body)]{

 @code{body} must evaluate to a single real number at each iteration, the maximum of which is returned.

}

@defform[(for/min (for-clauses) body-or-break ... body)]{

 @code{body} must evaluate to a single real number at each iteration, the minimum of which is returned.

}

@defform[(for*/min (for-clauses) body-or-break ... body)]{

 @code{body} must evaluate to a single real number at each iteration, the minimum of which is returned.

}


@defform[(for/string maybe-length (for-clauses) body-or-break ... body)
         #:grammar [(maybe-length (code:line) (code:line #:length len))]
         #:contracts [(len exact-nonnegative-integer?)]]{

@code{body} must evaluate to a string or character at each iteration, all of which are returned as a single string.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.

}

@defform[(for*/string (for-clauses) body-or-break ... body)
         #:grammar [(maybe-length (code:line) (code:line #:length len))]
         #:contracts [(len exact-nonnegative-integer?)]]{

@code{body} must evaluate to a string or character at each iteration, all of which are returned as a single string.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.

}

@defform[(for/bytes (for-clauses) body-or-break ... body)
         #:grammar [(maybe-length (code:line) (code:line #:length len))]
         #:contracts [(len exact-nonnegative-integer?)]]{

@code{body} must evaluate to a single byte at each iteration, all of which are returned as a bytestring.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.

}

@defform[(for*/bytes (for-clauses) body-or-break ... body)
         #:grammar [(maybe-length (code:line) (code:line #:length len))]
         #:contracts [(len exact-nonnegative-integer?)]]{

@code{body} must evaluate to a single byte at each iteration, all of which are returned as a bytestring.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.

}

@defform[(for/list/mv (for-clauses) body-or-break ... body)]{

 Like @code{for/list}, but the body can return multiple values, all of which are added to the result list.

}

@defform[(for*/list/mv (for-clauses) body-or-break ... body)]{

 Like @code{for*/list}, but the body can return multiple values, all of which are added to the result list.

}

@defform[(for/count (for-clause ...) body-or-break ... body)]{

Returns the number of times @code{body} evaluates to a true value.

}

@defform[(for*/count (for-clause ...) body-or-break ... body)]{

Returns the number of times @code{body} evaluates to a true value.

}

@subsection{Sequences}

@defproc[(in-char-range [start char?] [end char?]) sequence?]{

Returns a sequence that iterates over the given (inclusive) range of characters. Ascending if the first character is @code{char<=?} the second, descending if @code{char>?}.

}

@defproc[(in-conses [list list?]) sequence?]{

 Returns a sequence that iterates over the successive pairs of the given list (Instead of the values stored in the cars like @code{in-list}).

}

@section{Hash Table functions}

@defmodule[soup-lib/hash]

@defproc[(hash->vector [htab hash?]) (vectorof pair?)]{

 Returns a vector holding the contents of the hash table as key-value pairs in an unspecified order.

}

@defproc[(hash-keys/vector [htab hash?]) vector?]{

 Returns a vector holding the keys of the hash table in an unspecified order.

}

@defproc[(hash-values/vector [htab hash?]) vector?]{

 Returns a vector holding the values of the hash table in an unspecified order.

}

@defproc[(hash->immutable-hash [htab hash?]) immutable-hash?]{

 Returns an immutable version of @code{htab} (If @code{htab} is already immutable, it's returned) using the same
 underlying comparison routine as the original.

}

@section{List-related functions}

@defmodule[soup-lib/list]

@defproc[(lmax [list list?] [< (-> any/c any/c any/c) <]) any/c]{

 Returns the maximum element of the list per the given comparision function.

}

@defproc[(lmin [list list?] [< (-> any/c any/c any/c) <]) any/c]{

 Returns the minimum element of the list per the given comparison function.

}

@defproc[(chunk [list list?] [nsublists exact-positive-integer?]) (listof list?)]{

 Split a list up into @code{nsublists} lists, all but possibly the last equal in size to each other, and return them all in a list.

}

@defproc[(slice [list list?] [size exact-positive-integer?]) (listof list?)]{

 Split a list up into lists @code{size} elements long and return them all in a list.

}

@subsection{Common Lisp functions}

Stuff from the @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/c_conses.htm"]{cons dictionary} without Racket or SRFI-1 equivalents.
as well as the @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md"]{Serapeum library} and others. Sometimes with better names.

@defproc[(subst [new any/c] [old any/c] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst}}.

}

@defproc[(subst-if [new any/c] [pred? (-> any/c any/c)] [tree any/c] [#:key key (-> any/c any/c) identity]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst-if}}.

}

@defproc[(adjoin [elem any/c] [list list?] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) list?]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_adjoin.htm"]{Common Lisp @racket{adjoin}}.

}

@defproc[(maplist [proc procedure?] [list list?] ...+) list?]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm"]{Common Lisp @racket{maplist}}.

}

@defproc[(append-maplist [proc procedure?] [list list?] ...+) list?]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm"]{Common Lisp @racket{mapcon}}.

}

@defproc[(tail? [obj any/c] [list (or/c pair? null?)]) boolean?]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_ldiffc.htm"]{Common Lisp @racket{tailp}}.

}

@defproc[(ldiff [list (or/c pair? null?)] [obj any/c] ) (or/c pair? null?)]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_ldiffc.htm"]{Common Lisp @racket{ldiff}}.

}

@defproc[(copy-tree [tree any/c]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_cp_tre.htm"]{Common Lisp @racket{copy-tree}}.

}

@defproc[(tree-equal? [tree1 any/c] [tree2 any/c] [#:test test (-> any/c any/c any/c) eqv?]) boolean?]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_tree_e.htm"]{Common Lisp @racket{tree-equal}}.

}

@defproc[(reuse-cons [x any/c] [y any/c] [x-y pair?]) pair?]{

 See @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#reuse-cons-x-y-x-y"]{Serapeum @racket{reuse-cons}}.

}

@defform[(collecting body ...)]{

 The classic Lisp @racket{collecting} macro. Executes the body and returns a list made up of the values passed to @code{collect} in it.

}

@defproc*[([(collect) list?]
           [(collect [value any/c] ...+) void?])]{

 Only usable inside @code{collecting}; appends the values to that macros' result list. When called without any arguments, returns a list of the
 currently collected values.

}

@defform[(with-collector (collector) body ...)]{

From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#with-collector-collector-body-body"]{Serapeum}.

 Like @code{collecting} but allows a user-defined name instead of @code{collect}.

}

@defform[(with-collectors (collector ...) body ...)]{

From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#with-collectors-rest-collectors-body-body"]{Serapeum}.

 Like @code{with-collector} but allows multiple different collectors. Returns one value per collector.

}

@subsection{Association List functions}

@defproc[(sublis [alist (listof pair?)] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_sublis.htm"]{Common Lisp @racket{sublis}}.

}

@defproc[(rassoc [item any/c] [alist (listof pair?)] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) (or/c pair? #f)]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm"]{Common Lisp @racket{rassoc}}.

}

@defproc[(rassoc-if [pred? (-> any/c any/c)] [alist (listof pair?)] [#:key key (-> any/c any/c) identity]) (or/c pair? #f)]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm"]{Common Lisp @racket{rassoc-if}}.

}

@defproc[(pairlis [keys list?] [values list?] [alist any/c '()]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_pairli.htm"]{Common Lisp @racket{pairlis}}.

}

@defproc[(alist-map [proc (-> any/c any/c any/c)] [alist (listof pair?)]) (listof pair?)]{

 From the @hyperlink["https://people.csail.mit.edu/jaffer/slib/Association-Lists.html"]{SLIB association list library}.

 Returns a new association list formed by mapping @code{proc} over the keys and values of @code{alist}. @code{proc} must be a function of 2 arguments which returns
 the new value part.

}

@defproc[(alist-for-each [proc (-> any/c any/c any)] [alist (listof pair?)]) void?]{

 From the @hyperlink["https://people.csail.mit.edu/jaffer/slib/Association-Lists.html"]{SLIB association list library}.

 Applies @code{proc} to each pair of keys and values of @code{alist}.

}

@section{Tree functions}

Functions for working on trees made of @code{cons} cells; mostly taken from Common Lisp. A few of these functions are also provided by @code{soup-lib/list}.

@defmodule[soup-lib/tree]

@defproc[(copy-tree [tree any/c]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_cp_tre.htm"]{Common Lisp @racket{copy-tree}}.

}

@defproc[(tree-equal? [tree1 any/c] [tree2 any/c] [#:test test (-> any/c any/c any/c) eqv?]) boolean?]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_tree_e.htm"]{Common Lisp @racket{tree-equal}}.

}

@defproc[(subst [new any/c] [old any/c] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst}}.

}

@defproc[(subst-if [new any/c] [pred? (-> any/c any/c)] [tree any/c] [#:key key (-> any/c any/c) identity]) any/c]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst-if}}.

}

@defproc[(walk-tree [fun (-> any/c any)] [tree any/c] [#:tag tag (or/c continuation-prompt-tag? #f) #f]
                    [#:traversal traversal (or/c 'preorder 'inorder 'postorder) 'preorder]) void?]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#walk-tree-fun-tree-key-tag-traversal"]{Serapeum}.

 Call @code{fun} in turn over each atom and cons of @code{tree}.

 @code{fun} can skip the current subtree with @code{(abort/cc tag '())}.

 }

@defproc[(map-tree [fun (-> any/c any/c)] [tree any/c] [#:tag tag (or/c continuation-prompt-tag? #f) #f]
                   [#:traversal traversal (or/c 'preorder 'inorder 'postorder) 'preorder]) any/c]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#map-tree-fun-tree-key-tag-traversal"]{Serapeum}.

 Walk @code{fun} over @code{tree} and build a tree from the results.

The new tree may share structure with the old tree.

@codeblock{
  (eq? tree (map-tree identity tree)) ; #t
 }

@code{fun} can skip the current subtree with @code{(abort/cc tag subtree)}, in which case @code{subtree} will be used as the value of the subtree.

}

@defproc[(leaf-walk [fun (-> any/c any)] [tree any/c]) void?]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#leaf-walk-fun-tree"]{Serapeum}.

 Call @code{fun} on each leaf of @code{tree}.

}

@defproc[(leaf-map [fun (-> any/c any/c)] [tree any/c]) any/c]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#leaf-map-fn-tree"]{Serapeum}.

 Call @code{fun} on each leaf of @code{tree}. Return a new tree possibly sharing structure with @code{tree}.

}

@defproc[(occurs-if [test (-> any/c any/c)] [tree any/c] [#:key key (-> any/c any/c) identity]
                     [#:traversal traversal (or/c 'preorder 'inorder 'postorder) 'preorder]) (values any/c boolean?)]{

From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#occurs-if-test-tree-key-key-traversal"]{Serapeum}.

Is there a node (leaf or cons) in @code{tree} that satisfies @code{test}?

 Returns two values - the node that matched (Or @code{undefined} if none did) and a boolean indicating if the node was found or not.

}

@defproc[(occurs [node any/c] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]
                 [#:traversal traversal (or/c 'preorder 'inorder 'postorder) 'preorder]) (values any/c boolean?)]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#occurs-node-tree-key-key-test-traversal"]{Serapeum}.

 Is @code{node} present in @code{tree}?

 Returns two values - the node that matched (Or @code{undefined} if none did) and a boolean indicating if the node was found or not.

}

@defproc[(prune-if [test (-> any/c any/c)] [tree list?] [#:key key (-> any/c any/c) identity]) list?]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#prune-if-test-tree-key-key"]{Serapeum}.

 Remove any atoms satisfying @code{test} from @code{tree}.

 Pruning is defined "modulo flatten": you should get the same result from pruning, and then flattening, that you would get from flattening, and then filtering.

 Also note that pruning is not defined for trees containing improper lists.

}

@defproc[(prune [leaf any/c] [tree list?] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) list?]{

 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#prune-leaf-tree-key-key-test"]{Serapeum}.

 Remove @code{leaf} from @code{tree} wherever it occurs. See @code{prune-if} for more information.

}

@section{String functions}

@defmodule[soup-lib/string]

@defproc[(string-join/vector [strs (vectorof string?)] [sep string? " "]) string?]{

 Like @code{string-join} but takes a vector of strings instead of a list of strings.

}

@defproc[(string->vector [s string?]) (vectorof char?)]{

 Like @code{string->list} but returns a vector instead.

}

@defproc[(vector->string [vc (vectorof char?)]) string?]{

 Like @code{list->string} but takes a vector instead.

}

@defproc[(string-sort [s string?] [<? (-> char? char? any/c) char<?]) string?]{

 Returns a copy of @code{s} with its characters sorted according to @code{<?}.

}

@defproc[(string-sort! [s mutable-string?] [<? (-> char? char? any/c) char<?]) void?]{

 Sorts @code{s} in-place.

}

@defproc[(string-escape [s string?] [mapper (or/c dict? (-> char? (or/c string? #f)))] [start exact-nonnegative-integer? 0] [stop exact-nonnegative-integer? (string-length s)])
         string?]{

Inspired by @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#escape-string-table-key-start-end-stream"]{Serapeum's @racket{escape}}, replaces
characters in @code{s} with the string that they map to in the dictionary @code{mapper} (Or what it returns if a function).

}


@section{JSON functions}

@defmodule[soup-lib/json]

@defproc[(->jsexpr [js (or/c jsexpr? struct->jsexpr?)]) jsexpr?]{

When called with a value that's already a @code{jsexpr?}, return it. Otherwise, if called on a struct that implements
the @code{gen:struct->jsexpr} generic interface, call that to convert it to a @code{jsexpr?}.

}

@defproc[(struct->jsexpr? [obj any/c]) boolean?]{

Tests if an object is a struct that implements the @code{gen:struct->jsexpr} interface.

}

@defidform[gen:struct->jsexpr]{

 A generic interface that supplies the @code{->jsexpr} method to convert a struct to a @code{jsepxr?} value for use with JSON modules.

   @codeblock{
             (struct example (foo bar)
               #:methods gen:struct->jsexpr
               ([define (->jsexpr ex) (hasheq 'foo (example-foo ex) 'bar (example-bar ex))]))
             (->jsexpr (example 1 "cat"))
 }

}

@defform[#:literals (string number array object true false boolean null else)
(json-match maybe-unsafe jsexpr match-clause ... maybe-else)
#:grammar
[(maybe-unsafe (code:line) #:unsafe)
 (match-clause
   (code:line (number expr ...+))
   (code:line ((number id) expr ...+))
   (code:line (string expr ...+))
   (code:line ((string id) expr ...+))
   (code:line (array expr ...+))
   (code:line ((array id) expr ...+))
   (code:line (object expr ...+))
   (code:line ((object id) expr ...+))
   (code:line (null expr ...+))
   (code:line boolean-clause))
 (boolean-clause
   (code:line exact-boolean-clause ...)
   (code:line (boolean expr ...+))
   (code:line ((boolean id) expr ...+)))
 (exact-boolean-clause
   (code:line (true expr ...+))
   (code:line (false expr ...+)))
 (maybe-else
   (code:line)
   (else expr ...+))]
#:contracts
([jsexpr jsexpr?])
         ]{

Conditional evaluation based on the type of a jsexpr value, possibly binding the value to the given identifier.
Only one of each particular type can be present. If the type of the given @code{jsexpr} is not present
and there is no @code{else} clause, an error is raised.

If the optional @code{#:unsafe} keyword is given, no check is done to make sure @code{jsexpr} is actually a @code{jsexpr?}.

Example:

@codeblock{
           (json-match "foo"
             (number 'num)
             (array 'arr)
             (object 'obj)
             (string 'str)
             (else 'other)) ; 'str
 }

}

@section{Parameter extensions}

@defmodule[soup-lib/parameter]

@defform*[((define-parameter id initial-value)
           (define-parameter id initial-value guard)
           (define-parameter id initial-value guard name))
          #:contracts ([id identifier?]
                       [initial-value any/c]
                       [guard (or/c (-> any/c any/c) #f)]
                       [name symbol?])]{

 Define a parameter with the given name and value.

 The optional @code{guard} and @code{name} arguments are as in @code{make-parameter}.
 The default name is @code{id} instead of @code{'parameter-procedure}, though.

}

@defform*[((define-boolean-parameter id)
           (define-boolean-parameter id initial-value)
           (define-boolean-parameter id initial-value name))
          #:contracts ([id identifier?]
                       [initial-value boolean?]
                       [name symbol?])]{

 Define a boolean parameter with the given name. The default initial value if not given is @code{#t}. Any value can be used to set the parameter,
 but it's converted to a boolean; in other words it's compatible with the contract @code{(parameter/c any/c boolean?)}.

 The optional @code{name} argument is as in @code{make-parameter}. The default name is @code{id} instead of @code{'parameter-procedure}, though.

}

@section{Math functions}

@defmodule[soup-lib/math]

Written in Typed Racket.

@defproc[(integer-log [base exact-integer?] [num exact-integer?]) exact-integer?]{

 Computes the integer logarithm base @code{base} of @code{num}. Uses Oleg Kiselyov's
 @hyperlink["https://okmij.org/ftp/Haskell/AlgorithmsH1.html#basewidth"]{fast algorithm} ported from Haskell to get good performance on even very very large numbers.

 }

@section{Control functions}

@defmodule[soup-lib/control]

@defform[(let/comp maybe-prompt k body ...+)
         #:grammar
         [(maybe-prompt (code:line) (code:line #:prompt prompt-tag))]
         #:contracts ([prompt-tag continuation-prompt-tag?])

         ]{

 Has the same relation to @code{call-with-composable-continuation} that @code{let/cc} has to @code{call-with-current-continuation}.

}

@defform[(named-let-values name ([(id ...) producer] ...) body ...+)
         #:contracts
         [(name identifier?)
          (id identifier?)]]{

Like a named @code{let}, but the initial values for @code{id}s are obtained from the values returned by evaluating @code{producer}s.
             When recursing in the body, arguments correspond to @code{id}s going left to right top to bottom.

 Example:

 @codeblock{
  (named-let-values loop ([(a b) (values 1 2)]) (if (= b 10) a (loop (+ a b) (+ b 1))))
  }
}

@defform[(named-lambda (name kw-formals ...) body ...+)
         #:contracts
         [(name identifier?)]]{

 From R2RS; equivalent to

 @codeblock{
  (letrec ((name (lambda (kw-formals ...) body ...))) name)
 }

 Also see @code{rec} from @hyperlink["https://srfi.schemers.org/srfi-31/srfi-31.html"]{SRFI-31}.

}

@subsection{Common Lisp forms}

Things taken from Common Lisp. Implicit blocks are supported, implicit tagbodies are not.


@defform[(lret ([id init] ...) body ...)]{

 Like @code{let}, but it returns the values of the bindings after executing the body.
 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#lret-rest-bindings-body-body"]{Serapeum}.

}

@defform[(lret* ([id init] ...) body ...)]{

 Like @code{let*}, but it returns the values of the bindings after executing the body.
 From @hyperlink["https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#lret-rest-bindings-body-body-1"]{Serapeum}.

}

@defform[(if-let ([id init] ...) true-case false-case)]{

 First binds values to @code{id}s like @code{let}, and if all are truthy, executes @code{true-case}, otherwise @code{false-case}.
 All bindings are visible in both cases. From @hyperlink["https://alexandria.common-lisp.dev/draft/alexandria.html#index-if_002dlet"]{Alexandria}.

}

@defform[(when-let ([id init] ...) body ...+)]{

 First binds values to @code{id}s like @code{let}, and if all are truthy, executes @code{body}. Returns the value(s) of the last expression in @code{body}, or @code{void}. From @hyperlink["https://alexandria.common-lisp.dev/draft/alexandria.html#index-when_002dlet"]{Alexandria}.

}

@defform[(block name body ...)
         #:contracts
         [(name identifier?)]]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/s_block.htm"]{Common Lisp @code{block}}.

}

@defform*[((return) (return result))]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_return.htm#return"]{Common Lisp @code{return}}.

}

@defform*[((return-from name) (return-from name result))
          #:contracts
          [(name identifier?)]]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/s_ret_fr.htm#return-from"]{Common Lisp @code{return-from}}.

}

@defform[(do ((var init incr) ...) (end-case result ...) body ...)]{

 Basically, normal Racket @code{do} extended to be in a @code{block} that can be @code{return}ed from early.
 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm"]{Common Lisp @code{do}} for more.

}

@defform[(do* ((var init incr) ...) (end-case result ...) body ...)]{

 Like @code{do} but with @code{let*} style scoping.
 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm"]{Common Lisp @code{do*}} for more.

}

@defform[(dotimes (var count-form maybe-result) body ...)
         #:grammar
         [(maybe-result (code:line) (code:line expr))]
         #:contracts
         [(var identifier?)]]{


 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_dotime.htm"]{Common Lisp @code{dotimes}}.

}

@defform[(dolist (var list-form maybe-result) body ...)
         #:grammar
         [(maybe-result (code:line) (code:line expr))]
         #:contracts
         [(var identifier?)]]{

 See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_dolist.htm"]{Common Lisp @code{dolist}}.

}

@defform[(prog (variable-declaration ...) body ...)
         #:grammar
         [(variable-declaration (code:line var) (code:line (var)) (code:line (var init-form)))]
         #:contracts
         [(var identifier?)]]{


See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_prog_.htm"]{Common Lisp @code{prog}}.

}

@defform[(prog* (variable-declaration ...) body ...)
         #:grammar
         [(variable-declaration (code:line var) (code:line (var)) (code:line (var init-form)))]
         #:contracts
         [(var identifier?)]]{


See @hyperlink["https://www.lispworks.com/documentation/HyperSpec/Body/m_prog_.htm"]{Common Lisp @code{prog*}}.

}


@section{Vector functions}

@defmodule[soup-lib/vector]

@defproc[(vector-map->list [f (-> any/c any/c)] [vec vector?] [stsart exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (vector-length vec)]) vector?]{

Like @code{vector-map}, but returns a list instead of a vector.

}

@defproc[(vector-shuffle [vec vector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (vector-length vec)]) vector?]{

 Returns a newly allocated copy of the given range of @code{vec}, shuffled in random order.

}

@defproc[(vector-shuffle! [vec mutable-vector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (vector-length vec)]) void?]{

 Shuffles the given range of the mutable vector in-place.

}

@defproc[(fxvector-sort! [vec fxvector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (fxvector-length vec)]) void?]{

 Sorts the given range of a fxvector inplace, in ascending order.


}

@defproc[(fxvector-sort [vec fxvector?] [start exact-nonnegative-integer? 0] [end exact-nonnegative-integer? (fxvector-length vec)]) fxvector?]{

 Returns a newly allocated copy of the given range of the fxvector, sorted in ascending order.

}

@section{I/O functions}

Many of these were inspired by the Common Lisp @hyperlink["https://quickref.common-lisp.net/uiop.html#The-uiop_002fstream-package"]{UIOP stream package}.

@defmodule[soup-lib/port]

@defproc[(call-with-input [input (or/c input-port? string? bytes? path? boolean?)] [proc (-> input-port? any)] [#:mode mode (or/c 'text 'binary) 'binary]) any]{

 Opens an input port based on the @code{input} argument's value and passes it to @code{proc}. Strings use string ports, byte strings byte ports, paths file ports,
 input ports are passed through, and a boolean uses the value of @code{(current-input-port)}. Returns what @code{proc} returns.

}

@defproc[(call-with-output [output (or/c output-port? path-string? boolean?)] [proc (-> output-port? any)]
                           [#:mode mode (or/c 'binary 'text) 'binary]
                           [#:exists exists symbol? 'error]
                           [#:permissions permissions (integer-in 0 65535) #o666]
                           [#:replace-permissions? replace-permissions? any/c #f]) any]{

Opens an output port based on the @code{output} argument's value and passes it to @code{proc}.
If given a path or string, treats it as a filename and passes the keyword arguments on to @code{open-output-file}.
A @code{#t} uses the value of @code{(current-output-port)}. A @code{#f} passes an output string port. Returns what @code{proc} returns except with a @code{#f} @code{output} value,
where it returns the string written to.

}

@defproc[(call-with-null-input [proc (-> input-port? any)]) any]{

Calls @code{proc} with an input stream that triggers end of file on any attempts to read from it. Returns what @code{proc} returns.

}

@defproc[(with-input-from-null [thunk (-> any)]) any]{

 Calls @code{thunk} with @code{current-input-port} set to a port that triggers end of file on any attempts to read from it. Returns what @code{thunk} returns.

}

@defproc[(call-with-null-output [proc (-> output-port? any)]) any]{

Calls @code{proc} with an output stream that discards anything written to it. Returns what @code{proc} returns.

}

@defproc[(with-output-to-null [thunk (-> any)]) any]{

 Calls @code{thunk} with @code{current-output-port} set to a port that discards anything written to it. Returns what @code{thunk} returns.

}

@defform[(with-input-file (var filename maybe-mode) body ...+)
         #:grammar [(maybe-mode (code:line) (code:line #:mode mode))]
         #:contracts
         [(var identifier?)
          (filename path-string?)
          (mode (or/c 'text 'binary))]]{

Binds @code{var} to an input port opened on @code{filename} and executes @code{body ...} with that binding in scope.
Returns the value(s) of the last expression in @code{body}.

}

@defform[(with-output-file (var filename kw-args ...) body ...+)
         #:contracts
         [(var identifier?)
          (filename path-string?)]]{

 Binds @code{var} to an output port opened on @code{filename} and executes @code{body ...} with that binding in scope.
 Returns the value(s) of the last expression in @code{body}.
 Keyword arguments following the filename are passed to @code{open-output-file}.

}

@defform[(with-null-input (var) body ...+)
         #:contracts [(var indentifier?)]]{

 Binds @code{var} to an input port that returns end of file on any attempts to read from it, and executes @code{body ...} with that binding in scope.
 Returns the value(s) of the last expression in @code{body}.

}

@defform[(with-null-output (var) body ...+)
         #:contracts [(var indentifier?)]]{

 Binds @code{var} to an output port that discards any data written to it, and executes @code{body ...} with that binding in scope.
 Returns the value(s) of the last expression in @code{body}.

}

@defform[(with-input (var input maybe-mode) body ...+)
         #:grammar [(maybe-mode (code:line) (code:line #:mode mode))]
         #:contracts
         [(var identifier?)
          (input (or/c input-port? string? bytes? path? boolean?))
          (mode (or/c 'text 'binary))]]{

Binds @code{var} to an input port opened as per @code{call-with-input} and executes @code{body ...} with that binding in scope.
Returns the value(s) of the last expression in @code{body}.

}

@defform[(with-output (var output kw-args ...) body ...+)
         #:contracts
         [(var identifier?)
          (output (or/c output-port? path-string? boolean?))]]{

 Binds @code{var} to an output port opened as per @code{call-with-output} and executes @code{body ...} with that binding in scope.
 Returns the value(s) of the last expression in @code{body}, or a string if @code{output} is @code{#f}.
 Keyword arguments following the filename are passed to @code{open-output-file} when creating a file port.

}

@defproc[(read-bytes-up-to [port input-port?] [delim byte?]) (or/c bytes? eof-object?)]{

 Like @code{read-bytes-line} but allows a custom delimiter byte.

}

@defproc[(read-string-up-to [port input-port?] [delim char?]) (or/c string? eof-object?)]{

 Like @code{read-line} but allows a custom delimiter character.

}
