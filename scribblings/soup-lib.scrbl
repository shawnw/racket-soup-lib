#lang scribble/manual
@require[@for-label[soup-lib
                    racket/base racket/contract racket/function json]]

@title{Soup: A library of useful routines}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

A collection of useful functions not important enough to spin off into their own packages.

@table-of-contents[]

@section{Top level interface}

@defmodule[soup-lib]

Provides all the functions exported by the modules below.

@section{Additional functions for paths and files}

@defmodule[soup-lib/files]

@defproc[(delete-file* [filename path-string?]) boolean?]{

 Deletes a file without raising an error if it doesn't exist.

 Returns true if the file was deleted, false on failure.

}

@section{Additional @code{for} and @code{for*} style comprehensions}

@defmodule[soup-lib/for]

@defform[(for/max (sequences) body ...+)]{

 @code{body} must evaluate to a single real number at each iteration, the maximum of which is returned.

}

@defform[(for*/max (sequences) body ...+)]{

 @code{body} must evaluate to a single real number at each iteration, the maximum of which is returned.

}

@defform[(for/min (sequences) body ...+)]{

 @code{body} must evaluate to a single real number at each iteration, the minimum of which is returned.

}

@defform[(for*/min (sequences) body ...+)]{

 @code{body} must evaluate to a single real number at each iteration, the minimum of which is returned.

}


@defform[(for/string [#:length exact-positive-integer?] (sequences) body ...+)]{

@code{body ...} must evaluate to a single character at each iteration, all of which are returned as a string.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.

}

@defform[(for*/string [#:length exact-positive-integer?] (sequences) body ...+)]{

@code{body ...} must evaluate to a single character at each iteration, all of which are returned as a string.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.
 
}

@defform[(for/bytes [#:length exact-positive-integer?] (sequences) body ...+)]{

@code{body ...} must evaluate to a single byte at each iteration, all of which are returned as a bytestring.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.

}

@defform[(for*/bytes [#:length exact-positive-integer?] (sequences) body ...+)]{

@code{body ...} must evaluate to a single byte at each iteration, all of which are returned as a bytestring.

 The optional @code{#:length} argument can be used to give the expected length of the result as an optimization.
 
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

@defproc[(hash->immutable-hash [htab hash?]) (and/c hash? immutable?)]{

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

Stuff from the @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/c_conses.htm"]{cons dictionary} without Racket or SRFI-1 equivalents.
Sometimes with better names.

@defproc[(sublis [alist (listof pair?)] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) any/c]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_sublis.htm"]{Common Lisp @racket{sublis}}.

}

@defproc[(subst [new any/c] [old any/c] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) any/c]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst}}.

}

@defproc[(subst-if [new any/c] [pred? (-> any/c any/c)] [tree any/c] [#:key key (-> any/c any/c) identity]) any/c]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst-if}}.

}

@defproc[(adjoin [elem any/c] [list list?] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) list?]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_adjoin.htm"]{Common Lisp @racket{adjoin}}.

}

@defproc[(maplist [proc procedure?] [list list?] ...+) list?]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm"]{Common Lisp @racket{maplist}}.

}

@defproc[(append-maplist [proc procedure?] [list list?] ...+) list?]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm"]{Common Lisp @racket{mapcon}}.

}

@defproc[(tail? [obj any/c] [list (or/c pair? null?)]) boolean?]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_ldiffc.htm"]{Common Lisp @racket{tailp}}.

}

@defproc[(ldiff [list (or/c pair? null?)] [obj any/c] ) (or/c pair? null?)]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_ldiffc.htm"]{Common Lisp @racket{ldiff}}.

}

@defproc[(rassoc [item any/c] [alist (listof pair?)] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) (or/c pair? #f)]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm"]{Common Lisp @racket{rassoc}}.

}

@defproc[(rassoc-if [pred? (-> any/c any/c)] [alist (listof pair?)] [#:key key (-> any/c any/c) identity]) (or/c pair? #f)]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm"]{Common Lisp @racket{rassoc-if}}.

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

@defproc[(string-sort! [s (and/c string? (not/c immutable?))] [<? (-> char? char? any/c) char<?]) void?]{

 Sorts @code{s} in-place.

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

}

@defform*[((define-boolean-parameter id)
           (define-boolean-parameter id initial-value)
           (define-boolean-parameter id initial-value name))
          #:contracts ([id identifier?]
                       [initial-value boolean?]
                       [name symbol?])]{

 Define a boolean parameter with the given name. The default initial value if not given is @code{#t}. Any value can be used to set the parameter,
 but it's converted to a boolean; in other words it's compatible with the contract @code{(parameter/c any/c boolean?)}.

 The optional @code{name} argument is as in @code{make-parameter}.

}
