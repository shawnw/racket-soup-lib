#lang scribble/manual
@require[@for-label[soup-lib
                    racket/base racket/function]]

@title{A library of useful routines}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

A collection of useful functions not important enough to spin off into their own packages.

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

@defproc[(subst [old any/c] [new any/c] [tree any/c] [#:key key (-> any/c any/c) identity] [#:test test (-> any/c any/c any/c) eqv?]) any/c]{

 See @hyperlink["http://www.lispworks.com/documentation/HyperSpec/Body/f_substc.htm"]{Common Lisp @racket{subst}}.

}

@defproc[(subst-if [old any/c] [pred? (-> any/c any/c)] [tree any/c] [#:key key (-> any/c any/c) identity]) any/c]{

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
