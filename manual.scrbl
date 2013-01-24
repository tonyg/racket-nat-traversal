#lang scribble/manual

@(require planet/scribble
	  scribble/racket
	  scriblib/footnote
	  (for-label racket
	  	     (this-package-in main)))

@title{racket-nat-pmp}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@local-table-of-contents[]

If you find that this library lacks some feature you need, or you have
a suggestion for improving it, please don't hesitate to
@link["mailto:tonygarnockjones@gmail.com"]{get in touch with me}!

@section{Introduction}

This library implements NAT-PMP, the Apple/IETF protocol for opening
TCP and UDP ports on home routers.

Using this library, you can discover the external IP address of your
home router, and can manage port mappings from the public internet to
internal TCP and UDP ports.

@section{References}

NAT-PMP is currently defined in an Internet-Draft.

@itemize[
  @item{@link["https://tools.ietf.org/html/draft-cheshire-nat-pmp-06"]{The NAT-PMP Internet-Draft at the time this library was written}.}
  @item{@link["http://miniupnp.free.fr/nat-pmp.html"]{A useful quick overview of the protocol}.}
  @item{@link["http://en.wikipedia.org/wiki/NAT_Port_Mapping_Protocol"]{NAT-PMP on Wikipedia}.}
]

@section{What to require}

All the functionality below can be accessed with a single
@racket[require]:

@(defmodule/this-package main)

@subsection{Getting the Gateway IP}

NAT-PMP depends on being able to learn the IP address of the current
default gateway. Currently, this library learns this information by
running the system utility @tt{netstat} and parsing its output.

@defproc[(gateway-ip-address) string?]{
Retrieves a string representation of the current gateway IP address,
for example @racket["10.0.0.1"].}

@subsection{Timeouts}

Requests made to the gateway using NAT-PMP will eventually time out if
the gateway does not support NAT-PMP. In this case, an exception is
raised.

@subsection{Discovering the current external IP address}

@defproc[(external-ip-address) string?]{
Uses the NAT-PMP protocol to ask the current gateway what the current
external IP address is.}

@subsection{Low-level interface: mapping ports manually}

If you can't or don't want to use the persistent mapping support of
this library, these routines let you explicitly manage mappings with
the gateway.

@defproc[(map-port! [protocol (or/c 'udp 'tcp)]
		    [local-port integer?]
		    [requested-port integer?]
		    [lifetime-seconds integer?]) mapping?]{
Creates a mapping at the gateway, requesting that
@racket[requested-port] on the external IP address be mapped to
@racket[local-port] on the IP address of the local machine. The
mapping will survive on the gateway for @racket[lifetime-seconds]
seconds or until the router is rebooted. If both
@racket[requested-port] and @racket[lifetime-seconds] are zero, an
existing mapping for @racket[local-port] is deleted (but use
@racket[unmap-port!] instead).}

@defproc[(unmap-port! [protocol (or/c 'udp 'tcp)] [local-port integer?]) void?]{
Unmaps an existing mapping for @racket[local-port].}

@defproc[(unmap-all-ports! [protocol (or/c 'udp 'tcp)]) void?]{
Unmaps all existing mappings for the local IP address and the given protocol.}

@defproc[(refresh-mapping! [mapping mapping?]) mapping?]{
Refreshes a mapping by extracting its fields and calling @racket[map-port!].}

@defproc[(delete-mapping! [mapping mapping?]) void?]{
Deletes a mapping by extracting its fields and calling @racket[unmap-port!].}

@defstruct[mapping ([protocol (or/c 'udp 'tcp)]
		    [internal-port integer?]
		    [external-port integer?]
		    [lifetime integer?]) #:prefab]{
A record of an established mapping.}

@subsection{High-level interface: persistent mappings}

@defproc[(make-persistent-mapping [protocol (or/c 'udp 'tcp)]
				  [local-port integer?]
				  [requested-port integer?]
				  [#:refresh-interval refresh-interval integer? 7200]
				  [#:on-mapping on-mapping (-> mapping? any/c) void]) persistent-mapping?]{
Establishes a persistent mapping, which will refresh itself in the
background every @racket[#:refresh-interval] seconds until told to
stop with @racket[stop-persistent-mapping!].

Every time the externally mapped port changes (including when the
mapping is first established!) the @racket[#:on-mapping] callback is
called with the updated mapping information. Note that the callback is
invoked directly from the mapping's thread - if it raises an
exception, it will kill the persistent mapping.}

@defproc[(stop-persistent-mapping! [p persistent-mapping?]) void?]{
Deletes and stop refreshing an earlier-established persistent mapping.
@racket[delete-mapping!] is called to delete the mapping at the
gateway.}

@defproc[(current-persistent-mapping [p persistent-mapping?]) mapping?]{
Retrieves the current mapping details from a persistent mapping.}

@defproc[(refresh-persistent-mapping! [p persistent-mapping?]) void?]{
Overrides the internal timers in the persistent mapping, causing it to
refresh itself at the gateway right now. Normal refreshing will resume
thereafter.}

@defstruct[persistent-mapping ([thread thread?]) #:prefab]{
Handle for a persistent mapping. Useful with @racket[stop-persistent-mapping!], etc.}
