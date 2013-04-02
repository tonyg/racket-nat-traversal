#lang scribble/manual

@(require planet/scribble
          scribble/racket
          scriblib/footnote
          (for-label racket
                     (this-package-in main)))

@title{racket-nat-traversal}
@author[(author+email "Tony Garnock-Jones" "tonygarnockjones@gmail.com")]

@local-table-of-contents[]

If you find that this library lacks some feature you need, or you have
a suggestion for improving it, please don't hesitate to
@link["mailto:tonygarnockjones@gmail.com"]{get in touch with me}!

@section{Introduction}

Using this library, you can discover the external IP address of your
home router, and can manage port mappings from the public internet to
internal TCP and UDP ports.

The library implements

@itemize[
  @item{NAT-PMP, the Apple/IETF protocol for opening TCP and UDP ports
  on home routers, and}

  @item{UPnP, in particular the "WANIPConnection" service, the
  Microsoft et al. protocol for doing the same, plus a whole lot
  more.}
]

It provides both a low-level interface to each of the two protocols as
well as a high-level interface that abstracts away from the details of
the particular NAT traversal techniques available.

@section{References}

NAT-PMP is currently defined in an Internet-Draft.

@itemize[
  @item{@link["https://tools.ietf.org/html/draft-cheshire-nat-pmp-06"]{The NAT-PMP Internet-Draft at the time this library was written}.}
  @item{@link["http://miniupnp.free.fr/nat-pmp.html"]{A useful quick overview of the protocol}.}
  @item{@link["http://en.wikipedia.org/wiki/NAT_Port_Mapping_Protocol"]{NAT-PMP on Wikipedia}.}
]

UPnP is a vast expanse of entangled specification.

@itemize[
  @item{The core discovery mechanism is
    @link["http://en.wikipedia.org/wiki/Simple_Service_Discovery_Protocol"]{SSDP}.}

  @item{Everything else in UPnP is done with
    @link["http://en.wikipedia.org/wiki/SOAP"]{SOAP} (!) over HTTP.}

  @item{You can @link["http://www.upnp.org/specs/arch/"]{download} the
    70MB (!) specification zip file from the
    @link["http://www.upnp.org/"]{UPnP forum}. Fair warning, it's not
    an easy read.}
]

@section{How to use the library}

@(defmodule/this-package main)

@subsection{The High-Level Interface}

@subsection{Getting information on local gateways and interfaces}

This library provides utilities for discovering and classifying local
interface addresses, and for discovering the local default gateway IP.

@defproc[(gateway-ip-address) string?]{
Retrieves a string representation of the current default gateway IP
address, for example @racket["10.0.0.1"]. Currently, this library
learns this information by running the system utility @tt{netstat} and
parsing its output.}

@defproc[(interface-ip-addresses) (listof string?)]{
Retrieves a list of string representations of the IP addresses
associated with the system's currently-active interfaces. This is done
by running the system utility @tt{ifconfig} and parsing its output.}

@defproc[(best-interface-ip-address) string?]{
Returns the "best" value from the list returned by
@racket[interface-ip-addresses], where

@itemize[
  @item{the first non-private, non-local IP address found is considered best;}
  @item{the first private, non-local IP address found is considered second-best;}
  @item{any other address found is considered third-best;}
  @item{and if no addresses were found at all, @racket["127.0.0.1"] is returned.}
]
}

@defproc[(wildcard-ip-address? [addr string?]) boolean?]{
Returns @racket[#t] if and only if the argument is the wildcard (a.k.a
INADDR_ANY) IP address string; that is, if it is the string
"0.0.0.0".}

@defproc[(localhost-ip-address? [addr string?]) boolean?]{
Returns @racket[#t] if and only if the argument is a local IP address
string; that is, if it begins with the string "127.".}

@defproc[(private-ip-address? [addr string?]) boolean?]{
Returns @racket[#t] if and only if the argument is an IP address in
one of the @link["http://tools.ietf.org/html/rfc1918"]{RFC 1918}
"private" address ranges; that is, 10.x.y.z, 172.16.x.y through
172.31.x.y, or 192.168.x.y.}

@subsection{Low-level interfaces}

@subsubsection{Records of established mappings}

@defstruct[mapping ([protocol (or/c 'udp 'tcp)]
                    [internal-address (or/c string? #f)]
                    [internal-port integer?]
                    [external-port integer?]
                    [lifetime integer?]) #:prefab]{
A record of an established mapping. In cases where the internal
address is not known at the time of mapping (e.g. when using NAT-PMP),
@racket[mapping-internal-address] will be @racket[#f].}

@subsubsection{NAT-PMP}

@racket[(require (planet tonyg/nat-traversal/nat-pmp))]

NAT-PMP depends on being able to learn the IP address of the current
default gateway. It does so by calling @racket[gateway-ip-address].

Requests made to the gateway using NAT-PMP will eventually time out if
the gateway does not support NAT-PMP. When this happens, an exception
is raised by the routine making the request.

@defproc[(nat-pmp-external-ip-address) string?]{
Uses the NAT-PMP protocol to ask the current gateway what the current
external IP address is.}

@defproc[(nat-pmp-make-persistent-mapping [protocol (or/c 'udp 'tcp)]
                                          [local-port integer?]
                                          [requested-port integer?]
                                          [#:refresh-interval refresh-interval integer? 3600]
                                          [#:on-mapping on-mapping (-> (or/c string? #f)
                                                                       (or/c mapping? #f)
                                                                       any/c) void])
                                          persistent-mapping?]{
Establishes a persistent mapping, which will refresh itself in the
background every @racket[#:refresh-interval] seconds until told to
stop with @racket[stop-persistent-mapping!].

Every time the externally mapped port changes (including when the
mapping is first established!) the @racket[#:on-mapping] callback is
called with the updated mapping information. Note that the callback is
invoked directly from the mapping's thread - if it raises an
exception, it will kill the persistent mapping.}

If you can't or don't want to use persistent mapping, the following
routines let you explicitly manage mappings with the gateway.

@defproc[(nat-pmp-map-port! [protocol (or/c 'udp 'tcp)]
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
@racket[nat-pmp-delete-mapping!] instead).}

@defproc[(nat-pmp-unmap-all-ports! [protocol (or/c 'udp 'tcp)]) void?]{
Unmaps all existing mappings for the local IP address and the given protocol.}

@defproc[(nat-pmp-refresh-mapping! [mapping mapping?]) mapping?]{
Refreshes a mapping by extracting its fields and calling @racket[nat-pmp-map-port!].}

@defproc[(nat-pmp-delete-mapping! [mapping mapping?]) void?]{
Deletes a mapping.}

@subsubsection{Managing low-level persistent mappings}

@defproc[(stop-persistent-mapping! [p persistent-mapping?]) void?]{
Deletes and stops refreshing an earlier-established persistent mapping.
The mapping is deleted at the gateway where it was established.}

@defproc[(current-persistent-mapping [p persistent-mapping?]) mapping?]{
Retrieves the current mapping details from a persistent mapping.}

@defproc[(refresh-persistent-mapping! [p persistent-mapping?]) void?]{
Overrides the internal timers in the persistent mapping, causing it to
refresh itself at its gateway right now. Normal refreshing will resume
thereafter.}

@defstruct[persistent-mapping ([thread thread?]) #:prefab]{
Handle for a persistent mapping. Useful with @racket[stop-persistent-mapping!], etc.}
