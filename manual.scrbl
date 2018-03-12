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

@section{How to use the library}

@(defmodule/this-package main)

@subsection{The High-Level Interface}

The high-level interface to the library lets you automatically manage
NAT port mappings by simply changing calls to @racket[udp-bind!] and
@racket[tcp-listen] to @racket[udp-bind!/public] and
@racket[tcp-listen/public], respectively.

Each socket managed by the library is associated with a
@racket[mapping-change-listener], a background thread that tracks
changes to the NAT configuration, keeping a set of "port assignments"
up-to-date. A user-supplied callback (@tt{on-mapping-change}) is
called every time the port assignment set changes.

Each port assignment in a set is an address at which the corresponding
socket is reachable. A set of port assignments includes both local
(internal to the NAT) and public (external to the NAT) addresses.

@defproc[(udp-bind!/public [udp-socket udp?]
                           [hostname-string (or/c string? #f)]
                           [port-no (and/c exact-nonnegative-integer?
                                           (integer-in 0 65535))]
                           [#:on-mapping-change on-mapping-change
                                                (-> (set/c port-assignment?)
                                                    any/c)
                                                void])
        mapping-change-listener?]{

Does the work of @racket[udp-bind!], and opens and starts managing a
UDP port mapping at the local NAT.}

@defproc[(tcp-listen/public [port-no (and/c exact-nonnegative-integer?
                                            (integer-in 0 65535))]
                            [max-allow-wait exact-nonnegative-integer? 4]
                            [reuse? boolean? #f]
                            [hostname (or/c string? #f) #f]
                            [#:on-mapping-change on-mapping-change
                                                 (-> (set/c port-assignment?)
                                                     any/c)
                                                 void])
        (values tcp-listener? mapping-change-listener?)]{

Does the work of @racket[tcp-listen], and opens and starts managing a
     TCP port mapping at the local NAT.}

@defproc[(mapping-change-listener [protocol (or/c 'tcp 'udp)]
                                  [initial-local-address string?]
                                  [local-port (integer-in 0 65535)]
                                  [on-mapping-change (-> (set/c port-assignment?) any/c)])
         mapping-change-listener?]{

Opens and starts managing a TCP or UDP port mapping at the local NAT
for the given port. This routine is the workhorse that both
@racket[udp-bind!/public] and @racket[tcp-listen/public] delegate to.}

@defstruct*[mapping-change-listener ([thread thread?]) #:prefab]{
Handle for a mapping change listener. Useful with
@racket[mapping-change-listener-stop!] and so forth.}

@defstruct*[port-assignment ([protocol (or/c 'udp 'tcp)]
                            [address string?]
                            [port exact-nonnegative-integer?]
                            [nat-traversal-technique (or/c 'nat-pmp 'upnp #f)])
                           #:prefab]{

Record of a particular name for a socket. The protocol, address, and
port together form a name that can be used by remote peers to contact
the socket. The @racket[port-assignment-nat-traversal-technique] field
is @racket['nat-pmp] or @racket['upnp] for a NAT-originated name, or
@racket[#f] for a local interface name (i.e., built from the results
of @racket[interface-ip-addresses]).}

@defproc[(mapping-change-listener-current-mappings [mcl mapping-change-listener?])
        (set/c port-assignment?)]{
Retrieves the current set of port assignments from the given change
listener. May be useful instead of or in addition to using
@tt{on-mapping-change}.}

@defproc[(mapping-change-listener-stop! [mcl mapping-change-listener?]) void?]{
Deletes any active NAT mappings, and stops the background thread
involved in a mapping change listener. Call this while or after
closing the associated socket.}

@subsection{Getting information on local gateways and interfaces}

This library provides utilities for discovering and classifying
interface IP addresses, and for discovering the local default gateway
IP.

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
Returns @racket[#t] if and only if the argument is the wildcard
(a.k.a. @tt{INADDR_ANY}) IP address string; that is, if it is the
string "0.0.0.0".}

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

@defstruct*[mapping ([protocol (or/c 'udp 'tcp)]
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

@subsubsection{UPnP}

@racket[(require (planet tonyg/nat-traversal/upnp-ip-gateway))]

TODO

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

@defstruct*[persistent-mapping ([thread thread?]) #:prefab]{
Handle for a persistent mapping. Useful with @racket[stop-persistent-mapping!], etc.}

@subsubsection{Calling other UPnP services}

@racket[(require (planet tonyg/nat-traversal/upnp))]

Routines for discovering UPnP services and calling service actions.

@defparam[default-scan-time seconds exact-nonnegative-integer?]{
Defines the number of seconds that an @racket[in-upnp-services]
discovery scan will remain active for.}

@defstruct*[(exn:fail:upnp exn:fail) ([code (or/c exact-nonnegative-integer? #f)]
                                      [description (or/c exact-nonnegative-integer? #f)])
        #:transparent]{
Exception thrown upon a UPnP-related SOAP fault or failure.}

@defproc[(exn:fail:upnp?/code [code exact-nonnegative-integer?])
        (-> any/c boolean?)]{
Returns a predicate that returns @racket[#t] if and only if its
argument is an @racket[exn:fail:upnp?] with
@racket[exn:fail:upnp-code] equal to @racket[code].}

@defstruct*[upnp-service ([type string?]
                         [control-url url?]
                         [event-url url?]
                         [scpd-url url?]) #:prefab]{
Describes a UPnP service. The @racket[upnp-service-type] will be a URI.}

@defstruct*[upnp-service-action ([name string?]
                                [args (listof string?)]
                                [results (listof string?)]) #:prefab]{
Describes a particular action available on a UPnP service. The
@racket[upnp-service-action-args] are the names of the arguments
expected from the caller, and the @racket[upnp-service-action-results]
are the names of the results expected to be returned from the server.}

@defproc[(in-upnp-services [#:scan-time scan-time exact-nonnegative-integer? (default-scan-time)])
        (sequenceof (case-> (-> 'descriptor upnp-service?)
                            (-> 'actions (hash/c string? upnp-service-action?))
                            (-> string? #:rest (listof string?) (hash/c string? string?))))]{
Produces a sequence of service dispatchers, one for each discovered
UPnP service on the local network. Yields each dispatcher as it is
discovered. Continues producing values as they arrive until
@racket[scan-time] seconds have elapsed, at which point the sequence
ends.

Each of the yielded dispatchers is a procedure taking a variable
number of arguments:

@itemize[
  @item{If the first argument is @racket['descriptor], the underlying
  @racket[upnp-service] is returned.}

  @item{If the first argument is @racket['actions], a hashtable
  mapping action name strings to @racket[upnp-service-action]
  instances is returned.}

  @item{Otherwise, the first argument must be a string naming an
  action supported by the service being dispatched to. The number of
  additional arguments must be equal to the number of arguments
  expected by the named action, and they must all be strings. The
  result will be a hashtable mapping result name to string result
  value.}
]
}

@defproc[(upnp-service-type=? [s upnp-service?] [type string?]) boolean?]{
Returns @racket[#t] if and only if @racket[(upnp-service-type s)] is
equal to @racket[type].}

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
