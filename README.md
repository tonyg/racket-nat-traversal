# racket-nat-traversal, a set of NAT traversal utilities for Racket.

This library implements

 - NAT-PMP, the Apple/IETF protocol for opening TCP and UDP ports on
   home routers, and

 - UPnP, in particular the "WANIPConnection" service, the Microsoft et
   al. protocol for doing the same, plus a whole lot more.

It provides both a low-level interface to each of the two protocols as
well as a high-level interface that abstracts away from the details of
the particular NAT traversal techniques available.

## License

racket-nat-traversal is written by Tony Garnock-Jones
<tonygarnockjones@gmail.com> and is licensed under the [AGPL
3.0](http://www.gnu.org/licenses/agpl-3.0.html).
