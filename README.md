# Reliable multicast experiments

This project contains experiments in using multicasting to synchronise
participants in a distributed topic. Participants take on one or more
of three roles:

 - `copyin`: a publisher of content. Transmits messages until it
   receives an acknowledgement from a `copyack`.

 - `copyout`: a receiver of content. Handles received messages,
   detecting gaps in the message stream and requesting retransmissions
   from a `copyack`.

 - `copyack`: a store of responsibility for message delivery. Takes
   responsibility from `copyin`s by acknowledging their publications,
   and ensures `copyout`s have what they need by responding to their
   replay requests. Stores a copy of every message seen so far.

## Areas of investigation

 - timing: acknowledgement and retransmission schedules; batching;
   replay-storm avoidance

 - cache expiry policies

 - protocol for joining and leaving the ring

## Software License

The code in this project is [open-source](http://www.opensource.org/)
code, licensed under the very liberal [MIT
license](http://www.opensource.org/licenses/mit-license.php), with the
exception of `gen_server2.erl` and `priority_queue.erl`, which are
licensed as detailed in the files themselves.

    Copyright (c) 2009 Tony Garnock-Jones <tonyg@lshift.net>
    Copyright (c) 2009 LShift Ltd. <query@lshift.net>

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without
    restriction, including without limitation the rights to use, copy,
    modify, merge, publish, distribute, sublicense, and/or sell copies
    of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be
    included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
    HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
    WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    DEALINGS IN THE SOFTWARE.
