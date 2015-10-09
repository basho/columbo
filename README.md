Columbo - the dependency detective - is a tool for detecting dependency problems in Erlang projects with many 3rd party components.

Columbo traverses all depedencies from directory it is being executed from.

It looks at the rebar.config file to figure out what the dependencies are (support for erlang.mk will come later).

*Warning!!* This is serious work-in-progress! The code has been slapped together to be ready for a presentation and needs to be massaged a bit to be maintainable.

# Dependencies

* graphviz
* git
* Erlang ;-)

# Installation

Clone the repo.

make escript.

Put the `columbo` script somewhere on your path.

# Example of Usage

`$ git clone https://github.com/basho/riak_core` 

`$ cd riak_core` 

`$ git checkout 2.1.1` 

`$ columbo` 

`$ dot -O -Tpng riak_core-2.1.1-29-g5a49f6b.dot` 

(Note: the exact string after 2.1.1 may be different from what I have in my repo at the time of writing this).

`$ open riak_core-2.1.1-29-g5a49f6b.dot.png` 

Basically, if your image has more than one node in a box, then you have a dependency conflict that should be fixed.

# To-Dos

* Make it work with erlang.mk based projects.
