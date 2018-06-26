# sonic-lisp

_Written by Lucas Vieira <lucasvieira@lisp.com.br>_


## What is this?

This is a clone of Sonic The Hedgehog's Sega Genesis engine, which aims to replicate the physics
of the mascot's classic games as much as possible.




## Why Common Lisp?

There are two reasons for that. First of all, Common Lisp is generally an underrated language, for those
who never tasted its power. So after thinkering with it for a few months, I decided to develop something
that would make me sweat my shirt a little more.

Which leads to the second reason. Lisp in game development is not news for me. For the past few years, I've
been developing generic game engines and trying to emulate Sonic engine on them has been a personal goal which
I've always set as one of my proof-of-concepts for this kind of project: if it can run such a complex platformer,
then it works. The second version of one of my engines also included the ability to run Scheme as scripting
language; however, the effort to embedding such a language in a C++ program seemed always so big, and while it
did not perform well due to design and implementation choices, there was just so much friction in just enabling
those scripts so they can work with in-game objects.

Hence enters Common Lisp. While not a minimal language like Scheme, Common Lisp is just as mature and has so many
powerful tools, either on the language or external, which fed my needs for game development. So, if we add these
tools, take a good implementation, and then take a mature engine which Lisp hackers use for developing their own
games (yes, Lisp game development is a thing), we get stuff like this project.

I am definitely biased on those terms but, if you came here looking for game development stuff, I tell you right
now: why not take Common Lisp for a spin?




## FAQ

### Why didn't you use _<insert some other dialect here>_?

Basically, because I wanted to use Common Lisp. You see other game developers using specifically Common Lisp?
They're kind of rare, you know. Most just go with what everyone uses and try to use something that underneath is
Python, Lua or anything else. And well, you know, just because something isn't very popular, doesn't mean it's
bad nor inappropriate for the job.

### Why don't you try _<insert some framework for other dialect>_ too? It's super easy to use.

No.

### Why didn't you use _<insert non-lisp-y language with Lisp disguise here>_? All the cool kids are using the base language, you get the best of both worlds!

It's not Lisp.

### Why not just use JavaScript?

**DON'T EVEN GET ME STARTED.**




## Running

As per `trivial-gamekit`'s requirements, in order to run the project, you will need:

- A x86_64 operational system (Windows, Linux, OSX)
- OpenGL 3.3+ support
- A x86_64 Lisp implementation (specifically SBCL on CCL)
- Quicklisp

### Installing Quicklisp

You will need Quicklisp in order to perform the next steps.

For more information, check [this page](https://www.quicklisp.org/beta/).

### Installing `cl-bodge` and `trivial-gamekit`

The package itself depends on `trivial-gamekit`, a layer of abstraction built over the `cl-bodge` engine.

For more information, check [this page](https://borodust.org/projects/trivial-gamekit/).


### Running `sonic-lisp`

For starters, either clone this repository on your `quicklisp/local-projects` directory, or create a symlink to the
cloned folder on your `quicklisp/local-projects` directory.

**REMEMBER THAT THIS REPOSITORY USES GIT LARGE FILE SUPPORT**, so you will most likely need to enable it in order to
fetch assets such as sprites, backgrounds and sound effects!

Then you can load the system and start the game:

```lisp
(ql:quickload :sonic-lisp)
(sonic-lisp:start)
```

Anytime you want to quit, either close the window or type the following in the REPL:


```lisp
(sonic-lisp:stop)
```





## Documentation

Currently, the project is very poor on documentation, this is due to the fact that it's still highly experimental.




## License

This project is distributed under the MIT License, except for eventual copyrighted characters (Sonic's sprites, some
sound effects, etc). Specifically for the code and for the code only, check the [LICENSE file](LICENSE) for details.
