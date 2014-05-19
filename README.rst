====
hbro
====


**In a nutshell**: *hbro* is a minimal, KISS compliant browser for linux written, configured and extensible in Haskell.

Informations about versions, dependencies, source repositories and contacts can be found in hackage_.


Design principles
-----------------

`Do one thing well`_
  A web browser is **not** a {window|bookmarks|history|download|passwords|package} manager, let alone an operating system.
  A web browser retrieves, renders and traverses web pages, period.

`Keep It Simple, Stupid`_
  The program should be written with simplicity in mind, and without obsession for performance, features or release frequency. It should not take time to start-up, consume much RAM or crash. Its code should be easy to understand (well, as long as you speak Haskell...) to encourage users to hack it. Simplicity provides lightness, scalability, stability and maintainability.

Extensible
  Configuration system should allow users to implement extra features. External programs should be able to query/order *hbro*.

Good defaults
  A default configuration, suitable for users that cannot afford or don't want to spend (waste ?) their time in tweaks, should be provided.

Keyboard driven
  Keyboard control should be made as much convenient, with as little mouse intervention, as possible.


Components and libraries used
-----------------------------

Programming language : Haskell_
  Modern, purely-functional language that makes it possible to work with a short, elegant and robust code.

Layout engine : WebKit_
  Webkit seems to be the only engine being open-source, (kind of) standards-compliant and providing a Haskell binding. It's not much of a choice, fortunately it's not that bad.

UI toolkit : `GTK+`_
  Given the above programming language and layout engine, there's no much choice left for the UI toolkit.

Interprocess interface : ZeroMQ_
  Socket-like interface that implements various convenient communication schemes like request-reply and publish-subscribe.

Configuration system : Dyre_
  Dynamic reconfiguration library for haskell programs.


Suggestions about better alternatives for any of these points (except the programming language) are welcome.


Configuration
-------------

By default, a minimal configuration file (see ``Hbro/Main.hs``) is used to build *hbro*. You can create your own at ``~/.config/hbro/hbro.hs`` to override it. Several extensions are provided with the * hbro-contrib_ * package, including a commented configuration file example.


GUI layout
----------

The graphical layout is described in an XML file that is parsed by GtkBuilder_ to build up the whole GUI. This file is looked for in several places with the following order of priority:

- the value from commandline option ``-u``;
- the file ``~/.config/hbro.ui.xml``;
- the file ``examples/ui.xml`` bundled with the package.

You must at least define the following widgets with the adequate ``id`` attribute:

+-----------------------+-----------------------+
| Type                  | ``id``                |
+=======================+=======================+
| ``GtkWindow``         | ``mainWindow``        |
+-----------------------+-----------------------+
| ``GtkVBox``           | ``windowBox``         |
+-----------------------+-----------------------+
| ``GtkScrolledWindow`` | ``webViewParent``     |
+-----------------------+-----------------------+
| ``GtkHBox``           | ``promptBox``         |
+-----------------------+-----------------------+
| ``GtkLabel``          | ``promptDescription`` |
+-----------------------+-----------------------+
| ``GtkEntry``          | ``promptEntry``       |
+-----------------------+-----------------------+
| ``GtkHBox``           | ``statusBox``         |
+-----------------------+-----------------------+
| ``GtkLabel``          | ``notificationLabel`` |
+-----------------------+-----------------------+


Known bugs and limitations
--------------------------

Unfortunately, many problems/limitations are inherited from the *Haskell* binding for *webkit*/*gtk*. Until fixed in upstream, nothing can be done on *hbro* to work around them. Here's a summary of them:

- *segmentation faults when using HTTPS*;
- *segmentation faults when loading some webpages or enabling javascript/flash*;
- vertical scrollbar cannot be hidden;
- no proxy configuration;
- no cookies management;
- javascript's ``window.open`` requests open in the same window instead of spawning a new one;
- toggling to source mode reloads current webpage (which may be undesired)

Patches or suggestions are welcome to deal with the following issues.


License
-------

*hbro* is distributed under the `Do-What-The-Fuck-You-Want-To public licence`_, which has a pretty self-explanatory name.


.. _hackage: http://hackage.haskell.org/package/hbro
.. _Do one thing well: http://en.wikipedia.org/wiki/Unix_philosophy
.. _Keep It Simple, Stupid: https://en.wikipedia.org/wiki/KISS_principle
.. _Do-What-The-Fuck-You-Want-To public licence: http://en.wikipedia.org/wiki/WTFPL
.. _Haskell: http://haskell.org/
.. _WebKit: http://www.webkit.org/
.. _GTK+: http://www.gtk.org/
.. _ZeroMQ: http://www.zeromq.org/
.. _Dyre: https://github.com/willdonnelly/dyre
.. _hbro-contrib: http://hackage.haskell.org/package/hbro-contrib
.. _GtkBuilder: https://developer.gnome.org/gtk3/stable/GtkBuilder.html
