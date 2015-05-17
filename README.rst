====
hbro
====

**In a nutshell**, *hbro* is a minimal web browser for linux. It is written, configured and extensible in Haskell.

Information about versions, dependencies, source repositories and contacts can be found in hackage_.


Design principles
-----------------

`Do one thing well`_
  A web browser is **not** a {window|bookmarks|history|download|passwords|package} manager, let alone an operating system.
  A web browser retrieves, renders and traverses web pages, period.

`Keep It Simple, Stupid`_
  The application should be written with simplicity in mind, and without obsession for performance, features or release frequency. It should boot instantly, consume little memory and offer an uncluttered graphical interface. The code should be easy to grasp (well, as long as you speak Haskell...) to encourage users to hack it. Simplicity provides lightness, scalability, stability and maintainability.

Extensible
  Users should be able to implement extra features through an extension system, without digging into the internals of the application. External programs should be able to query/command the web browser.

Good defaults
  The default behavior should be suitable for users that cannot afford or don't want to spend (waste ?) their time in tweaks.

Keyboard driven
  Keyboard control should be made as much convenient, with as little mouse intervention, as possible.


Components and libraries used
-----------------------------

Programming language : Haskell_
  Modern, purely-functional language that makes it possible to work with a concise, elegant and robust code.

Layout engine : WebKit_
  Has to be open-source, be (kind of) standards-compliant, and provide a Haskell binding. Considering those requirements, WebKit is pretty much the only game in town.

HTTP client : WebKit_
  Ideally, the HTTP client should be delegated to a distinct library (typically http-conduit_), but WebKit clearly wasn't designed to be used as a *mere* layout engine. So for now, it still handles all network connections.

UI toolkit : `GTK+`_
  Given the programming language and layout engine, there's no much choice left for the UI toolkit.

Interprocess interface : ZeroMQ_
  Socket-like interface that implements various communication schemes like request-reply and publish-subscribe.

Configuration system : Dyre_
  Dynamic reconfiguration library for haskell programs.


Suggestions about better alternatives for any of these points (except the programming language) are welcome.


Installation notes
------------------

Up until GHC 7.8, *hbro* requires the *integer-simple* package, which means you won't be able to build it using a standard GHC installation that uses the *integer-gmp* package. This distinction is `documented here`_, and the reason for this constraint is `explained there`_.

Starting with GHC 7.10, the *integer-gmp* package was completely rewritten and *integer-simple* is no longer required.


Configuration
-------------

By default, a minimal configuration file (see ``Hbro/Main.hs``) is used to build *hbro*. You can create your own at ``~/.hbro/hbro.hs`` to override it. Several extensions are provided with the * hbro-contrib_ * package, including a commented configuration file example.


GUI layout
----------

The graphical layout is described in an XML file that is parsed by GtkBuilder_. This file is looked for in several places with the following order of priority:

- the value from commandline option ``-U``;
- the ``~/.hbro/ui.xml`` file;
- the ``examples/ui.xml`` file bundled with the package.

At least the following widgets must be defined, with the adequate ``id`` attributes, for the browser to start:

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

Many problems/limitations are inherited from the *Haskell* bindings webkitgtk3_ and gtk3_. Until fixed in upstream, nothing can be done on *hbro* to work around them. Here's a summary of them:

- **segmentation faults when loading some webpages while javascript/flash is enabled**;
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
.. _http-conduit: https://hackage.haskell.org/package/http-conduit
.. _webkitgtk3: http://hackage.haskell.org/package/webkitgtk3
.. _gtk3: http://hackage.haskell.org/package/gtk3
.. _documented here: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer
.. _explained there: http://sourceforge.net/p/gtk2hs/mailman/gtk2hs-users/thread/20140515065151.GA8342%40mystik/
