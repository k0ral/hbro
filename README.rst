====
hbro
====


**In a nutshell**: *hbro* is a minimal KISS compliant browser for linux written and configured in Haskell, still in development but pretty usable.

Informations about versions, dependencies, source repositories and contacts can be found in hackage_.


Design principles
-----------------

Do only one thing...
  Modern browsers include many features that could be easily externalized ; actually, life would even be easier if those features were independent and shared with all desktop applications. Such features are for instance tabs, bookmarks, history, downloads, adblocking, passwords saving, self-updating. A UNIX-compliant browser should just be flexible enough to call external tools for such jobs.

... but do it well
  Browsing well first implies making our way through the mess of web standards that are growing harder and sloppier; sometimes it means breaking some other golden rules, but there's no choice.

Keep It Simple, Stupid
  Simplicity is not only compatible with but also essential to a powerful application. It often comes together with lightweight, scalable and easy-to-hack qualities.

Robustness & stability
  Let's spend our time in developing new features rather than in tracking rare bugs. Let's go slow but sure.

Extensibility and programmable interface
  Targets are advanced users who have various expectations ; to be sure everyone is happy, configuration should use a programming language, and an interprocess interface should be available. As he who can do the most can do the least, the default configuration should be suitable for users that cannot afford/don't want to spend (waste ?) their time in tweaks.

Keyboard-friendly
  Special attention should be given to allow keyboard control of the browser whenever possible ; however, this should not become an obsession in cases where mouse use is obviously a more convenient solution (yes, such cases do exist).

Free software
  Hbro is distributed under the `Do-What-The-Fuck-You-Want-To public licence`_, which has a pretty self-explanatory name :) .

Note that some of these principles are taken from the `suckless manifest`_.


Components and libraries used
-----------------------------

Programming language : Haskell_
  Modern, purely-functional language that makes it possible to work with a short, elegant and robust code.

Layout engine : WebKit_
  Webkit seems to be the only one being open-source, (kind of) standards-compliant and providing a Haskell binding. It's then not much of a choice, fortunately it's not that bad.

UI toolkit : `GTK+`_
  Given the above programming language and layout engine, there's no much choice left for the UI toolkit.

Interprocess interface : ZeroMQ_
  Socket-like interface that implements various convenient communication schemes like request-reply and publish-subscribe.

Configuration system : Dyre_
  Dynamic reconfiguration library for haskell programs.


Apart from the programming language, if you happen to find a better alternative for one of these points, please note that suggestions are more than welcome :) .


.. How to install it ?
   -------------------
    
   Please note that despite being written in a multiplatform language, *hbro* will only run under a linux environment.
    
   The simplest way is using the haskell packaging system::
    
     cabal install hbro
    
   Alternatively, you can download the hbro package from hackage, and install it with cabal-install.


.. Where to get the source ?
   -------------------------
    
   The latest source is hosted:
    
   * on github: ``git@github.com:k0ral/hbro.git``
   * on a personal server, which is unfortunately shutdown every european night: ``git://twyk.org/haskell-browser.git``
    
   You can still retrieve the source from hackage at any time, however the very last commits may not be included.


Configuration
-------------

By default, a pretty limited configuration file (see ``Hbro/Main.hs``) is used to build *hbro*. You can create your own at ``~/.config/hbro/hbro.hs`` to override it. Several extensions are provided with the * hbro-contrib_ * package, including a featured and self-explanatory example of configuration file.


Known bugs and limitations
--------------------------

Patches or suggestions are welcome to deal with the following issues. See package description for contact address.

Flash videos make hbro freeze
  The demo webkit browser for haskell's binding has the same problem, so it doesn't seem to come from hbro itself.

Javascript's window.open requests open in the same window instead of spawning a new one.
  This is due to this webkit's bug.

When toggling to source mode, current webpage is reloaded
  This is an undesired behavior since the webpage may have changed after reloading; webkit's API allows to get the content of the DOM but only inside the body tag; it is also possible to store the HTML source as it is downloaded, but then any further change in the DOM (for example triggered by javascript functions) wouldn't be visible.

No cookies management available
  The Haskell binding is missing some necessary functions that make it impossible to act on cookies management.

Configuring a proxy is impossible
  This feature would make use of to the webkit_get_default_session_ function. Unfortunately, Webkit's Haskell binding doesn't provide such function for now.


.. _hackage: http://hackage.haskell.org/package/hbro
.. _suckless manifest: http://suckless.org/manifest/
.. _Do-What-The-Fuck-You-Want-To public licence: http://en.wikipedia.org/wiki/WTFPL
.. _Haskell: http://haskell.org/
.. _WebKit: http://www.webkit.org/
.. _GTK+: http://www.gtk.org/
.. _ZeroMQ: http://www.zeromq.org/
.. _Dyre: https://github.com/willdonnelly/dyre
.. _webkit_get_default_session: http://webkitgtk.org/reference/webkitgtk/stable/webkitgtk-Global-functions.html
.. _hbro-contrib: http://hackage.haskell.org/package/hbro-contrib
