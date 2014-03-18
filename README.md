Hermite is a lightweight system terminal for people that want full control
of their terminal editor without writing their own. It follows the spirit 
xmonad but use dyre to support dynamic reconfiguration. Everything can be
changed, from default colors, how urls are handled, to creating popups on
events. Hermite uses gnomes VTE internaly, like many other terminal, 
because of it's good bindings and standard support.

This is the real main module and should not be modified.
For costumization Hermite (and dyre) edit the config file
(~/.config/hermite/hermite.hs). Typically this means just setting colors
and edit bindings and adding 1 or 2 widgets, but you could change all 
of the behaviour of hermite if wanted. All you really are forced to do is
to spawn a vte and a window containing that vte.

The project is under heavy development atm and your config will break.

TODO
====

An incomplete list of things that would be cool to have:

 * a widget for supplying input (search, hints etc), like termite
 * easier bind system, to many levels etc, more suger?
 * improve startup, if you configure the code in one way a lot of static code gets run on each startup
 * tabs
 * some cool features that makes it a bit more uniq

