guiles
======
guiles, an IRC bot.
Written in Guile, this bot is a work in progress.

To run it, run ./main.scm
Configuration is done using config.scm
Pass the *-d file* flag to ./main.scm to create a domain socket, *file*, which shall act as a REPL.


Current Features
----------------
• Ability to join server, and allow access via REPL to reprogram & interact with the bot while running.
This allows access to the bot's modules, and channels can be joined/left, modules activated, nick changed etc.

• A near-working module system. Run `(register-module '(mymodule))` to cause a module registration event for that module. This is used to add hooks.

