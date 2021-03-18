# twitchreplbot
A small and 'hopefully' save Lisp REPL for Twitch. Powered by cl-isolated.

If you want to use the bot by yourself, be aware that you have to trust [cl-isolated](https://github.com/kanru/cl-isolated) that it is an actual save environment. 

*NEW*: The bot is now able to evaluate a command in an isolated docker container all by itself.

If you want to be super secure just put the bot in a docker and run the docker from a virtual machine. If someone can break out of this they probably deserve the loot. Kappa (just kidding)

## Usage

Just `(ql:quickload :lispbot)` and it runs automatically. You just have to change the vars in the code. `*target_channel*` should be the channel you want the bot to connect. The call to `connect-join-channel` should take the name of your bot account as first argument. You can use your main twitch account for it aswell if you want, but I never tried it.

Put the `init.conf` in the `~/.config/lispbot/` directory or replace the `*path-to-conf*` variable with your desired path.

The first line in the init file should be your oauth token, the second one is the websocket you want to connect to (currently only unsecure websocket is supported, but who cares in this case?) the last one is the initial message the bot barfs into the chat to welcome everyone.

*DO NOT DOX YOUR OAUTH TOKEN*

## Compilation

You could now potentially just compile the bot with `sbcl --eval "(asdf:operate :build-op :lispbot)"` but for some reason the bot will not stay connected to the Twitch IRC. I have no idea why this is and I also just run the bot in a SBCL REPL, so I actually do not care enough to investigate.

Have fun!


