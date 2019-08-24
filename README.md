# haskell-twitch-bot
A simple educational twitch chat bot written in Haskell. 

Modified from https://wiki.haskell.org/Roll_your_own_IRC_bot.

Create your bot by following these steps: 
1. Configure authentication by following the steps in ```src/config/Secret.hs.template```
2. Copy, modify, and rename ```src/botsTemplateBot.hs``` according to your bot requirements. 
3. Rename the import on line _8_ of ```src/Lib.hs``` to the name of your bot module. 

After you follow these steps, build your bot:
```bash
stack build
```

Then run it:

```bash
stack run
```

Alternatively, you may run your bot interactively:

```bash
stack ghci
> Lib.main
```