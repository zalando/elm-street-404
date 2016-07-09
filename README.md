# 404 Elm Street

Work in progress, current demo can be [seen here](http://zalando.github.io/elm-street-404/).

Read more about the game on [our blog](https://tech.zalando.com/blog/using-elm-to-create-a-fun-game-in-just-five-days/).

![Screencast](screen.gif)

You are Joe, the courier. It is your job, to deliver *all* the fashion to *all* the customers. Sometimes, you have to pick up stuff and return it to Zalando.

No problem.

However, customers keep ordering more and more — and your bicycle only has room for so many boxes...

To succeed, you'll have to plan your route carefully. You have to decide which packages to deliver in which order. And you really, really have to hurry. You'll find, that it's not as easy being a Zalandoer as you thought it was:

Unhappy customers: **you lose**.

## Instructions to run

1. Install elm [elm-lang.org/install](http://elm-lang.org/install)
2. Clone this repo and `cd` into it
3. Start `elm reactor`
4. Open `http://localhost:8000/src/Main.elm` in the browser to see the game

## Embedded mode

1. Run `elm make src/Main.elm --output elm.js`
2. Start `elm reactor`
3. Open `http://localhost:8000/index.html`
4. Alternatively there is `http://localhost:8000/embed.html` that demonstrates how the game may be toggled by a button

## Gotchas

In order to be able to correctly suspend/restore the game for embeded mode, [this fix has to be applied to the compiled elm.js](https://github.com/elm-lang/core/issues/628#issuecomment-225719492).

```diff
- numSteps = step(numSteps, process);
+ if (process.root) { numSteps = step(numSteps, process); } 
```

