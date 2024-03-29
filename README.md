# TimeLoop

This is a demonstrator for studying time travel in a simple setting.
The application will generate and display all the possible trajectories in a universe, in presence of time travel.
See this [series of blog posts](https://www.corentindupont.info/blog/posts/Cosmology/2022-04-04-TimeTravel2.html) for more explainations.

Install
=======

[Download](https://github.com/cdupont/Timeloop/releases/tag/v1.0) one of the binary files from the release, unzip and run it.

Alternatively, you can clone this repo. Then [Install  Haskell](https://www.haskell.org/ghcup/) and run:
```
cabal install
```
Launch the demo:
```
timetravel
```


How to play
===========

You start by setting up the universe, and then run the simulation.
In the picture bellow, there is an "emitter" arrow on the left and two time portals: an entry portal, and an exit portal.    
The emitter will emit a "walker", here at time step 0.
The walkers always walk straight, except when they collides with another walker. 
When a walker collide with another walker, they always turn right (as a rule).
If the walker enters the entry portal at the right time, it will walk out of the exit portal at the mentioned time.
   
![start](img/start.png)

You can move the various elements of the universe (emitters and portals), setup their activation time, and add more of them (see the instructions).   
**When you are ready, hit Enter.**

![whole](img/whole.gif)

In this universe setup, there are **2 possible trajectories** for the walker:   
The first solution is simple: he just goes straight. At step 6, he will walk over the exit portal (this has no effects).

![sol1](img/sol1.gif)

However, there is another solution!   
At the start of the simulation (highlighted as step 0), another walker appears in the exit portal: it's you from the future! Both goes straight some steps, meeting in the middle. Then, as per the rule on collisions, both turn right. The initial walker enters the portal at step 6, thus closing the loop. The second walker continues toward the top.

![sol2](img/sol2.gif)

You can play around with different universe setups, and generate interresting configurations such as paradoxes, "Djinns" and more.
You can load examples using the number keys.   
Have fun!
