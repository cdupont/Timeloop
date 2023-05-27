# timetravel

This is a demonstrator for studying time travel in a simple setting.
The application will generate and display all the possible trajectories in a universe, in presence of time travel.
See this [series of blog posts](https://www.corentindupont.info/blog/posts/Cosmology/2022-04-04-TimeTravel2.html) for more explainations.

Install
=======

run:
```
cabal install
```
Launch:
```
timetravel
```

How to play
===========

You start by setting up the universe.
In the picture bellow, there is an "emitter" on the left and two time portals: an entry portal, and an exit portal.
The emitter will emit a "walker" that always walk straight, except when it collides with another walker. 
When a walker collide with another walker, they always turn right (as a rule).
If the walker enters the entry portal at the right time, it will walk out the of the exit portal at the mentioned time.

![start](img/start.png)

You can move the various elements of the universe (emitter and portals), and add more of them.
When you are ready, hit Enter.

![whole](img/whole.gif)

In this scenario, there are 2 possible trajectories for the character:
1. He just goes straight and passes between the two portals. No problem.

![sol1](img/sol1.gif)

2. At the start of the simulation, another character appears in the exit portal: it's you from the future! Both goes straight one step, meeting in the middle. Then, as per the rule on collisions, both turn right. The initial character enters the portal at step 2, thus closing the loop. The second character continues toward the left.

![sol2](img/sol2.gif)


