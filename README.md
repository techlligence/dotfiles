# What makes this config special?
My bspwm is finally setup. It takes my xmonad config and emulates it. This is done by using the swap function in bspc and as well with some bash to sort the desktops.

BSPWM:

I use lemonbar and the cool thing, is that, each bar is setup dynamically irrespective of how many monitors are attached along with their respective resolutions. In addition to this, I setup lemonbar in such a way, that each bar is independent of the monitor it is on. In other words, when I am on workspace 1, workspace 1 is only highlighted on the monitor that is focused. 

XMONAD:

I have two xmonad config files:
One that has xmonad like monitor swapping, it has the following features:
- Dynamic bar setup for multiiple monitors
- Ezconfig implementation
- Correct fullscreen implementation (for hiding xmobar)

The other config simulates the multihead behaviour of dwm. I prefer the above config.
