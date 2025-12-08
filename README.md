# Bouncing balls with Gloss
Also known as glossyBouncyBalls.

### Gameplay
1. Watch the balls bounce
2. Create new balls with your mouse
3. Go to step 1

### How to Run
Clone the Git repository or download the ZIP file as you prefer.  
After that, extract the ZIP or change into the project directory as applicable.

Make sure all of the following external dependencies are installed:
- **libGL**: A C graphics library for interfacing with OpenGL (required by Gloss)
- **libGLU**: Another C graphics library for interfacing with OpenGL (required by Gloss)
- **GLUT / freeGLUT**: The OpenGL Utility Toolkit - a collection of tools and interfaces for OpenGL development (required by Gloss)
- **Cabal**: A package manager for Haskell applications and libraries, used to build the project and its Haskell-based dependencies

With all external dependencies installed, run `cabal run` in the project directory. You should see a black window with three large, colorful bouncing balls. Enjoy!

Alternatively flake.nix and flake.lock files are provided with the project files so the demo can easily be installed using the nix package manager
