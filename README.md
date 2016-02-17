GOG
===
An experimental language with polynotational semantics
-----

# Installation

1. Install Rlwrap and Racket
2. Install optional MySQL client
3. Clone GOG
4. Add GOG launcher to your path

Assuming you have Ubuntu, this can be done with the following lines

    sudo apt-get install racket rlwrap
    sudo apt-get install mysql-client libssl-dev # Optional MySQL support
    sudo git clone https://github.com/nosovicki/gog.git /opt/gog
    sudo ln -s /opt/gog/gog /usr/bin/

Now you can run GOG interactively and / or pass programs to it:

    gog
    gog examples/life.gog


## Introduction

See [here] (https://nosovicki.azurewebsites.net/gog/)
