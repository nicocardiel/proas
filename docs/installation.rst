Installing the package
======================

Requirements
------------

Before you install **proas**, make sure that ``PGPLOT`` is already installed in your system. To check this, you can
just type

::
    
    $ echo $PGPLOT_DIR
    
The answer must be something like `/usr/local/pgplot/`

If this is not the case (``PGPLOT`` is not installed), you can download `PGPLOT<http://astro.caltech.edu/~tjp/pgplot/>`

Installation
-------------

To install **proas** on your hard disk, you must complete the following steps:

1. Download the latest distribution from github:

::

    $ git clone https://github.com/nicocardiel/proas.git
    
    
2. Move to the `proas` directory

::

    $ cd proas
    
3. Execute the script file `create_proas`, supplying 3 arguments

::

    $ create_proas f77 cc /usr/lib
    
The first and second arguments are the fortran and C compilers respectively. The third argument is the
directory where the library `libX11.a` can be found.
