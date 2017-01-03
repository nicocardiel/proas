Installing the package
======================

Requirements
------------

Before you install **proas**, make sure that ``PGPLOT`` is already installed in your system. To check this, you can
just type

::
    
    $ echo $PGPLOT_DIR
    
The answer must be something like `/usr/local/pgplot/`

If this is not the case (``PGPLOT`` is not installed), you can download `PGPLOT <http://astro.caltech.edu/~tjp/pgplot/>`_

Installation
-------------

To install **proas** on your hard disk, you must complete the following steps:

1.- Download the latest distribution from github:

::

    $ git clone https://github.com/nicocardiel/proas.git
    
    
2.- Move to the `proas` directory

::

    $ cd proas
    
3.- Execute the following commands:

::

    $ autoreconf -s -i -f
    $ ./configure
    $ make
    
.. note:: Mac users can easily indicate a different Fortran compiler using
      ``./configure F77=gfortran-mp-5``.

4.- You must finish the installation procedure by placing the
executable in its corresponding directory (you may need root privileges):

::

   $ sudo make install

