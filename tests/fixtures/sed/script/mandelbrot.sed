#!/bin/sed -Enf
#
# Draw the Mandelbrot set.
# Obtained from https://github.com/laserbat/mandelbrot.sed
#
# Public domain (http://unlicense.org/)
#
# usage: echo | ./mandelbrot.sed

s/.*/aaaa;/
s/a/aaaaa/g
x

:s
    g
    x

    /;a{21}/q
    /^;/s/;/&a/
    s/^a//
    x

    s/;//
    s/a*/,,-tt,&,;/
    s/t/tttt/g
    s/t/aaaaa/g

    :g
        s/(,,a*)([^;]*;)$/&\1a\2/
    /,a{55}-/!bg

    :l
        s/[^x]*x{30,};/ ;/g
        s/(-?)(a*),(-?)(a*),((-?a*),(a*),x*)/\2:x\2-\4:x\4+\6,\7\1\3+\2\2:x\4,\5x/g
        s/[^;]*:x[-a]{55,}:x[^;]*,(x*);/\1;/g
        s/--|\+?(-)\+?/\1/g

        :m
            s/(a*):(a*y*)xa/\1:\1\2x/g
            s/a{18}(y*x)/y\1/g
        tm

        s/a{9}(y*x)/y\1/g

        s/a*:a*(y*)x/\1/g
        y/y/a/

        s/-(a*)-(a*)/-\1\2/g
        s/(a*)-(a*)\+(a*)/\1\3-\2/g
        s/\+//g

        :u
            s/a-a/-/g
        tu

        s/-,/,/g

    /a/bl

    s/ ?x*/<&>/g
    s/(x{5})*//g
    s/xxxx/,/g
    s/xxx/-/g
    s/xx/o/g
    s/x/+/g
    s/<>/./g
    s/[<>;]//g
    s/./&&/gp
bs
