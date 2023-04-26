REPORT ZPG_017M_FICO.
Data : g_counter(1) type n value 0,
g_field_init( 11) type c value 'P9002-FAMSA' ,
g_field_final( 12) type c.

LOOP AT GT_PA0021 INTO GS_PA0021
g_counter = g_counter + 1.

concatenate g_field_init
g_counter
into
g_field_final.
MOVE GA_PA0021-SUBTY TO g_field_final.

if u want to exit from loop once the values reaches 10 then

if g_counter = 10
exit.
endif.
ENDLOOP.
