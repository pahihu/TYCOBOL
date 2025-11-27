export CURRENCY="€"; cob2run ./CURRENCY-ENV
export CURRENCY="£"; cob2run ./CURRENCY-ENV
export CURRENCY="$"; cob2run ./CURRENCY-ENV
export CURRENCY="¥"; cob2run ./CURRENCY-ENV
unset CURRENCY; cob2run ./CURRENCY-ENV

# sudo cp /usr/lib/locale/somelocale /usr/lib/locale/mylocale
# sudo localedef -i mylocale /usr/lib/locale/mylocale
# COB_LOCALE_CURRENCY=yes LC_MONETARY=mylocale cobcrun yourprogram
