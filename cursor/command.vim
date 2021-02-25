call cursor( 1, 1)
while line('.') < line('$')
if stridx(getline('.'), 'hola') >  -1
echom 'si1'
execute "s/hola/HOLA/g"
execute "normal! A___cambió_HOLA___\<ESC>"
else
echom 'no1'
endif
call cursor( line('.') + 1, 1)
endwhile
call cursor( 1, 1)
let a = 10
while line('.') < 10
if stridx(getline('.'), 'coma') >  -1
execute "s/coma/COMA/g"
echom 'si2'
execute "normal! A___cambió_COMA___\<ESC>"
else
echom 'no2'

endif
call cursor( line('.') + 1, 1)
endwhile
call cursor( line('$'), 1)
let b = 9
let c = 1 + b
while line('.') > c
if stridx(getline('.'), 'auto') >  -1
execute "s/auto/AUTO/g"
echom 'si3'
execute "normal! A___cambió_AUTO___\<ESC>"
else
echom 'no3'
endif
call cursor( line('.') - 1, 1)
endwhile
call cursor(15, 1)
while line('.') < line('$')
if stridx(getline('.'), 'lunes') >  -1
echom 'si4'
execute "s/lunes/LUNES/g"
execute "normal! A___cambió_LUNES___\<ESC>"
else
echom 'no4'
endif
call cursor( line('.') + 1, 1)
endwhile
call cursor(25, 1)
execute "normal! dd"
let x = 'hola'
let y = ' que tal!'
let u = x . y
let w = 'hola ' . 'que ' . 'tal!'
let q = 'hola ' . 'que ' . 'tal!'
echom u
echom w
echom q
echom 'termina OK'