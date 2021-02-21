let a = 1 + 2
let b = 2 + 3 * a
let c = 1 + a
let d = 5 + line('.')
let d = 1005 + line('$')
let x = 'hola '
let y = ' chau'
let u = x . y
let z = 'Lunes y ' . 'Martes'
let w = 'hola ' . 'que ' . 'tal!'
let q = 'hola ' . 'que ' . 'tal!'
while line('.') < line('$')
call cursor( line('.') + 1, 1)
if stridx(getline('.'), 'hola') >  -1
echom 'si'
execute "s/hola/CHAU/g"
else
echom 'no'
endif
endwhile
call cursor( line('.') - 1, 1)
call cursor( 1, 1)
while line('.') < line('$')
call cursor( line('.') + 1, 1)
if stridx(getline('.'), 'coma') >  -1
execute "s/coma/COMA/g"
else

endif
endwhile
echom q
echom 'termina OK'