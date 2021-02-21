while line('.') < line('$')
  call cursor( line('.')+1, 1)
  if stridx(getline('.'), 'hola') > -1
    execute "s/hola/chau/g"
    execute "normal! A;\<ESC>"
  endif
endwhile
