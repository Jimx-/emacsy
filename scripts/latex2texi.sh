sed -ri \
  -e 's,\\chapter\{([^}]*)\},@chapter \1,' \
  -e 's,\\section\{([^}]*)\},@section \1,'\
  -e 's,\\subsection\{([^}]*)\},@subsection \1,'\
  -e 's,\\verb\{([^}]*)\},@verb{\1},'\
  -e 's,\\emph\{([^}]*)\},@emph{\1},'\
  -e 's,\\begin\{([^}]*)\},@\1,'\
  -e 's,\\end\{([^}]*)\},@end  \1,'\
  -e 's,\\item (.*),@item \1,'\
  emacsy/*.c emacsy/*.h emacsy/*.scm test/*.scm
