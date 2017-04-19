$kanji  = "-kanji=$ENV{\"LATEXENC\"}" if defined $ENV{"LATEXENC"};
$latex  = "platex -interaction=nonstopmode $kanji";
$bibtex = 'pbibtex $kanji';
$dvipdf = 'perl -e "exec(\'dvipdfmx\', \$ARGV[0])"';
$pdf_mode = 3;
