tar -C _site -cvz . > docs.tar.gz
curl -v --oauth2-bearer "$(cat SRHT_TOKEN)" \
  -Fcontent=@docs.tar.gz \
  https://pages.sr.ht/publish/achille.acatalepsie.fr
