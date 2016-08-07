all: Domingo.js

ELMS=Domingo.elm \
DomingoCardPrimitives.elm \
DomingoCards.elm \
DomingoConf.elm \
DomingoLib.elm \
DomingoModel.elm \
DomingoPorts.elm \
DomingoView.elm

Domingo.js: $(ELMS)
	elm make Domingo.elm --output Domingo.js
