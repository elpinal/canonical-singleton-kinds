mlkit:
	mlkit -o canonical-singleton-kinds-mlkit canonical-singleton-kinds.mlb

mlton:
	mlton -output canonical-singleton-kinds-mlton -default-ann 'warnUnused true' canonical-singleton-kinds.mlb

.PHONY: mlkit mlton
