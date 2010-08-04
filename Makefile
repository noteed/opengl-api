dist/setup-config: opengl-api.cabal
	cabal configure

dist/build/opengl-api/opengl-api: Text/OpenGL/Spec.hs \
		Text/OpenGL/Api.hs \
		Text/OpenGL/ExtHeader.hs \
		dist/setup-config
	cabal build

the-glext.h: dist/build/opengl-api/opengl-api \
		spec-files/opengl/gl.tm \
		spec-files/opengl/enumext-2010-06-15.spec \
		spec-files/opengl/gl-2010-07-22.spec
	dist/build/opengl-api/opengl-api \
		-tspec-files/opengl/gl.tm \
		-espec-files/opengl/enumext-2010-06-15.spec \
		-fspec-files/opengl/gl-2010-07-22.spec > the-glext.h

the-diff.txt: the-glext.h results/glext-2010-06-15-version-63.h
	-diff results/glext-2010-06-15-version-63.h the-glext.h > the-diff.txt

sanity-check: results/expected-diff-2010-06-15-version-63.txt the-diff.txt
	diff results/expected-diff-2010-06-15-version-63.txt the-diff.txt
