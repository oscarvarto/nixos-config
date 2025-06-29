#!/usr/bin/env bash

cs bootstrap \
	--java-opt -XX:+UseG1GC \
	--java-opt -XX:+UseStringDeduplication \
	--java-opt -Xss4m \
	--java-opt -Xms100m \
	--java-opt -Xmx30G \
	--java-opt -Dmetals.client=emacs \
	-r sonatype:snapshots -r sonatype:releases \
	org.scalameta:metals_2.13:$1 -o ~/.local/bin/metals -f
