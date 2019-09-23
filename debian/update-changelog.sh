#!/bin/bash

HARBINGER_VERSION=$1

# Save changelog history
mv debian/changelog debian/changelog_history

# Generate new changelog
dch --create -U -v "$HARBINGER_VERSION" -M --package harbinger "$HARBINGER_VERSION Release"

# Append changelog history to new entry
cat <(echo) debian/changelog >> debian/changelog_history

# Remove old changelog file
rm debian/changelog_history
