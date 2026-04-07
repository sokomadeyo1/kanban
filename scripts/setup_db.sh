#!/bin/bash
dir=$(pwd | sed 's/.*\///')
if [[ $dir = "scripts" ]]; then
	exit 1
fi

bd_file="data/kanban.db"
mkdir data
touch $bd_file
for migration in migrations/*; do
	sqlite3 $bd_file <$migration
done
