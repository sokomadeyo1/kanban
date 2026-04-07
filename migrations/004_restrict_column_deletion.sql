BEGIN;

CREATE TABLE Entry_restrict(
	entryID INTEGER PRIMARY KEY,
	entryTitle TEXT NOT NULL,
	entryDesc TEXT,
	entryColumn INTEGER NOT NULL,
	FOREIGN KEY (entryColumn) REFERENCES Column(ColumnID) ON DELETE NO ACTION
);

INSERT INTO Entry_restrict SELECT * FROM Entry;
DROP TABLE Entry;
ALTER TABLE Entry_restrict RENAME TO Entry;

COMMIT;
