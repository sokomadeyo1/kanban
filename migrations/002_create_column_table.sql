BEGIN;

CREATE TABLE Column(
	columnID INTEGER PRIMARY KEY,
	columnTitle TEXT NOT NULL
);

-- default column
INSERT INTO Column (columnID, columnTitle) VALUES (1, 'Backlog');

ALTER TABLE Entry
	ADD COLUMN entryColumn INTEGER REFERENCES Column(columnID);

UPDATE Entry SET entryColumn=1;

ALTER TABLE Entry ALTER COLUMN entryColumn SET NOT NULL;

COMMIT;
