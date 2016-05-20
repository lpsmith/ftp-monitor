import sqlite3
import base64
import sys

conn = sqlite3.connect(sys.argv[1])

c = conn.cursor()

columns = c.execute('PRAGMA table_info(file_metadata)').fetchall()

target = [
    (0, u'id', u'INTEGER', 0, None, 1),
    (1, u'filename', u'TEXT', 0, None, 0),
    (2, u'filehash', u'TEXT', 0, None, 0),
    (3, u'hostname', u'TEXT', 0, None, 0),
    (4, u'port', u'INT', 0, None, 0),
    (5, u'username', u'TEXT', 0, None, 0),
    (6, u'rdns_hostname', u'TEXT', 0, None, 0),
    (7, u'ftp_size', u'INT', 0, None, 0),
    (8, u'ftp_mtime', u'TEXT', 0, None, 0),
    (9, u'fetch_start_time', u'INT', 0, None, 0),
    (10, u'fetch_finish_time', u'INT', 0, None, 0)
  ]

if columns != target:
    print "schema not what was expected; backing out"
    sys.exit(0)

c.execute("""
        CREATE TABLE file_metadata_new (
            id                INTEGER PRIMARY KEY,
            filename          TEXT,
            filehash          BLOB,
            hostname          TEXT,
            port              INT,
            username          TEXT,
            rdns_hostname     TEXT,
            ftp_size          INT,
            ftp_mtime         TEXT,
            fetch_start_time  INT,
            fetch_finish_time INT
          )
""")

for row in c.execute("SELECT * FROM file_metadata").fetchall():
    newrow = (
        row[0],
        row[1],
        sqlite3.Binary(base64.b16decode(row[2][:],True)),
        row[3],
        row[4],
        row[5],
        row[6],
        row[7],
        row[8],
        row[9],
        row[10]
      )
    c.execute("INSERT INTO file_metadata_new VALUES (?,?,?,?,?,?,?,?,?,?,?)", newrow)

c.execute("DROP TABLE file_metadata")
c.execute("ALTER TABLE file_metadata_new RENAME TO file_metadata")
