CREATE TABLE Users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    preferences TEXT
);

CREATE TABLE Eateries (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL UNIQUE,
    location TEXT
);

CREATE TABLE Reviews (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER,
    eatery_id INTEGER,
    dish TEXT,
    rating INTEGER,
    comment TEXT,
    FOREIGN KEY (user_id) REFERENCES Users(id),
    FOREIGN KEY (eatery_id) REFERENCES Eateries(id)
);
